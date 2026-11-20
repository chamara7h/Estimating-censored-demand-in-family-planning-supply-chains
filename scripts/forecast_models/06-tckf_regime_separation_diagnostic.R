# POM Revision - regime-separation diagnostic

library(tidyverse)
library(forecast)
library(tsibble)
library(doParallel)
library(foreach)

source("scripts/forecast_models/05-helpers.R")

set.seed(100)

# load data ---------------------------------------------------------------

s_data <- read_rds(
  "data/tidy_data/synthetic_regime_separation_data.rds"
) |>
  mutate(
    censorship_type = as.character(censorship_type)
  ) |>
  as_tsibble(
    key = c(scenario, series_category, series_type, store, product),
    index = date
  )

s_data_diag <- s_data |>
  mutate(
    id = paste(
      scenario, series_category, series_type, store, product,
      sep = "-"
    )
  ) |>
  as_tsibble(key = id, index = date)

id_list <- s_data_diag |>
  distinct(id) |>
  pull(id)


# fit common TCKF parameters ---------------------------------------------

fit_regime_parameters <- function(data, spec, stl_out, train_n = 30) {
  init <- spec$init_from_stl(stl_out)
  X0 <- init$X0
  P0 <- init$P0
  d <- spec$dim

  P0_diag <- diag(P0)
  P0_diag[!is.finite(P0_diag) | P0_diag <= 0] <- 1
  P0 <- diag(P0_diag, d, d)

  if (!"k_t" %in% names(data)) {
    data <- data |>
      mutate(k_t = build_k_t(censorship_type))
  }

  neg_ll <- function(par) {
    Q0_diag <- exp(par[1:d])
    R <- exp(par[d + 1])
    phi <- plogis(par[d + 2])
    lambda <- exp(par[d + 3])

    F <- spec$F(phi)
    H <- spec$H
    Q0 <- diag(Q0_diag, d, d)

    X_upd <- X0
    P_upd <- P0
    nll <- 0

    for (t in (train_n + 1):nrow(data)) {
      Qt <- Q0 + lambda * data$k_t[t] * diag(d)
      Xp <- F %*% X_upd
      Pp <- F %*% P_upd %*% t(F) + Qt

      mu_t <- as.numeric(H %*% Xp)
      S_t <- max(as.numeric(H %*% Pp %*% t(H) + R), 1e-8)
      sigma_t <- sqrt(S_t)

      if (data$censorship_type[t] == "none") {
        innovation <- data$observed_demand[t] - mu_t
        nll <- nll + 0.5 * (log(S_t) + innovation^2 / S_t)

        K <- Pp %*% t(H) / S_t
        X_upd <- Xp + as.numeric(K) * innovation
        P_upd <- (diag(d) - K %*% H) %*% Pp
      } else if (data$censorship_type[t] == "censored") {
        beta <- (data$observed_demand[t] - mu_t) / sigma_t
        beta <- clamp(beta, -7, 7)
        nll <- nll - pnorm(beta, lower.tail = FALSE, log.p = TRUE)

        expected_demand <- mu_t +
          sigma_t * dnorm(beta) / (pnorm(beta, lower.tail = FALSE) + 1e-12)
        K <- Pp %*% t(H) / S_t
        X_upd <- Xp + as.numeric(K) * (expected_demand - mu_t)
        P_upd <- (diag(d) - K %*% H) %*% Pp
      } else {
        # An interruption is uninformative about latent demand.
        X_upd <- Xp
        P_upd <- Pp
      }
    }

    nll
  }

  uncensored_var <- var(
    data$observed_demand[data$censorship_type == "none"],
    na.rm = TRUE
  )

  if (!is.finite(uncensored_var) || uncensored_var <= 0) {
    uncensored_var <- 1
  }

  par0 <- c(
    log(rep(0.1, d)),
    log(uncensored_var),
    qlogis(0.9),
    log(0.5)
  )

  opt <- optim(
    par0,
    neg_ll,
    method = "L-BFGS-B",
    lower = c(rep(-10, d), log(1e-6), qlogis(1e-3), log(1e-6)),
    upper = c(rep(10, d), log(1e6), qlogis(1 - 1e-3), log(10))
  )

  ph <- opt$par

  list(
    Q0 = diag(exp(ph[1:d]), d, d),
    R = exp(ph[d + 1]),
    phi = plogis(ph[d + 2]),
    lambda_hat = exp(ph[d + 3]),
    X0 = X0,
    P0 = P0,
    convergence = opt$convergence
  )
}


# run separated and pooled update rules ----------------------------------

run_regime_filter <- function(data, spec, fit, variant, train_n = 30,
                              conformal_window = 24, alpha = 0.10) {
  d <- spec$dim
  lambda <- clamp(fit$lambda_hat, 0, 1)

  F <- spec$F(fit$phi)
  H <- spec$H

  X_upd <- fit$X0
  P_upd <- fit$P0

  n <- nrow(data)
  yhat <- rep(NA_real_, n)
  lower_q90 <- rep(NA_real_, n)
  upper_q90 <- rep(NA_real_, n)
  residual_pool <- numeric(0)

  for (t in (train_n + 1):n) {
    Qt <- fit$Q0 + lambda * data$k_t[t] * diag(d)
    Xp <- F %*% X_upd
    Pp <- F %*% P_upd %*% t(F) + Qt

    mu_t <- as.numeric(H %*% Xp)
    S_t <- max(as.numeric(H %*% Pp %*% t(H) + fit$R), 1e-8)
    sigma_t <- sqrt(S_t)
    yhat[t] <- pmax(0, mu_t)

    # Construct the interval before adding the current forecast error.
    if (length(residual_pool) >= 10) {
      residual_window <- tail(residual_pool, conformal_window)
      q_alpha <- as.numeric(
        quantile(residual_window, probs = 1 - alpha, na.rm = TRUE, type = 8)
      )
      lower_q90[t] <- pmax(0, yhat[t] - q_alpha)
      upper_q90[t] <- yhat[t] + q_alpha
    }

    if (data$censorship_type[t] == "none") {
      innovation <- data$observed_demand[t] - mu_t
      K <- Pp %*% t(H) / S_t
      X_upd <- Xp + as.numeric(K) * innovation
      P_upd <- (diag(d) - K %*% H) %*% Pp

      residual_pool <- c(
        residual_pool,
        abs(data$actual_demand[t] - yhat[t])
      )
    } else if (
      data$censorship_type[t] == "censored" ||
        variant == "TCKF-Pooled"
    ) {
      # The pooled variant applies the stockout update to interruptions too.
      beta <- (data$observed_demand[t] - mu_t) / sigma_t
      beta <- clamp(beta, -7, 7)
      expected_demand <- mu_t +
        sigma_t * dnorm(beta) / (pnorm(beta, lower.tail = FALSE) + 1e-12)

      K <- Pp %*% t(H) / S_t
      X_upd <- Xp + as.numeric(K) * (expected_demand - mu_t)
      P_upd <- (diag(d) - K %*% H) %*% Pp
    } else {
      # The separated variant skips the update during interruptions.
      X_upd <- Xp
      P_upd <- Pp
    }
  }

  data |>
    as_tibble() |>
    mutate(
      variant = variant,
      yhat = yhat,
      lower_q90 = lower_q90,
      upper_q90 = upper_q90,
      previous_regime = lag(censorship_type),
      train_n = train_n,
      conformal_window = conformal_window,
      lambda = lambda,
      phi = fit$phi,
      R = fit$R,
      fit_convergence = fit$convergence
    )
}


# run diagnostic ----------------------------------------------------------

train_n <- 30
conformal_window <- 24
spec <- make_spec("full")

cl <- makeCluster(max(1, parallel::detectCores(logical = FALSE) - 1))
registerDoParallel(cl)

system.time(
  tckf_regime_diag <- foreach(
    i = id_list,
    .combine = "rbind",
    .packages = c(
      "doParallel", "foreach", "tidyverse", "forecast", "tsibble",
      "lubridate", "purrr"
    ),
    .errorhandling = "remove"
  ) %dopar% {
    data <- s_data_diag |>
      as_tibble() |>
      filter(id == i) |>
      arrange(date) |>
      mutate(k_t = build_k_t(censorship_type))

    if (nrow(data) <= train_n + 2) {
      return(NULL)
    }

    stl_out <- stl_init(data, train_n = train_n)
    fit <- fit_regime_parameters(data, spec, stl_out, train_n = train_n)

    bind_rows(
      run_regime_filter(
        data,
        spec,
        fit,
        variant = "TCKF-Separated",
        train_n = train_n,
        conformal_window = conformal_window
      ),
      run_regime_filter(
        data,
        spec,
        fit,
        variant = "TCKF-Pooled",
        train_n = train_n,
        conformal_window = conformal_window
      )
    )
  }
)

stopCluster(cl)

write_rds(
  tckf_regime_diag |>
    filter(!is.na(yhat)),
  "data/predictions/tckf_regime_separation_diag.rds"
)
