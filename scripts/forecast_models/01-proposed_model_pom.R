# POM Revision

# Proposed model

library(tidyverse)
library(forecast)
library(tsibble)
library(doParallel)
library(foreach)

# Load data ---------------------------------------------------------------

s_data <- read_rds("data/tidy_data/synthetic_data.rds") |> 
  mutate(censorship_type = as.factor(censorship_type),
         censorship_type = if_else(observed_demand == actual_demand, 'none', censorship_type)) |> 
  as_tsibble(
    key   = c(series_category, series_type, store, product),
    index = date
  )

# creating training set

train_len <- 70
test_len <- 30

s_data_train <- s_data |>
  filter_index(. ~ as.character(min(s_data$date)+(train_len - 1))) 

# create tscv train data

f_horizon = 1

s_data_train_tscv <- s_data |> 
  stretch_tsibble(.init = (length(unique(s_data_train$date)) - f_horizon + 1), .step = 1) |> 
  # rename(horizon = .id) |> 
  group_by(.id, store, product) |> 
  mutate(period = row_number(),
         id = paste0(series_category,'-',series_type,'-',store, product,'-',.id)) |> 
  ungroup()

# create tscv test data

s_data_test_tscv <- s_data |> 
  filter_index(as.character(max(s_data$date)-(test_len) + 1) ~ .) |>
  slide_tsibble(.size = 1, .step = 1) |> 
  # rename(horizon = .id) |> 
  group_by(.id, store, product) |> 
  mutate(period = row_number(),
         id = paste0(series_category,'-',series_type,'-',store, product,'-',.id)) |> 
  ungroup()

# id list

id_list <- s_data_train_tscv |> 
  filter(.id != 31) |> 
  pull(id) |> 
  unique()

# Register parallel backend

n_cores <- detectCores(logical = FALSE) - 1   # 16 physical cores – 1 = 15
cl_outer <- makeCluster(n_cores)
registerDoParallel(cl_outer)


system.time(
  tckf_pred <- foreach(
    i = id_list,
    .combine  = "rbind",
    .packages = c("doParallel", "foreach", "tidyverse", "forecast", "tsibble", "lubridate", "purrr")
  ) %dopar% {
    
    # i = id_list[13]
    # i = 	'uncorrelated-erratic-S39P4-1'
    
    # data for one series
    data <- s_data_train_tscv |>
      as_tibble() |>
      filter(id == i)
    
    test_data <- s_data_test_tscv |>
      as_tibble() |>
      filter(id == i)
    
    # init window (STL)
    train_n  <- 30
    stl_data <- data |> slice(1:train_n)
    

    # STL init: mask censored points, interpolate, decompose
    ts_train <- ts(
      stl_data$observed_demand,
      frequency = 12,
      start     = c(year(min(stl_data$date)), month(min(stl_data$date)))
    )
    
    ts_train[stl_data$censorship_type != "none"] <- NA
    
    ts_filled <- na.interp(ts_train)
    dim(ts_filled) <- NULL
    
    stl_out <- stl(ts_filled, s.window = "periodic", robust = TRUE)
    
    ell0   <- last(stl_out$time.series[, "trend"])
    tau0   <- mean(diff(stl_out$time.series[, "trend"]), na.rm = TRUE)
    gamma0 <- last(stl_out$time.series[, "seasonal"])
    
    P0_diag <- c(
      var(stl_out$time.series[, "trend"], na.rm = TRUE),
      var(diff(stl_out$time.series[, "trend"]), na.rm = TRUE),
      var(stl_out$time.series[, "seasonal"], na.rm = TRUE)
    )
    
    # guard against degenerate variances
    P0_diag[!is.finite(P0_diag) | P0_diag <= 0] <- 1
    
    # k_t: consecutive censorship spells
    data <- data |>
      mutate(
        k_t = purrr::accumulate(
          censorship_type != "none",
          ~ if_else(.y, .x + 1L, 0L),
          .init = 0L
        )[-1]
      )
    
    # shorthand vectors
    y_obs <- data$observed_demand
    ctype <- data$censorship_type
    k_t   <- data$k_t
    n     <- nrow(data)
    
    # stable log(1 - Phi(z))
    log1m_pnorm <- function(z) {
      pnorm(z, lower.tail = FALSE, log.p = TRUE)
    }
    

    # negative log-likelihood (censored = right tail; disruption = missing)
    neg_ll <- function(par) {
      
      Q0_diag <- exp(par[1:3])
      R       <- exp(par[4])
      phi     <- plogis(par[5])       # (0,1)
      lambda  <- plogis(par[6])       # (0,1)  
      
      F <- matrix(
        c(1, 1, 0,
          0, 1, 0,
          0, 0, phi),
        3, 3, byrow = TRUE
      )
      
      H  <- matrix(c(1, 0, 1), 1, 3)
      Q0 <- diag(Q0_diag)
      
      X_upd <- c(ell0, tau0, gamma0)
      P_upd <- diag(P0_diag)
      
      nll <- 0
      
      for (t in (train_n + 1):n) {
        
        Qt <- Q0 + lambda * k_t[t] * diag(3)
        
        # predict
        Xp <- F %*% X_upd
        Pp <- F %*% P_upd %*% t(F) + Qt
        
        mu <- as.numeric(H %*% Xp)
        S  <- as.numeric(H %*% Pp %*% t(H) + R)
        S  <- max(S, 1e-8)  # numerical guard
        sig <- sqrt(S)
        
        if (ctype[t] == "none") {
          
          e   <- y_obs[t] - mu
          nll <- nll + 0.5 * (log(S) + (e^2) / S)
          
          # KF update
          K     <- Pp %*% t(H) / S
          X_upd <- Xp + as.numeric(K) * e
          P_upd <- (diag(3) - K %*% H) %*% Pp
          
        } else if (ctype[t] == "censored") {
          
          # stockout: latent demand >= observed issuance (censoring point)
          z   <- (y_obs[t] - mu) / sig
          
          # RIGHT tail: P(Y >= c) = 1 - Phi(z)
          nll <- nll - log1m_pnorm(z)     
          
          # keep prediction-only propagation in likelihood stage
          X_upd <- Xp
          P_upd <- Pp
          
        } else if (ctype[t] == "disruption") {
          
          # full censoring: treat as missing -> no likelihood term
          X_upd <- Xp
          P_upd <- Pp
        }
      }
      
      nll
    }
    

    # parameter estimation (bounded)
    unc_var <- var(y_obs[ctype == "none"], na.rm = TRUE)
    if (!is.finite(unc_var) || unc_var <= 0) unc_var <- 1
    
    # params: log(Q0_diag1..3), log(R), qlogis(phi), qlogis(lambda)
    par0 <- c(log(rep(0.1, 3)), log(unc_var), qlogis(0.9), qlogis(0.5))
    
    opt <- optim(
      par0, neg_ll,
      method = "L-BFGS-B",
      lower  = c(rep(-10, 3), log(1e-6), qlogis(1e-3), qlogis(1e-3)),
      upper  = c(rep( 10, 3), log(1e+6), qlogis(1 - 1e-3), qlogis(1 - 1e-3))
    )
    
    ph <- opt$par
    
    Q0_fit <- diag(exp(ph[1:3]))
    R_fit  <- exp(ph[4])
    phi_f  <- plogis(ph[5])
    lam_f  <- plogis(ph[6])     
    

    # filtering with partial updates (censored = truncated mean; disruption = skip)
    data <- data |> mutate(in_sample = period <= train_n)
    
    F <- matrix(
      c(1, 1, 0,
        0, 1, 0,
        0, 0, phi_f),
      3, 3, byrow = TRUE
    )
    H <- matrix(c(1, 0, 1), 1, 3)
    
    X_upd <- c(ell0, tau0, gamma0)
    P_upd <- diag(P0_diag)
    
    yhat <- rep(NA_real_, n)
    state_matrix <- matrix(NA_real_, n, 3)
    
    for (t in (train_n + 1):n) {
      
      Qt <- Q0_fit + lam_f * k_t[t] * diag(3)
      
      # predict
      Xp <- F %*% X_upd
      Pp <- F %*% P_upd %*% t(F) + Qt
      
      mu <- as.numeric(H %*% Xp)
      S  <- as.numeric(H %*% Pp %*% t(H) + R_fit)
      S  <- max(S, 1e-8)
      sig <- sqrt(S)
      
      yhat[t] <- mu
      
      if (ctype[t] == "none") {
        
        e <- y_obs[t] - mu
        K <- Pp %*% t(H) / S
        
        X_upd <- Xp + as.numeric(K) * e
        P_upd <- (diag(3) - K %*% H) %*% Pp
        
      } else if (ctype[t] == "censored") {
        
        # stockout censoring: latent demand >= y_obs[t]
        beta <- (y_obs[t] - mu) / sig
        beta <- max(min(beta, 7), -7)
        
        EY <- mu + sig * dnorm(beta) / (1 - pnorm(beta) + 1e-12)
        
        e <- EY - mu
        K <- Pp %*% t(H) / S
        
        X_upd <- Xp + as.numeric(K) * e
        P_upd <- (diag(3) - K %*% H) %*% Pp
        
        yhat[t] <- EY
        
      } else if (ctype[t] == "disruption") {
        
        # full censoring: prediction only (skip update)
        X_upd <- Xp
        P_upd <- Pp
        
        # keep yhat[t] = mu (latent mean)
      }
      
      state_matrix[t, ] <- as.numeric(X_upd)
    }
    

    # attach state estimates
    data <- data |>
      mutate(
        yhat     = pmax(0, yhat),
        level    = state_matrix[, 1],
        trend    = state_matrix[, 2],
        seasonal = state_matrix[, 3]
      )
    

    # conformal residuals (synthetic: use actual_demand; empirical: switch to observed)
    set.seed(100)
    
    data <- data |>
      mutate(
        model    = "tckf",
        residual = if_else(
          censorship_type == "none",
          abs(actual_demand - yhat),
          NA_real_
        ),
        resid_list = purrr::accumulate(
          residual,
          ~ c(.x, .y) |> discard(is.na),
          .init = numeric(0)
        )[-1]
      )
    
    residuals <- data$residual[data$censorship_type == "none" & !is.na(data$residual)]
    

    tkf_forecast <- function(data, F, H, X_last, P_last, Q0, lambda, k_t_last, R, residuals,
                             h = 10, alphas = c(0.05, 0.1, 0.15)) {
      
      yhat_f <- numeric(h)
      Pp <- P_last
      
      for (step in 1:h) {
        Qt <- Q0 + lambda * k_t_last * diag(nrow(Q0))
        
        X_last <- F %*% X_last
        Pp <- F %*% Pp %*% t(F) + Qt
        
        yhat_f[step] <- as.numeric(H %*% X_last)
      }
      
      alpha_levels <- sort(unique(alphas))
      
      intervals <- map_dfc(alpha_levels, function(alpha) {
        q_alpha <- quantile(residuals, probs = 1 - alpha, na.rm = TRUE)
        tibble(
          !!paste0("upper_q", 100 * (1 - alpha)) := yhat_f + q_alpha
        )
      })
      
      future_dates <- yearmonth(
        seq(as.Date(max(data$date)) + months(1), by = "month", length.out = h)
      )
      
      tibble(
        date            = future_dates,
        id              = i,
        yhat            = yhat_f,
        model           = "tckf",
        censorship_type = "prediction"
      ) |>
        bind_cols(intervals)
    }
    
    forecast_df <- tkf_forecast(
      data       = data,
      F          = F,
      H          = H,
      X_last     = X_upd,
      P_last     = P_upd,
      Q0         = Q0_fit,     
      lambda     = lam_f,
      k_t_last   = 0L,
      R          = R_fit,
      residuals  = residuals,
      h          = f_horizon,
      alphas     = c(0.025, 0.05, 0.1, 0.15, 0.2)
    )
    
    data_final <- forecast_df |>
      bind_cols(
        test_data |>
          select(c("series_category", "series_type", "store", "product", ".id",
                   "actual_demand", "observed_demand"))
      )
    
    data_final
  }
)


# Stop outer cluster

stopCluster(cl_outer)

# write_rds(tckf_pred, 'data/predictions/tckf_pred_full.rds')

write_rds(tckf_pred, 'data/predictions/tckf_pred_pom.rds')
