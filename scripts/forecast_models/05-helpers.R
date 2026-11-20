# helpers

clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

build_k_t <- function(censorship_type) {
  purrr::accumulate(censorship_type != "none",
                    ~ if_else(.y, .x + 1L, 0L),
                    .init = 0L)[-1]
}

stl_init <- function(data, train_n = 30) {
  stl_data <- data |> slice(1:train_n)
  
  ts_train <- ts(
    stl_data$observed_demand,
    frequency = 12,
    start     = c(year(min(stl_data$date)), month(min(stl_data$date)))
  )
  
  ts_train[stl_data$censorship_type != "none"] <- NA
  ts_filled <- na.interp(ts_train)
  dim(ts_filled) <- NULL
  
  stl(ts_filled, s.window = "periodic", robust = TRUE)
}

make_spec <- function(spec = c("level", "level_season", "full")) {
  spec <- match.arg(spec)
  
  if (spec == "level") {
    list(
      name = "level",
      dim  = 1L,
      F = function(phi) matrix(1, 1, 1),
      H = matrix(1, 1, 1),
      init_from_stl = function(stl_out) {
        ell0 <- last(stl_out$time.series[, "trend"])
        P0   <- matrix(var(stl_out$time.series[, "trend"], na.rm = TRUE), 1, 1)
        list(X0 = c(ell0), P0 = P0)
      }
    )
  } else if (spec == "level_season") {
    list(
      name = "level_season",
      dim  = 2L,
      F = function(phi) matrix(c(1, 0,
                                 0, phi), 2, 2, byrow = TRUE),
      H = matrix(c(1, 1), 1, 2),
      init_from_stl = function(stl_out) {
        ell0   <- last(stl_out$time.series[, "trend"])
        gamma0 <- last(stl_out$time.series[, "seasonal"])
        P0_diag <- c(
          var(stl_out$time.series[, "trend"], na.rm = TRUE),
          var(stl_out$time.series[, "seasonal"], na.rm = TRUE)
        )
        list(X0 = c(ell0, gamma0), P0 = diag(P0_diag))
      }
    )
  } else {
    list(
      name = "full",
      dim  = 3L,
      F = function(phi) matrix(c(1, 1, 0,
                                 0, 1, 0,
                                 0, 0, phi), 3, 3, byrow = TRUE),
      H = matrix(c(1, 0, 1), 1, 3),
      init_from_stl = function(stl_out) {
        ell0   <- last(stl_out$time.series[, "trend"])
        tau0   <- mean(diff(stl_out$time.series[, "trend"]), na.rm = TRUE)
        gamma0 <- last(stl_out$time.series[, "seasonal"])
        P0_diag <- c(
          var(stl_out$time.series[, "trend"], na.rm = TRUE),
          var(diff(stl_out$time.series[, "trend"]), na.rm = TRUE),
          var(stl_out$time.series[, "seasonal"], na.rm = TRUE)
        )
        list(X0 = c(ell0, tau0, gamma0), P0 = diag(P0_diag))
      }
    )
  }
}


# fit parameters (ML) for a given spec
fit_params <- function(data, spec, stl_out, train_n = 30) {
  
  init <- spec$init_from_stl(stl_out)
  X0 <- init$X0
  P0 <- init$P0
  d  <- spec$dim
  
  if (!"k_t" %in% names(data)) {
    data <- data |> mutate(k_t = build_k_t(censorship_type))
  }
  
  neg_ll <- function(par) {
    Q0_diag <- exp(par[1:d])
    R       <- exp(par[d + 1])
    phi     <- plogis(par[d + 2])
    lambda  <- exp(par[d + 3])
    
    F  <- spec$F(phi)
    H  <- spec$H
    Q0 <- diag(Q0_diag, d, d)
    
    X_upd <- X0
    P_upd <- P0
    ll <- 0
    
    for (t in (train_n + 1):nrow(data)) {
      
      Qt <- Q0 + lambda * data$k_t[t] * diag(d)
      Xp <- F %*% X_upd
      Pp <- F %*% P_upd %*% t(F) + Qt
      
      mu_t    <- as.numeric(H %*% Xp)
      sigma_t <- sqrt(as.numeric(H %*% Pp %*% t(H) + R))
      
      if (data$censorship_type[t] == "none") {
        
        e  <- data$observed_demand[t] - mu_t
        ll <- ll + 0.5 * (log(sigma_t^2) + (e^2) / sigma_t^2)
        
        K <- Pp %*% t(H) / sigma_t^2
        X_upd <- Xp + as.numeric(K) * e
        P_upd <- (diag(d) - K %*% H) %*% Pp
        
      } else if (data$censorship_type[t] == "censored") {
        
        z  <- (data$observed_demand[t] - mu_t) / sigma_t
        ll <- ll - log(1 - pnorm(z) + 1e-12)
        
      } else if (data$censorship_type[t] == "disruption") {
        
        z  <- (0 - mu_t) / sigma_t
        ll <- ll - log(1 - pnorm(z) + 1e-12)
      }
    }
    
    ll
  }
  
  unc_var <- var(data$observed_demand[data$censorship_type == "none"], na.rm = TRUE)
  unc_var <- ifelse(is.na(unc_var) || unc_var <= 0, 1, unc_var)
  
  par0 <- c(
    log(rep(0.1, d)),
    log(unc_var),
    qlogis(0.9),
    log(0.5)
  )
  
  opt <- optim(
    par0, neg_ll,
    method = "L-BFGS-B",
    lower  = c(rep(-10, d), log(1e-6), qlogis(1e-3),  log(1e-6)),
    upper  = c(rep( 10, d), log(1e+6), qlogis(1-1e-3),log(1e+1))
  )
  
  ph <- opt$par
  
  list(
    Q0 = diag(exp(ph[1:d]), d, d),
    R  = exp(ph[d + 1]),
    phi = plogis(ph[d + 2]),
    lambda_hat = exp(ph[d + 3]),
    opt = opt,
    X0 = X0,
    P0 = P0
  )
}


# filter with TCKF update rules
run_filter <- function(data, spec, fit, train_n = 30, lambda_choice = c("hat","fixed"), lambda_fixed = NA_real_) {
  
  lambda_choice <- match.arg(lambda_choice)
  
  d <- spec$dim
  lam <- if (lambda_choice == "hat") fit$lambda_hat else lambda_fixed
  lam <- clamp(lam, 0, 1)
  
  if (!"k_t" %in% names(data)) {
    data <- data |> mutate(k_t = build_k_t(censorship_type))
  }
  
  F <- spec$F(fit$phi)
  H <- spec$H
  
  X_upd <- fit$X0
  P_upd <- fit$P0
  
  n <- nrow(data)
  yhat <- rep(NA_real_, n)
  state_mat <- matrix(NA_real_, nrow = n, ncol = d)
  
  for (t in (train_n + 1):n) {
    
    Qt <- fit$Q0 + lam * data$k_t[t] * diag(d)
    Xp <- F %*% X_upd
    Pp <- F %*% P_upd %*% t(F) + Qt
    
    mu_t    <- as.numeric(H %*% Xp)
    sigma_t <- sqrt(as.numeric(H %*% Pp %*% t(H) + fit$R))
    
    yhat[t] <- mu_t
    
    if (data$censorship_type[t] == "none") {
      
      S <- sigma_t^2
      e <- data$observed_demand[t] - mu_t
      K <- Pp %*% t(H) / S
      
      X_upd <- Xp + as.numeric(K) * e
      P_upd <- (diag(d) - K %*% H) %*% Pp
      
    } else if (data$censorship_type[t] == "censored") {
      
      beta <- (data$observed_demand[t] - mu_t) / sigma_t
      beta <- clamp(beta, -7, 7)
      
      EY <- mu_t + sigma_t * dnorm(beta) / (1 - pnorm(beta) + 1e-12)
      e  <- EY - mu_t
      S  <- sigma_t^2
      K  <- Pp %*% t(H) / S
      
      X_upd <- Xp + as.numeric(K) * e
      P_upd <- (diag(d) - K %*% H) %*% Pp
      
      yhat[t] <- EY
      
    } else if (data$censorship_type[t] == "disruption") {
      
      alpha <- (0 - mu_t) / sigma_t
      alpha <- clamp(alpha, -7, 7)
      
      EY <- mu_t + sigma_t * dnorm(alpha) / (1 - pnorm(alpha) + 1e-12)
      yhat[t] <- EY
      
      X_upd <- Xp
      P_upd <- Pp
    }
    
    state_mat[t, ] <- X_upd
  }
  
  out <- data |>
    mutate(
      yhat = pmax(0, yhat)
    )
  
  if (spec$name == "level") {
    out <- out |> mutate(level = state_mat[, 1])
  } else if (spec$name == "level_season") {
    out <- out |> mutate(level = state_mat[, 1], seasonal = state_mat[, 2])
  } else {
    out <- out |> mutate(level = state_mat[, 1], trend = state_mat[, 2], seasonal = state_mat[, 3])
  }
  
  out
}


# conformal intervals (global vs rolling window)

add_conformal <- function(df, alphas = c(0.025, 0.05, 0.1, 0.15, 0.2),
                          mode = c("global","rolling"), window = 24,
                          use_actual = TRUE) {
  
  mode <- match.arg(mode)
  
  df <- df |>
    mutate(
      resid = case_when(
        censorship_type == "none" & use_actual  ~ abs(actual_demand - yhat),
        censorship_type == "none" & !use_actual ~ abs(observed_demand - yhat),
        TRUE ~ NA_real_
      )
    )
  
  res_pool <- numeric(0)
  
  # store q_t for each alpha, then build intervals
  q_store <- matrix(NA_real_, nrow = nrow(df), ncol = length(alphas))
  colnames(q_store) <- paste0("q_", 100 * (1 - alphas))
  
  for (t in seq_len(nrow(df))) {
    if (!is.na(df$resid[t])) res_pool <- c(res_pool, df$resid[t])
    
    for (j in seq_along(alphas)) {
      alpha <- alphas[j]
      
      if (length(res_pool) < 10) {
        q_store[t, j] <- NA_real_
      } else {
        pool_use <- if (mode == "global") res_pool else tail(res_pool, window)
        q_store[t, j] <- as.numeric(quantile(pool_use, probs = 1 - alpha, na.rm = TRUE, type = 8))
      }
    }
  }
  
  q_df <- as_tibble(q_store)
  
  out <- bind_cols(df, q_df)
  
  for (j in seq_along(alphas)) {
    nm <- paste0("q_", 100 * (1 - alphas[j]))
    up <- paste0("upper_q", 100 * (1 - alphas[j]))
    lo <- paste0("lower_q", 100 * (1 - alphas[j]))
    
    out[[up]] <- out$yhat + replace_na(out[[nm]], 0)
    out[[lo]] <- pmax(0, out$yhat - replace_na(out[[nm]], 0))
  }
  
  out |> select(-resid, starts_with("q_"))
}