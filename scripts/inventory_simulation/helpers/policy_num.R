# helpers - inventory policy

library(tidyverse)
library(lubridate)

# ---- Policy 1: deterministic (current practice) -------------------------

policy_target_deterministic <- function(df, prediction_col = "prediction", buffer = 3) {
  df |>
    mutate(target_stock = buffer * .data[[prediction_col]])
}


# ---- Policy 2: quantile-based (uncertainty-aware base-stock) ------------

policy_target_quantile <- function(df, quantile_col) {
  df |>
    mutate(target_stock = .data[[quantile_col]])
}


# ---- Policy 3: myopic (critical fractile via kappa = p/h) ---------------

# we approximate the required fractile alpha = kappa/(1+kappa) by interpolation.
policy_target_myopic <- function(df,
                                 kappa = 10,
                                 q_cols = c("upper_q80", "upper_q85", "upper_q90", "upper_q95", "upper_q97.5"),
                                 q_levels = c(0.80, 0.85, 0.90, 0.95, 0.975)) {
  
  alpha <- kappa / (1 + kappa)
  stopifnot(length(q_cols) == length(q_levels))
  
  df |>
    mutate(
      target_stock = pmap_dbl(
        pick(all_of(q_cols)),
        function(...) {
          qs <- c(...)
          if (any(is.na(qs))) return(max(qs, na.rm = TRUE))
          as.numeric(approx(x = q_levels, y = qs, xout = alpha, rule = 2)$y)
        }
      )
    )
}


# ---- Policy 4: capped base-stock (cap monthly order quantity) -----------

# Note: cap is implemented in the simulator because it caps q_t, not U_t directly.

policy_target_base_for_cap <- function(df, base = c("quantile", "myopic", "deterministic"),
                                       prediction_col = "prediction",
                                       buffer = 3,
                                       quantile_col = NULL,
                                       kappa = 10,
                                       q_cols = c("upper_q80", "upper_q85", "upper_q90", "upper_q95", "upper_q97.5"),
                                       q_levels = c(0.80, 0.85, 0.90, 0.95, 0.975)) {
  
  base <- match.arg(base)
  
  if (base == "deterministic") {
    return(policy_target_deterministic(df, prediction_col = prediction_col, buffer = buffer))
  }
  
  if (base == "quantile") {
    stopifnot(!is.null(quantile_col))
    return(policy_target_quantile(df, quantile_col = quantile_col))
  }
  
  if (base == "myopic") {
    return(policy_target_myopic(df, kappa = kappa, q_cols = q_cols, q_levels = q_levels))
  }
}


# ---- Policy 5: oracle (upper bound) -------------------------------------

# With L=1, oracle target is next-period latent demand: U_t = D_{t+1}^*

policy_target_oracle <- function(df, demand_col = "actual_demand", L = 1) {
  df |>
    arrange(date) |>
    mutate(
      target_stock = lead(.data[[demand_col]], n = L),
      target_stock = replace_na(target_stock, 0)
    )
}
