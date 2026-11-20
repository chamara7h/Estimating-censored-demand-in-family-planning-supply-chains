# POM Revision - model diagnostic

library(tidyverse)
library(forecast)
library(tsibble)
library(doParallel)
library(foreach)

source('scripts/forecast_models/05-helpers.R')

set.seed(100)

# load data

s_data <- read_rds("data/tidy_data/synthetic_data.rds") |>
  mutate(
    censorship_type = as.factor(censorship_type),
    censorship_type = if_else(observed_demand == actual_demand, "none", as.character(censorship_type))
  ) |>
  mutate(censorship_type = as.factor(censorship_type)) |>
  as_tsibble(
    key   = c(series_category, series_type, store, product),
    index = date
  )

# pick 100 keys (series) at random
keys_100 <- s_data |>
  distinct(series_category, series_type, store, product) |>
  slice_sample(n = 500)

# filter the tsibble to those 100 series
s_data_100 <- s_data |>
  semi_join(keys_100, by = c("series_category", "series_type", "store", "product"))

# create an id you can use in foreach
s_data_100 <- s_data_100 |>
  mutate(id = paste(series_category, series_type, store, product, sep = "-")) |>
  as_tsibble(key = id, index = date)

id_list <- s_data_100 |>
  distinct(id) |>
  pull(id)


# run diagnostics for all series

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

diag_grid <- tidyr::crossing(
  diag_spec = c("level", "level_season", "full"),
  diag_lambda_choice = c("0", "hat", "1"),
  diag_cp_mode = c("global", "rolling")
)

# choose once
train_n <- 30
alphas  <- c(0.025, 0.05, 0.1, 0.15, 0.2)
roll_w  <- 24

# Set TRUE for synthetic (has actual_demand), FALSE for empirical (no actual)
use_actual_for_resid <- TRUE

system.time(
  tckf_diag_pred <- foreach(
    i = id_list,
    .combine  = "rbind",
    .packages = c("doParallel","foreach","tidyverse","forecast","tsibble","lubridate","purrr")
  ) %dopar% {
    
    # i = id_list[54] 
    
    # data
    data <- s_data_100 |>
      as_tibble() |>
      filter(id == i) |>
      arrange(date)
    
    # safety
    if (nrow(data) <= (train_n + 2)) return(NULL)
    
    # build k_t once
    data <- data |> mutate(k_t = build_k_t(censorship_type))
    
    # stl init once
    stl_out <- stl_init(data, train_n = train_n)
    

    # loop over diagnostic grid
    out_all <- vector("list", nrow(diag_grid))
    
    for (g in seq_len(nrow(diag_grid))) {
      
      spec_name   <- diag_grid$diag_spec[g]
      lam_choice  <- diag_grid$diag_lambda_choice[g]
      cp_mode     <- diag_grid$diag_cp_mode[g]
      
      spec <- make_spec(spec_name)
      
      # fit params for this spec
      fit <- fit_params(data, spec, stl_out, train_n = train_n)
      
      # decide lambda
      if (lam_choice == "hat") {
        df_filt <- run_filter(data, spec, fit, train_n = train_n, lambda_choice = "hat")
        lam_used <- clamp(fit$lambda_hat, 0, 1)
      } else {
        lam_fixed <- as.numeric(lam_choice)
        df_filt <- run_filter(data, spec, fit, train_n = train_n, lambda_choice = "fixed", lambda_fixed = lam_fixed)
        lam_used <- lam_fixed
      }
      
      # add conformal intervals (global vs rolling)
      df_cp <- add_conformal(
        df_filt,
        alphas = alphas,
        mode   = cp_mode,
        window = roll_w,
        use_actual = use_actual_for_resid
      )
      
      # tag diagnostics + store key fitted params
      out_all[[g]] <- df_cp |>
        mutate(
          id = i,
          model = "tckf_diag",
          diag_spec = spec_name,
          diag_lambda_choice = lam_choice,
          diag_lambda = lam_used,
          diag_cp_mode = cp_mode,
          diag_cp_window = roll_w,
          phi = fit$phi,
          R   = fit$R,
          Q0_1 = fit$Q0[1,1],
          Q0_2 = ifelse(nrow(fit$Q0) >= 2, fit$Q0[2,2], NA_real_),
          Q0_3 = ifelse(nrow(fit$Q0) >= 3, fit$Q0[3,3], NA_real_)
        )
    }
    
    bind_rows(out_all) 
  }
)


stopCluster(cl)

# quick example summaries
diag_summary <- tckf_diag_pred |>
  filter(!is.na(yhat)) |>
  group_by(diag_spec, diag_lambda_choice, diag_cp_mode) |>
  summarise(
    rmse = sqrt(mean((yhat - actual_demand)^2, na.rm = TRUE)),
    # csl  = mean(in_stock == 1, na.rm = TRUE),
    .groups = "drop"
  )

write_rds(tckf_diag_pred |> filter(!is.na(yhat)), 'data/predictions/tckf_diag.rds')
