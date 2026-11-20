library(doParallel)
library(foreach)
library(tsibble)
library(tidyverse)
library(purrr)

source("scripts/inventory_simulation/helpers/inventory_metrics_revised.R")
source("scripts/inventory_simulation/helpers/inventory_sim_emp_revised.R")
source("scripts/inventory_simulation/helpers/policy_emp.R")

set.seed(100)

# ---- read prediction master ---------------------------------------------

pred_master_base <- read_rds("data/predictions/pred_master_emp.rds") |>
  filter(model %in% c("tckf", "system_gen", "demand_planner")) |>
  mutate(across(starts_with("upper_q"), ~ pmax(.x, 0))) |>
  mutate(
    prediction = pmax(prediction, 0),
    date = as.Date(date),
    id = paste(site_code, product_code, model, sep = "|")
  ) |>
  arrange(site_code, product_code, model, date)


# ---- derive empirical lead-time mix -------------------------------------

next_receipt_delay <- function(received_pos) {
  n <- length(received_pos)
  out <- rep(NA_integer_, n)
  receipt_idx <- which(received_pos)

  if (length(receipt_idx) == 0) {
    return(out)
  }

  for (i in seq_len(n)) {
    j <- receipt_idx[receipt_idx >= i][1]
    if (!is.na(j)) {
      out[i] <- j - i
    }
  }

  out
}

lead_time_probs_emp <- read_rds("data/tidy_data/stock_tsb_filtered.rds") |>
  mutate(
    ordered_pos = stock_ordered > 0,
    received_pos = stock_receieved > 0
  ) |>
  arrange(site_code, product_code, date) |>
  group_by(site_code, product_code) |>
  mutate(next_receipt_delay_m = next_receipt_delay(received_pos)) |>
  ungroup() |>
  filter(ordered_pos) |>
  mutate(
    lead_time_draw = case_when(
      is.na(next_receipt_delay_m) ~ NA_integer_,
      next_receipt_delay_m <= 1 ~ 1L,
      next_receipt_delay_m == 2 ~ 2L,
      next_receipt_delay_m >= 3 ~ 3L
    )
  ) |>
  filter(!is.na(lead_time_draw)) |>
  count(lead_time_draw, name = "n") |>
  mutate(prob = n / sum(n))

lead_time_mix_tbl <- pred_master_base |>
  distinct(site_code, product_code, date) |>
  mutate(
    fixed_lead_time = sample(
      x = lead_time_probs_emp$lead_time_draw,
      size = n(),
      replace = TRUE,
      prob = lead_time_probs_emp$prob
    )
  )


# ---- lead-time scenarios -------------------------------------------------

build_pred_master_scenario <- function(pred_master_base,
                                       scenario,
                                       lead_time_mix_tbl) {

  if (scenario == "lead_time_1") {
    return(pred_master_base |> mutate(fixed_lead_time = 1L))
  }

  if (scenario == "lead_time_2") {
    return(pred_master_base |> mutate(fixed_lead_time = 2L))
  }

  if (scenario == "lead_time_3") {
    return(pred_master_base |> mutate(fixed_lead_time = 3L))
  }

  if (scenario == "lead_time_mix_123") {
    return(
      pred_master_base |>
        left_join(lead_time_mix_tbl, by = c("site_code", "product_code", "date"))
    )
  }

  stop("Unknown scenario.")
}


# ---- local oracle helper for stochastic lead times ----------------------

policy_target_oracle_dynamic <- function(df,
                                         demand_col = "stock_distributed",
                                         lead_time_col = "fixed_lead_time") {

  demand_vec <- df[[demand_col]]
  lead_vec <- df[[lead_time_col]]
  target_idx <- seq_along(demand_vec) + lead_vec

  target_stock <- if_else(
    target_idx <= length(demand_vec),
    demand_vec[target_idx],
    0
  )

  df |>
    mutate(target_stock = replace_na(target_stock, 0))
}


# ---- run one series ------------------------------------------------------

run_one_series_policy_lt_emp <- function(pred_master,
                                         id,
                                         demand_col = "stock_distributed",
                                         policy = c("deterministic", "quantile", "myopic", "capped", "oracle"),
                                         buffer = 3,
                                         quantile_col = "upper_q90",
                                         kappa = 10,
                                         q_max = 999999,
                                         q_cols = c("upper_q80", "upper_q85", "upper_q90", "upper_q95", "upper_q97.5"),
                                         q_levels = c(0.80, 0.85, 0.90, 0.95, 0.975)) {

  policy <- match.arg(policy)

  keys <- stringr::str_split(id, "\\|", simplify = TRUE)
  stopifnot(ncol(keys) == 3)

  df <- pred_master |>
    filter(
      site_code == keys[1],
      product_code == keys[2],
      model == keys[3]
    ) |>
    arrange(date)

  if (nrow(df) == 0) {
    return(tibble())
  }

  df <- switch(
    policy,
    deterministic = policy_target_deterministic(df, prediction_col = "prediction", buffer = buffer),
    quantile = policy_target_quantile(df, quantile_col = quantile_col),
    myopic = policy_target_myopic(df, kappa = kappa, q_cols = q_cols, q_levels = q_levels),
    capped = policy_target_base_for_cap(df, base = "quantile", quantile_col = quantile_col),
    oracle = {
      if (n_distinct(df$fixed_lead_time) == 1) {
        policy_target_oracle(df, demand_col = demand_col, L = df$fixed_lead_time[1])
      } else {
        policy_target_oracle_dynamic(df, demand_col = demand_col, lead_time_col = "fixed_lead_time")
      }
    }
  ) |>
    mutate(target_stock = replace_na(target_stock, 0))

  simulate_inventory_policy_revised_emp(
    df = df,
    demand_col = demand_col,
    lead_time_col = "fixed_lead_time",
    policy_name = policy,
    q_max = ifelse(policy == "capped", q_max, Inf)
  )
}


# ---- parallel backend ----------------------------------------------------

worker_packages <- c("dplyr", "purrr", "stringr", "lubridate", "tidyr", "tibble")

cl <- makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(dplyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(tibble)
  source("scripts/inventory_simulation/helpers/inventory_sim_emp_revised.R")
  source("scripts/inventory_simulation/helpers/policy_emp.R")
  NULL
})

registerDoParallel(cl)


# ---- run policy grid for one scenario -----------------------------------

run_scenario_emp <- function(pred_master_scenario,
                             scenario_label) {

  id_list <- pred_master_scenario$id |> unique()

  common_exports <- c(
    "pred_master_scenario",
    "run_one_series_policy_lt_emp",
    "policy_target_oracle_dynamic"
  )

  # deterministic
  inv_det <- foreach(
    id = id_list,
    .combine = "rbind",
    .packages = worker_packages,
    .export = common_exports
  ) %dopar% {
    run_one_series_policy_lt_emp(
      pred_master = pred_master_scenario,
      id = id,
      demand_col = "stock_distributed",
      policy = "deterministic",
      buffer = 3
    )
  } |>
    mutate(scenario = scenario_label)

  # quantile policies
  quantile_specs <- tibble(
    qcol = c("upper_q80", "upper_q85", "upper_q90", "upper_q95"),
    label = c("upper_q80", "upper_q85", "upper_q90", "upper_q95")
  )

  run_quantile <- function(qcol, label) {
    foreach(
      id = id_list,
      .combine = "rbind",
      .packages = worker_packages,
      .export = common_exports
    ) %dopar% {
      run_one_series_policy_lt_emp(
        pred_master = pred_master_scenario,
        id = id,
        demand_col = "stock_distributed",
        policy = "quantile",
        quantile_col = qcol
      )
    } |>
      mutate(policy = label, scenario = scenario_label)
  }

  quantile_results <- map2(quantile_specs$qcol, quantile_specs$label, run_quantile)
  names(quantile_results) <- quantile_specs$label

  # myopic policies
  myopic_specs <- tibble(
    kappa = c(5, 10, 20),
    label = c("myopic (k=5)", "myopic (k=10)", "myopic (k=20)")
  )

  run_myopic <- function(kappa_value, label) {
    foreach(
      id = id_list,
      .combine = "rbind",
      .packages = worker_packages,
      .export = common_exports
    ) %dopar% {
      run_one_series_policy_lt_emp(
        pred_master = pred_master_scenario,
        id = id,
        demand_col = "stock_distributed",
        policy = "myopic",
        kappa = kappa_value
      )
    } |>
      mutate(policy = label, scenario = scenario_label)
  }

  myopic_results <- map2(myopic_specs$kappa, myopic_specs$label, run_myopic)
  names(myopic_results) <- myopic_specs$label

  # capped base-stock
  inv_cap <- foreach(
    id = id_list,
    .combine = "rbind",
    .packages = worker_packages,
    .export = common_exports
  ) %dopar% {
    q_max_95 <- pred_master_scenario |>
      filter(.data$id == id) |>
      summarise(q_max = as.numeric(quantile(stock_distributed, 0.95, na.rm = TRUE))) |>
      pull(q_max)

    run_one_series_policy_lt_emp(
      pred_master = pred_master_scenario,
      id = id,
      demand_col = "stock_distributed",
      policy = "capped",
      quantile_col = "upper_q90",
      q_max = q_max_95
    )
  } |>
    mutate(scenario = scenario_label)

  # oracle
  inv_oracle <- foreach(
    id = id_list,
    .combine = "rbind",
    .packages = worker_packages,
    .export = common_exports
  ) %dopar% {
    run_one_series_policy_lt_emp(
      pred_master = pred_master_scenario,
      id = id,
      demand_col = "stock_distributed",
      policy = "oracle"
    )
  } |>
    mutate(scenario = scenario_label)

  bind_rows(
    inv_det,
    quantile_results[["upper_q80"]],
    quantile_results[["upper_q85"]],
    quantile_results[["upper_q90"]],
    quantile_results[["upper_q95"]],
    myopic_results[["myopic (k=5)"]],
    myopic_results[["myopic (k=10)"]],
    myopic_results[["myopic (k=20)"]],
    inv_cap,
    inv_oracle
  )
}


# ---- run all lead-time scenarios ----------------------------------------

scenario_labels <- c(
  "lead_time_1",
  "lead_time_2",
  "lead_time_3",
  "lead_time_mix_123"
)

inv_df_emp_leadtime <- map_dfr(
  scenario_labels,
  function(scenario_label) {
    pred_master_scenario <- build_pred_master_scenario(
      pred_master_base = pred_master_base,
      scenario = scenario_label,
      lead_time_mix_tbl = lead_time_mix_tbl
    )

    run_scenario_emp(
      pred_master_scenario = pred_master_scenario,
      scenario_label = scenario_label
    )
  }
)

stopCluster(cl)


# ---- write simulation output --------------------------------------------

write_rds(inv_df_emp_leadtime, "data/results/inv_df_emp_leadtime_sensitivity.rds")
write_rds(lead_time_probs_emp, "data/results/lead_time_probs_emp.rds")


# ---- metrics -------------------------------------------------------------

inventory_metrics_revised_lt <- function(inv_df,
                                         demand_col = "stock_distributed") {
  inv_df |>
    mutate(id = paste(site_code, product_code, model, sep = "|")) |>
    group_by(id, site_code, product_code, model, scenario, policy) |>
    group_modify(~ summarise_inventory_metrics_revised(.x, demand_col = demand_col)) |>
    ungroup()
}

metrics_inv_emp_leadtime <- inventory_metrics_revised_lt(
  inv_df_emp_leadtime |>
    filter(date != "2019-01-01", censorship_type == "none"),
  demand_col = "stock_distributed"
)

write_rds(metrics_inv_emp_leadtime, "data/results/metrics_inv_emp_leadtime_sensitivity.rds")
