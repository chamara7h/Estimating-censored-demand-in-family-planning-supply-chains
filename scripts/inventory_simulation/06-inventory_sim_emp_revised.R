library(doParallel)
library(foreach)
library(tsibble)
library(tidyverse)
library(purrr)

source("scripts/inventory_simulation/helpers/inventory_metrics_revised.R")
source("scripts/inventory_simulation/helpers/inventory_sim_emp_revised.R")
source("scripts/inventory_simulation/helpers/policy_emp.R")

set.seed(100)

pred_master <- read_rds("data/predictions/pred_master_emp.rds") |>
  mutate(across(starts_with("upper_q"), ~ pmax(.x, 0))) |>
  mutate(
    prediction = pmax(prediction, 0),
    date = as.Date(date),
    id = paste(site_code, product_code, model, sep = "|")
  ) |>
  arrange(site_code, product_code, model, date)

id_list <- pred_master$id |> unique()
worker_packages <- c("dplyr", "purrr", "stringr", "lubridate", "tidyr", "tibble")

# parallel backend
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

# ---- run deterministic ---------------------------------------------------
inv_det <- foreach(id = id_list, .combine = "rbind",.packages = worker_packages) %dopar% {
  run_one_series_policy_revised_emp(
    pred_master = pred_master,
    id = id,
    demand_col = "stock_distributed",
    lead_time = 1,
    policy = "deterministic",
    buffer = 3
  )
}

# ---- run quantile policy (e.g., upper_q90) -------------------------------
quantile_specs <- tibble(
  qcol = c("upper_q80", "upper_q85", "upper_q90", "upper_q95"),
  label = c("upper_q80", "upper_q85", "upper_q90", "upper_q95")
)

run_quantile <- function(qcol, label) {
  foreach(id = id_list, .combine = "rbind", .packages = worker_packages, .export = c("pred_master", "id_list")) %dopar% {
    run_one_series_policy_revised_emp(
      pred_master = pred_master,
      id = id,
      demand_col = "stock_distributed",
      lead_time = 1,
      policy = "quantile",
      quantile_col = qcol
    ) |>
      mutate(policy = label)
  }
}

quantile_results <- map2(quantile_specs$qcol, quantile_specs$label, run_quantile)
names(quantile_results) <- quantile_specs$label

inv_q80 <- quantile_results[["upper_q80"]]
inv_q85 <- quantile_results[["upper_q85"]]
inv_q90 <- quantile_results[["upper_q90"]]
inv_q95 <- quantile_results[["upper_q95"]]

# ---- run myopic policy (kappa grid) -------------------------------------
run_myopic <- function(kappa_value, label) {
  foreach(id = id_list, .combine = "rbind", .packages = worker_packages, .export = c("pred_master", "id_list")) %dopar% {
    run_one_series_policy_revised_emp(
      pred_master = pred_master,
      id = id,
      demand_col = "stock_distributed",
      lead_time = 1,
      policy = "myopic",
      kappa = kappa_value
    ) |>
      mutate(policy = label)
  }
}

myopic_specs <- tibble(
  kappa = c(5, 10, 20),
  label = c("myopic (k=5)", "myopic (k=10)", "myopic (k=20)")
)

myopic_results <- map2(myopic_specs$kappa, myopic_specs$label, run_myopic)
names(myopic_results) <- myopic_specs$label

inv_myopic_k5 <- myopic_results[["myopic (k=5)"]]
inv_myopic_k10 <- myopic_results[["myopic (k=10)"]]
inv_myopic_k20 <- myopic_results[["myopic (k=20)"]]

# ---- run capped base-stock ----------------------------------------------
inv_cap <- foreach(id = id_list, .combine = "rbind", .packages = worker_packages) %dopar% {
  q_max_95 <- pred_master |>
    filter(.data$id == id) |>
    summarise(q_max = as.numeric(quantile(stock_distributed, 0.95, na.rm = TRUE))) |>
    pull(q_max)

  run_one_series_policy_revised_emp(
    pred_master = pred_master,
    id = id,
    demand_col = "stock_distributed",
    lead_time = 1,
    policy = "capped",
    quantile_col = "upper_q90",
    q_max = q_max_95
  )
}

# ---- run oracle ----------------------------------------------------------
inv_oracle <- foreach(id = id_list, .combine = "rbind", .packages = worker_packages) %dopar% {
  run_one_series_policy_revised_emp(
    pred_master = pred_master,
    id = id,
    demand_col = "stock_distributed",
    lead_time = 1,
    policy = "oracle"
  )
}

stopCluster(cl)

inv_df_revised <- bind_rows(
  inv_det,
  inv_q80,
  inv_q85,
  inv_q90,
  inv_q95,
  inv_myopic_k5,
  inv_myopic_k10,
  inv_myopic_k20,
  inv_cap,
  inv_oracle
)

write_rds(inv_df_revised, "data/results/inv_df_emp_revised.rds")

inventory_metrics_revised <- function(inv_df, demand_col = "stock_distributed") {
  inv_df |>
    mutate(id = paste(site_code, product_code, model, sep = "|")) |>
    group_by(id, site_code, product_code, model, policy) |>
    group_modify(~ summarise_inventory_metrics_revised(.x, demand_col = demand_col)) |>
    ungroup()
}

metrics_inv_emp_revised <- inventory_metrics_revised(
  inv_df_revised |>
    filter(date != "2019-01-01", censorship_type == "none"),
  demand_col = "stock_distributed"
)

write_rds(metrics_inv_emp_revised, "data/results/metrics_inv_emp_revised.rds")
