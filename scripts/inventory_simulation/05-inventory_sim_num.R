library(doParallel)
library(foreach)
library(tsibble)

source('scripts/inventory_simulation/helpers/inventory_metrics.R')
source('scripts/inventory_simulation/helpers/inventory_sim_num.R')
source('scripts/inventory_simulation/helpers/policy_num.R')

set.seed(100)

pred_master <- read_rds("data/predictions/pred_master.rds") |>
  mutate(date = as.Date(date)) |>
  arrange(series_category, series_type, store, product, model, date)

id_table <- pred_master |>
  distinct(series_category, series_type, store, product, model) |>
  mutate(id = paste(series_category, series_type, store, product, model, sep = "|"))

id_list <- id_table$id |> unique()

# id_list <- sample(id_list, 1)

# parallel backend
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# ---- run deterministic ---------------------------------------------------

system.time(
  inv_det <- foreach(id = id_list,
                     .combine = "rbind",
                     .packages = c("tidyverse", "lubridate")) %dopar% {
                       
                       # id = id_list[1]
                       
                       run_one_series_policy(
                         pred_master = pred_master,
                         id          = id,
                         demand_col  = "actual_demand",
                         lead_time   = 1,
                         policy      = "deterministic",
                         buffer      = 3
                       )
                     }
)

# ---- run quantile policy (e.g., upper_q90) -------------------------------

system.time(
  inv_q80 <- foreach(id = id_list,
                   .combine = "rbind",
                   .packages = c("tidyverse", "lubridate")) %dopar% {
                     
                     run_one_series_policy(
                       pred_master  = pred_master,
                       id           = id,
                       demand_col   = "actual_demand",
                       lead_time    = 1,
                       policy       = "quantile",
                       quantile_col = "upper_q80"
                     ) |> 
                       mutate(policy = "upper_q80") 
                   }
)

system.time(
  inv_q85 <- foreach(id = id_list,
                     .combine = "rbind",
                     .packages = c("tidyverse", "lubridate")) %dopar% {
                       
                       run_one_series_policy(
                         pred_master  = pred_master,
                         id           = id,
                         demand_col   = "actual_demand",
                         lead_time    = 1,
                         policy       = "quantile",
                         quantile_col = "upper_q85"
                       ) |> 
                         mutate(policy = "upper_q85") 
                     }
)

system.time(
  inv_q90 <- foreach(id = id_list,
                     .combine = "rbind",
                     .packages = c("tidyverse", "lubridate")) %dopar% {
                       
                       run_one_series_policy(
                         pred_master  = pred_master,
                         id           = id,
                         demand_col   = "actual_demand",
                         lead_time    = 1,
                         policy       = "quantile",
                         quantile_col = "upper_q90"
                       ) |> 
                         mutate(policy = "upper_q90") 
                     }
)

system.time(
  inv_q95 <- foreach(id = id_list,
                     .combine = "rbind",
                     .packages = c("tidyverse", "lubridate")) %dopar% {
                       
                       run_one_series_policy(
                         pred_master  = pred_master,
                         id           = id,
                         demand_col   = "actual_demand",
                         lead_time    = 1,
                         policy       = "quantile",
                         quantile_col = "upper_q95"
                       ) |> 
                         mutate(policy = "upper_q95") 
                     }
)



# ---- run myopic policy (kappa grid) -------------------------------------

system.time(
  inv_myopic_k5 <- foreach(id = id_list,
                        .combine = "rbind",
                        .packages = c("tidyverse", "lubridate")) %dopar% {
                          
                          run_one_series_policy(
                            pred_master = pred_master,
                            id          = id,
                            demand_col  = "actual_demand",
                            lead_time   = 1,
                            policy      = "myopic",
                            kappa       = 5
                          ) |>
                            mutate(policy = "myopic (k=5)")
                            
                        }
)

system.time(
  inv_myopic_k10 <- foreach(id = id_list,
                           .combine = "rbind",
                           .packages = c("tidyverse", "lubridate")) %dopar% {
                             
                             run_one_series_policy(
                               pred_master = pred_master,
                               id          = id,
                               demand_col  = "actual_demand",
                               lead_time   = 1,
                               policy      = "myopic",
                               kappa       = 10
                             ) |>
                               mutate(policy = "myopic (k=10)")
                             
                           }
)

system.time(
  inv_myopic_k20 <- foreach(id = id_list,
                           .combine = "rbind",
                           .packages = c("tidyverse", "lubridate")) %dopar% {
                             
                             run_one_series_policy(
                               pred_master = pred_master,
                               id          = id,
                               demand_col  = "actual_demand",
                               lead_time   = 1,
                               policy      = "myopic",
                               kappa       = 20
                             ) |>
                               mutate(policy = "myopic (k=20)")
                             
                           }
)


# ---- run capped base-stock ----------------------------------------------

system.time(
  inv_cap <- foreach(id = id_list,
                     .combine = "rbind",
                     .packages = c("tidyverse", "lubridate")) %dopar% {
                       
                       q_max_95 <- pred_master |>
                         filter(id == id) |>
                         summarise(q_max = as.numeric(quantile(actual_demand, 0.95, na.rm = TRUE))) |>
                         pull(q_max)
                       
                       run_one_series_policy(
                         pred_master  = pred_master,
                         id           = id,
                         demand_col   = "actual_demand",
                         lead_time    = 1,
                         policy       = "capped",
                         quantile_col = "upper_q90",
                         q_max        = q_max_95
                       ) 
                     }
)

# ---- run oracle ----------------------------------------------------------

system.time(
  inv_oracle <- foreach(id = id_list,
                        .combine = "rbind",
                        .packages = c("tidyverse", "lubridate")) %dopar% {
                          
                          run_one_series_policy(
                            pred_master = pred_master,
                            id          = id,
                            demand_col  = "actual_demand",
                            lead_time   = 1,
                            policy      = "oracle"
                          ) 
                        }
)

stopCluster(cl)


# Metrics summary

add_id_cols <- function(df) {
  df |>
    mutate(id = paste(series_category, series_type, store, product, model, sep = "|"))
}

inv_df <- inv_det |> 
  bind_rows(
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

write_rds(inv_df, 'data/results/inv_df_num.rds')

inventory_metrics <- function(inv_df, demand_col = "actual_demand") {
  
  inv_df |>
    add_id_cols() |>
    group_by(id, series_category, series_type, store, product, model, policy) |>
    group_modify(~{
      mets <- summarise_inventory_metrics(.x, demand_col = demand_col)
      bind_cols(mets$summary, tibble(loss_curve = list(mets$loss_curve)))
    }) |>
    ungroup()
}

metrics_inv_num <- inventory_metrics(inv_df, demand_col = "actual_demand")

write_rds(metrics_inv_num, 'data/results/metrics_inv_num.rds')



metrics_inv_num |>
  group_by(policy, model) |>
  summarise(
    CSL = mean(CSL, na.rm = TRUE),
    stockout_rate = mean(stockout_rate, na.rm = TRUE),
    I_norm = mean(I_norm, na.rm = TRUE),
    avg_I = mean(avg_I, na.rm = TRUE),
    .groups = "drop"
  ) |> view()



metrics_det_long <- metrics_inv_num |>
  unnest(loss_curve)

metrics_det_long |>
  group_by(policy, model, lambda) |>
  summarise(
    CSL = mean(CSL, na.rm = TRUE),
    stockout_rate = mean(stockout_rate, na.rm = TRUE),
    I_norm = mean(I_norm, na.rm = TRUE),
    avg_I = mean(avg_I, na.rm = TRUE),
    L = mean(L, na.rm = TRUE),
    .groups = "drop"
  ) |> view()
