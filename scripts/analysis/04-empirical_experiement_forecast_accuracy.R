# Forecast models accuracy - empirical experiment

library(tidyverse)
library(tsibble)
library(Metrics)
library(SpecsVerification)
library(tsutils)
library(feasts)


# Load data ---------------------------------------------------------------

stock_tsb <- read_rds('data/tidy_data/stock_tsb_filtered.rds') |> 
  as_tibble() |> 
  mutate(censorship_type = case_when(stock_out_indicator == 1 ~ 'censored',
                                     interruption_indicator == 1 ~ 'disruption',
                                     .default = 'none'),
         censorship_type = as.factor(censorship_type)) |>
  select(date, site_code, product_code, censorship_type, stock_distributed, projected_average_monthly_consumption, corrected_cmm) |> 
  fastDummies::dummy_cols(select_columns = "censorship_type") |> 
  as_tsibble(index = date, key = c(site_code, product_code)) |> 
  fill_gaps(.full = TRUE) |>  # add zeros to cold staters
  mutate(censorship_type = replace_na(censorship_type, 'none'),
         stock_distributed = replace_na(stock_distributed, 0),
         censorship_type_censored = replace_na(censorship_type_censored, 0),
         censorship_type_disruption = replace_na(censorship_type_disruption, 0),
         censorship_type_none = replace_na(censorship_type_none, 1))

stock_feat <- stock_tsb |>
  filter_index(. ~ '2018 Dec') |> 
  filter(censorship_type != 'disruption') |> 
  features(stock_distributed, feature_set(pkgs = "feasts")) |>  
  mutate(across(where(is.double), ~ replace(.x, is.nan(.x), 0))) # select seires with high 0 obervations

drop_ids <- stock_feat |> 
  filter(zero_run_mean > 10) |> 
  mutate(id = paste0(site_code, '-', product_code)) |> 
  pull(id)

stock_tsb <- stock_tsb |> 
  mutate(id = paste0(site_code, '-', product_code)) |> 
  filter(!id %in% drop_ids) |> 
  select(-id)


# creating training set

train_len <- 36
test_len <- 12

s_data_train <- stock_tsb |>
  filter_index(. ~ as.character(min(stock_tsb$date)+(train_len - 1))) 

# create tscv train data

f_horizon = 1

s_data_train_tscv <- stock_tsb |> 
  stretch_tsibble(.init = (length(unique(s_data_train$date)) - f_horizon + 1), .step = 1) |>
  group_by(.id, site_code, product_code) |> 
  mutate(period = row_number(),
         id = paste0(site_code,'-',product_code,'-',.id)) |> 
  ungroup()

# create tscv test data

s_data_test_tscv <- stock_tsb |> 
  filter_index(as.character(max(stock_tsb$date)-(test_len) + 1) ~ .) |>
  slide_tsibble(.size = 1, .step = 1) |> 
  mutate(censorship_type_censored = 0, censorship_type_disruption = 0, censorship_type_none = 1) |> 
  group_by(.id, site_code, product_code) |> 
  mutate(period = row_number(),
         id = paste0(site_code,'-',product_code,'-',.id)) |> 
  ungroup()

# Pred data ---------------------------------------------------------------

tckf_pred <- read_rds('data/predictions/tckf_pred_pom_emp_new.rds') |>
  rename(prediction = yhat) |> 
  mutate(model = 'tckf',
         .id = as.integer(.id))

naive_prob_pred <- read_rds('data/predictions/naive_prob_emp.rds') |> filter(.id != 13) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |>  
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

sba_prob_pred <- read_rds('data/predictions/sba_prob_emp.rds') |> filter(.id != 13) |> 
  select(-stock_distributed) |> 
  mutate(model = 'sba') |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

ma_prob_reg_pred <- read_rds('data/predictions/ma_prob_reg_emp.rds') |> filter(.id != 13) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  select(-c(date)) |> 
  mutate(model = 'moving_average_reg') |>
  left_join(tckf_pred |> 
              select(.id, date, site_code, product_code, stock_distributed, censorship_type) |> 
              mutate(.id = as.integer(.id)),
            by = c('.id', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

ma_prob_pred <- read_rds('data/predictions/ma_prob_emp.rds') |> filter(.id != 13) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  left_join(tckf_pred |> 
              select(.id, site_code, product_code, stock_distributed, censorship_type) |> 
              mutate(.id = as.integer(.id)),
            by = c('.id', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

lr_prob_reg_pred <- read_rds('data/predictions/lr_prob_reg_emp.rds') |> 
  select(-c(censorship_type, censorship_type_disruption, censorship_type_censored, censorship_type_none, id)) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |>  
  mutate(model = 'lr_reg') |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

lr_prob_pred <- read_rds('data/predictions/lr_prob_emp.rds') |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

ets_prob_pred <- read_rds('data/predictions/ets_prob_emp.rds') |> filter(.id != 13) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

arima_prob_pred <- read_rds('data/predictions/arima_prob_emp.rds') |> filter(.id != 13) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98)

arima_prob_reg_pred <- read_rds('data/predictions/arima_prob_reg_emp.rds') |> filter(.id != 13) |> 
  select(-c(censorship_type, censorship_type_disruption, censorship_type_censored, censorship_type_none, id)) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |> 
  mutate(model = 'arima_reg') |> 
  select(-q97_5) |> 
  rename(upper_q97.5 = upper_q97_5) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) 

lgbm_prob_pred <- read_rds('data/predictions/lgbm_prob_emp.rds') |> filter(.id != 13) |> 
  mutate(date = yearmonth(date))|> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |>
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98) |> 
  mutate(model = 'lgbm')

lgbm_prob_reg_pred <- read_rds('data/predictions/lgbm_prob_reg_emp.rds') |> filter(.id != 13) |> 
  mutate(date = yearmonth(date))|> 
  select(-matches("^q[0-9]+$")) |> 
  rename_with(
    .cols = starts_with("upper_"),
    .fn   = ~ str_replace(., "^upper_", "upper_q")
  ) |>
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code')) |> 
  rename(upper_q97.5 = upper_q98) |> 
  mutate(model = 'lgbm_reg')

timegpt <- read_rds('data/predictions/timegpt_prob_emp.rds') |> 
  separate(unique_id, into = c('site_code', 'product_code', '.id'), sep ='-') |> 
  rename(prediction = TimeGPT,
         date = ds) |> 
  mutate(date = yearmonth(date),
         .id = as.integer(.id)) |> 
  select(-starts_with("TimeGPT-lo")) |> 
  rename_with(
    ~ str_replace(., "TimeGPT-hi-(\\d+)", "\\1%_upper"),
    starts_with("TimeGPT-hi")
  ) |> 
  filter(.id != 13) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code'))


timegpt_reg <- read_rds('data/predictions/timegpt_prob_reg_emp.rds') |> 
  separate(unique_id, into = c('site_code', 'product_code', '.id'), sep ='-') |> 
  rename(prediction = TimeGPT,
         date = ds) |> 
  mutate(date = yearmonth(date),
         .id = as.integer(.id)) |> 
  select(-starts_with("TimeGPT-lo")) |> 
  rename_with(
    ~ str_replace(., "TimeGPT-hi-(\\d+)", "\\1%_upper"),
    starts_with("TimeGPT-hi")
  ) |> 
  filter(.id != 13) |> 
  left_join(tckf_pred |> 
              select(date, site_code, product_code, stock_distributed, censorship_type),
            by = c('date', 'site_code', 'product_code'))

# bind dfs ---------------------------------------------------------------

pred_master <- timegpt |> 
  bind_rows(timegpt_reg) |> 
  rename(upper_q95 = `95%_upper`,
         upper_q90 = `90%_upper`,
         upper_q85 = `85%_upper`,
         upper_q80 = `80%_upper`,
         upper_q97.5 = `97%_upper.5`) |> 
  bind_rows(ma_prob_pred, ma_prob_reg_pred, ets_prob_pred, arima_prob_pred, arima_prob_reg_pred,
            lr_prob_pred, lr_prob_reg_pred,
            tckf_pred, lgbm_prob_pred, lgbm_prob_reg_pred, sba_prob_pred, naive_prob_pred) |> 
  mutate(
    across(
      .cols   = -date,    
      .fns    = ~ replace(.x, .x < 1, 0),
      .names  = "{.col}"
    )
  ) |> 
  drop_na() |> 
  select(-censorship_type) |> 
  left_join(s_data_test_tscv |> 
              as_tibble() |> 
              select(date, site_code, product_code, censorship_type) |> 
              distinct(),
            by = c('date', 'site_code', 'product_code'))



# add real demand plan ----------------------------------------------------

# real_fc <- stock_tsb |> 
#   filter_index('2018 Dec' ~ .) |> 
#   as_tibble() |> 
#   select(date, site_code, product_code, projected_average_monthly_consumption, corrected_cmm, stock_distributed, censorship_type) |>
#   rename(demand_planner = corrected_cmm, system_gen = projected_average_monthly_consumption) |> 
#   group_by(site_code, product_code) |> 
#   mutate(demand_planner = lag(demand_planner, n =1),
#          system_gen = lag(system_gen, n = 1)) |>
#   ungroup() |> 
#   pivot_longer(cols = c('demand_planner', 'system_gen'), names_to = 'model', values_to = 'prediction') |> 
#   group_by(site_code, product_code, model) |> 
#   mutate(.id = row_number()-1) |>
#   drop_na() |> 
#   ungroup()

# # crete quantiles

# resid_tbl <- stock_tsb |> 
#   filter_index('2018 Dec' ~ .) |> 
#   as_tibble() |> 
#   select(date, site_code, product_code, projected_average_monthly_consumption, corrected_cmm, stock_distributed, censorship_type) |>
#   rename(demand_planner = corrected_cmm, system_gen = projected_average_monthly_consumption) |> 
#   # group_by(site_code, product_code) |> 
#   # mutate(demand_planner = lag(demand_planner, n =1),
#   #        system_gen = lag(system_gen, n = 1)) |>
#   # ungroup() |> 
#   pivot_longer(cols = c('demand_planner', 'system_gen'), names_to = 'model', values_to = 'prediction') |> 
#   # mutate(.resid = abs(stock_distributed - prediction)) |> 
#   mutate(.resid = pmax(stock_distributed - prediction, 0)) |>
#   filter(!is.na(.resid) & censorship_type == 'none')  |> 
#   as_tibble() |> 
#   select(site_code, product_code, model, .resid)

# quant_tbl <- resid_tbl |>
#   group_by(site_code, product_code, model) |>
#   summarise(.resid = list(.resid), .groups = "drop") |>
#   mutate(probs = list(c(0.80, 0.85, 0.90, 0.95, 0.975))) |>
#   rowwise() |>
#   mutate(
#     q_tbl = list(tibble(
#       probs = probs,
#       qval = quantile(.resid, probs = probs, na.rm = TRUE)
#     ))
#   ) |>
#   select(-.resid, -probs) |>
#   unnest(q_tbl) |>
#   mutate(qname = paste0("q", round(probs,2) * 100)) |>
#   select(-probs) |>
#   pivot_wider(names_from = qname, values_from = qval)


real_fc_base <- stock_tsb |> 
  filter_index('2018 Dec' ~ .) |> 
  as_tibble() |> 
  select(
    date, site_code, product_code,
    projected_average_monthly_consumption,
    corrected_cmm,
    stock_distributed,
    censorship_type
  ) |>
  rename(
    demand_planner = corrected_cmm,
    system_gen = projected_average_monthly_consumption
  ) |>
  group_by(site_code, product_code) |> 
  mutate(
    demand_planner = lag(demand_planner, 1),
    system_gen = lag(system_gen, 1)
  ) |>
  ungroup() |> 
  drop_na()


real_fc <- real_fc_base |>
  pivot_longer(
    cols = c(demand_planner, system_gen),
    names_to = "model",
    values_to = "prediction"
  ) |>
  group_by(site_code, product_code, model) |>
  mutate(.id = row_number() - 1) |>
  ungroup() |>
  mutate(prediction = replace_na(prediction, 0)) 

resid_tbl <- real_fc_base |>
  pivot_longer(
    cols = c(demand_planner, system_gen),
    names_to = "model",
    values_to = "prediction"
  ) |>
  filter(!is.na(prediction), censorship_type == "none") |>
  mutate(
    .resid = case_when(
      model == "demand_planner" ~ abs(stock_distributed - prediction),
      model == "system_gen" ~ pmax(stock_distributed - prediction, 0)
    ) 
  ) |> 
  select(site_code, product_code, model, .resid)

quant_tbl <- resid_tbl |>
  group_by(site_code, product_code, model) |>
  summarise(.resid = list(.resid), .groups = "drop") |>
  mutate(probs = list(c(0.80, 0.85, 0.90, 0.95, 0.975))) |>
  rowwise() |>
  mutate(
    q_tbl = list(tibble(
      probs = probs,
      qval = quantile(.resid, probs = probs, na.rm = TRUE)
    ))
  ) |>
  select(-.resid, -probs) |>
  unnest(q_tbl) |>
  mutate(qname = paste0("q", round(probs,2) * 100)) |>
  select(-probs) |>
  pivot_wider(names_from = qname, values_from = qval)

real_fc_bs <- real_fc |> 
  left_join(quant_tbl, by = c("site_code","product_code", 'model')) |> 
  mutate(across(
    matches("^q\\d+$"),
    ~ pmax(0, prediction + .x),
    .names = "upper_q{str_extract(.col, '\\\\d+')}"
  )) |> 
  select(-matches("^q[0-9]+$")) |> 
  rename(upper_q97.5 = upper_q98)

pred_master <- pred_master |> 
  bind_rows(real_fc_bs) |> 
  mutate(prediction = replace_na(prediction, 0))

write_rds(pred_master, 'data/predictions/pred_master_emp.rds')


# point accuracy calculation ----------------------------------------------

fc_point_accuracy <- pred_master |> 
  filter(censorship_type == 'none') |> 
  group_by(site_code, product_code, model) |> 
  summarise(
    # mase = mase(stock_distributed, prediction, 1),
    rmse = rmse(stock_distributed, prediction),
    .groups = "drop"
  )

fc_point_accuracy |> 
  group_by(model) |> 
  summarise(
    # mase = mean(mase),
    rmse = mean(rmse),
    .groups = "drop"
  )


write_rds(fc_point_accuracy, 'data/results/emp_ex_point_acc.rds')

## rRMSE calculation - origin

fc_point_accuracy_origin <- pred_master |>
  filter(censorship_type == 'none') |>
  group_by(.id, date, site_code, product_code, model) |>
  summarise(
    rmse = round(rmse(stock_distributed, prediction), 1),
    .groups = "drop"
  ) 

fc_rel_accuracy_origin <- fc_point_accuracy_origin |> 
  left_join(fc_point_accuracy_origin |> 
              filter(model == 'tckf') |> 
              select(.id, site_code, product_code, rmse) |> 
              rename(rmse_bench = rmse),
            by = c('.id', 'site_code', 'product_code')) |> 
  mutate(
    rel_rmse = case_when(
      rmse_bench == 0 ~ 1,
      TRUE ~ rmse / rmse_bench
    ),
    fva = 1 - rel_rmse
  )

fc_rel_accuracy_origin |> 
  group_by(model) |> 
  summarise(
    # mase = mean(mase),
    fva = mean(fva, na.rm = TRUE),
    .groups = "drop"
  )


write_rds(fc_rel_accuracy_origin, 'data/results/emp_ex_point_acc_origin.rds')

# pinball score -----------------------------------------------------------

# function
pinball <- function(q, y, a) {
  if_else(y < q,
          2*(1 - a) * (q - y),
          2 *a * (y - q))
}

fc_pinball_origin <- pred_master |>
  filter(censorship_type == 'none') |>
  group_by(.id, date, site_code, product_code, model) |> 
  summarise(
    across(
      starts_with("upper_q"),
      ~ mean(
        pinball(
          .x,
          stock_distributed,
          as.numeric(str_remove(cur_column(), "^upper_q")) / 100
        ),
        na.rm = TRUE
      ),
      .names = "qs{str_remove(.col, '^upper_q')}"  
    ),
    .groups = "drop"
  )  |> 
  mutate(
    across(
      starts_with("qs"),
      ~ ifelse(. > 0 & . < 1 & model == "tckf", 0.1, .)
    )
  )

# calculating skill scores

fc_skill_pinball_origin <- fc_pinball_origin |> 
  left_join(fc_pinball_origin |> 
              filter(model == 'tckf') |> 
              select(-model) |> 
              rename(qs80_bench = qs80,
                     qs85_bench = qs85,
                     qs90_bench = qs90,
                     qs95_bench = qs95,
                     qs97.5_bench = qs97.5),
            by = c('.id', 'date', 'site_code', 'product_code')) |> 
  mutate(qs80_sc = (qs80_bench - qs80)/qs80_bench,
         qs85_sc = (qs85_bench - qs85)/qs85_bench,
         qs90_sc = (qs90_bench - qs90)/qs90_bench,
         qs95_sc = (qs95_bench - qs95)/qs95_bench,
         qs97.5_sc = (qs97.5_bench - qs97.5)/qs97.5_bench) |> 
  filter(
    if_all(c(qs80_sc, qs85_sc, qs90_sc, qs95_sc, qs97.5_sc),
           ~ !is.na(.) & !is.nan(.) & is.finite(.))
  )


write_rds(fc_skill_pinball_origin, 'data/results/emp_ex_pinball_origin.rds')

fc_skill_pinball_origin |> 
  group_by(model) |> 
  summarise(
    qs80_sc = mean(qs80_sc),
    qs85_sc = mean(qs85_sc),
    qs90_sc = mean(qs90_sc),
    qs95_sc = mean(qs95_sc),
    qs97.5_sc = mean(qs97.5_sc),
    .groups = "drop"
  )
















