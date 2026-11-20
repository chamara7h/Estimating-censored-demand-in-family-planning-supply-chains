# Forecast models accuracy - numerical experiment

library(tidyverse)
library(tsibble)
library(Metrics)
library(SpecsVerification)
library(tsutils)
library(feasts)

# Load data --------------------------------------------------------

s_data <- read_rds("data/tidy_data/synthetic_data.rds") |> 
  mutate(censorship_type = as.factor(censorship_type)) |> 
  # filter(series_category == 'correlated', store == 'S1', product == 'P10') |> 
  fastDummies::dummy_cols(select_columns = "censorship_type") |> 
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
  mutate(period = row_number(),
         id = paste0(series_category,'-',series_type,'-',store, product,'-',.id)) 

# pred data

tckf_pred <- read_rds('data/predictions/tckf_pred_pom.rds') |>
  select(-id, -censorship_type) |> 
  rename(prediction = yhat)

naive_prob_pred <- read_rds('data/predictions/naive_prob.rds') |> filter(.id != 31) |> 
  select(-c(q95, q90, q85, q80, q98)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

sba_prob_pred <- read_rds('data/predictions/sba_prob.rds') |> filter(.id != 31) |> 
  select(-c(q95, q90, q85, q80, q97_5, observed_demand)) |> 
  mutate(model = 'sba') |> 
  rename(upper_q97.5 = upper_97_5,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

ma_prob_reg_pred <- read_rds('data/predictions/ma_prob_reg.rds') |> filter(.id != 31) |> 
  mutate(model = 'moving_average_reg') |>
  select(-c(q95, q90, q85, q80, q98, date)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, .id, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('.id', 'series_category', 'series_type', 'store', 'product'))

ma_prob_pred <- read_rds('data/predictions/ma_prob.rds') |> filter(.id != 31) |> 
  select(-c(q95, q90, q85, q80, q98)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

lr_prob_reg_pred <- read_rds('data/predictions/lr_prob.rds') |> 
  select(-c(censorship_type_censored, censorship_type_disruption, censorship_type_none, period, id,
            q95, q90, q85, q80, q98, actual_demand)) |> 
  mutate(model = 'lr_reg') |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

lr_prob_pred <- read_rds('data/predictions/lr_prob_reg.rds') |> 
  select(-c(q95, q90, q85, q80, q98)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

ets_prob_pred <- read_rds('data/predictions/ets_prob.rds') |> filter(.id != 31) |> 
  select(-c(q95, q90, q85, q80, q98)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

arima_prob_pred <- read_rds('data/predictions/arima_prob.rds') |> filter(.id != 31) |> 
  select(-c(q95, q90, q85, q80, q97_5)) |> 
  rename(upper_q97.5 = upper_97_5,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

arima_prob_reg_pred <- read_rds('data/predictions/arima_prob_reg.rds') |> filter(.id != 31) |> 
  select(-c(censorship_type, censorship_type_none, censorship_type_disruption, censorship_type_censored, period, id, actual_demand)) |> 
  mutate(model = 'arima_reg') |> 
  select(-c(q95, q90, q85, q80, q97_5)) |> 
  rename(upper_q97.5 = upper_97_5,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))

lgbm_prob_pred <- read_rds('data/predictions/lgbm_prob.rds') |> filter(.id != 31) |> 
  mutate(date = yearmonth(date),
         model = 'lgbm')|> 
  select(-c(q95, q90, q85, q80, q98)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'store', 'product'))

lgbm_prob_reg_pred <- read_rds('data/predictions/lgbm_prob_reg.rds') |> filter(.id != 31) |> 
  mutate(date = yearmonth(date),
         model = 'lgbm_reg')|> 
  select(-c(q95, q90, q85, q80, q98)) |> 
  rename(upper_q97.5 = upper_98,
         upper_q95 = upper_95,
         upper_q90 = upper_90,
         upper_q85 = upper_85,
         upper_q80 = upper_80) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'store', 'product'))

timegpt <- read_rds('data/predictions/timegpt_prob.rds') |> 
  separate(unique_id, into = c('series_category', 'series_type', 'store', 'product', '.id'), sep ='-') |> 
  rename(prediction = TimeGPT,
         date = ds) |> 
  mutate(date = yearmonth(date),
         .id = as.integer(.id)) |> 
  select(-starts_with("TimeGPT-lo")) |> 
  rename_with(
    ~ str_replace(., "TimeGPT-hi-(\\d+)", "\\1%_upper"),
    starts_with("TimeGPT-hi")
  ) |> 
  filter(.id != 31) |> 
  rename(upper_q97.5 = `97%_upper.5`,
         upper_q95 = `95%_upper`,
         upper_q90 = `90%_upper`,
         upper_q85 = `85%_upper`,
         upper_q80 = `80%_upper`) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))
  
  
timegpt_reg <- read_rds('data/predictions/timegpt_prob_reg.rds') |> 
  separate(unique_id, into = c('series_category', 'series_type', 'store', 'product', '.id'), sep ='-') |> 
  rename(prediction = TimeGPT,
         date = ds) |> 
  mutate(date = yearmonth(date),
         .id = as.integer(.id)) |> 
  select(-starts_with("TimeGPT-lo")) |> 
  rename_with(
    ~ str_replace(., "TimeGPT-hi-(\\d+)", "\\1%_upper"),
    starts_with("TimeGPT-hi")
  ) |> 
  filter(.id != 31) |> 
  rename(upper_q97.5 = `97%_upper.5`,
         upper_q95 = `95%_upper`,
         upper_q90 = `90%_upper`,
         upper_q85 = `85%_upper`,
         upper_q80 = `80%_upper`) |> 
  left_join(tckf_pred |> 
              select(date, series_category, series_type, store, product, actual_demand, observed_demand),
            by = c('date', 'series_category', 'series_type', 'store', 'product'))
  
# bind dfs ---------------------------------------------------------------

pred_master <- naive_prob_pred |> 
  bind_rows(ma_prob_pred, ma_prob_reg_pred, ets_prob_pred, arima_prob_pred, arima_prob_reg_pred,
            lr_prob_pred, lr_prob_reg_pred, timegpt, timegpt_reg,
            tckf_pred, lgbm_prob_pred, lgbm_prob_reg_pred, sba_prob_pred) |> 
  mutate(prediction = if_else(prediction < 0, 0, prediction),
         prediction = if_else(is.na(prediction), 0, prediction))

write_rds(pred_master, 'data/predictions/pred_master.rds')

# point accuracy calculation ----------------------------------------------

fc_point_accuracy <- pred_master |> 
  group_by(series_category, series_type, store, product, model) |> 
  summarise(
    mase = mase(actual_demand, prediction, 1),
    rmse = rmse(actual_demand, prediction),
    .groups = "drop"
  )

fc_point_accuracy |> 
  group_by(model) |> 
  summarise(
    mase = mean(mase),
    rmse = mean(rmse),
    .groups = "drop"
  )


write_rds(fc_point_accuracy, 'data/results/num_ex_point_acc.rds')

## rRMSE calculation - origin

fc_point_accuracy_origin <- pred_master |>
  group_by(.id, date, store, product, model) |>
  summarise(
    rmse = rmse(actual_demand, prediction),
    .groups = "drop"
  ) 

fc_rel_accuracy_origin <- fc_point_accuracy_origin |> 
  left_join(fc_point_accuracy_origin |> 
              filter(model == 'tckf') |> 
              select(.id, store, product, rmse) |> 
              rename(rmse_bench = rmse) |> 
              mutate(rmse_bench = if_else(rmse_bench < 1, 0, rmse_bench)),
            by = c('.id', 'store', 'product')) |> 
  mutate(
    rel_rmse = case_when(
      rmse_bench == 0      ~ NA_real_,
      TRUE                 ~ rmse / rmse_bench
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


write_rds(fc_rel_accuracy_origin, 'data/results/num_ex_point_acc_origin.rds')

# pinball score -----------------------------------------------------------

# function
pinball <- function(q, y, a) {
  if_else(y < q,
          2*(1 - a) * (q - y),
          2 *a * (y - q))
}

fc_pinball_origin <- pred_master |>
  group_by(.id, date, store, product, model) |> 
  summarise(
    qs80 = mean(pinball(upper_q80, actual_demand, 0.80), na.rm = TRUE),
    qs85 = mean(pinball(upper_q85, actual_demand, 0.85), na.rm = TRUE),
    qs90 = mean(pinball(upper_q90, actual_demand, 0.90), na.rm = TRUE),
    qs95 = mean(pinball(upper_q95, actual_demand, 0.95), na.rm = TRUE),
    qs97.5 = mean(pinball(upper_q97.5, actual_demand, 0.975), na.rm = TRUE),
    .groups = "drop"
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
            by = c('.id', 'date', 'store', 'product')) |> 
  mutate(qs80_sc = (qs80_bench - qs80)/qs80_bench,
         qs85_sc = (qs85_bench - qs85)/qs85_bench,
         qs90_sc = (qs90_bench - qs90)/qs90_bench,
         qs95_sc = (qs95_bench - qs95)/qs95_bench,
         qs97.5_sc = (qs97.5_bench - qs97.5)/qs97.5_bench)

write_rds(fc_skill_pinball_origin, 'data/results/num_ex_pinball_origin.rds')

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



