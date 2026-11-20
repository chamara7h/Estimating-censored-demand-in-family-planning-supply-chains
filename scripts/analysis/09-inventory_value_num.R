library(tidyverse)

# read data ---------------------------------------------------------------

inv_path <- if (file.exists("data/results/inv_df_num.rds")) {
  "data/results/inv_df_num.rds"
} else {
  "../../data/results/inv_df_num.rds"
}

metrics_path <- if (file.exists("data/results/metrics_inv_num.rds")) {
  "data/results/metrics_inv_num.rds"
} else {
  "../../data/results/metrics_inv_num.rds"
}

inv_df_num <- read_rds(inv_path) |>
  mutate(
    model = case_when(
      model == "lr" ~ "Linear Regression",
      model == "lr_reg" ~ "Censored Linear Regression",
      model == "moving_average" ~ "Mean",
      model == "moving_average_reg" ~ "Censored Mean",
      model == "naive" ~ "Naive",
      model == "tckf" ~ "TCKF",
      model == "arima" ~ "ARIMA",
      model == "arima_reg" ~ "Censored ARIMA",
      model == "ets" ~ "ETS",
      model == "timegpt" ~ "TimeGPT",
      model == "timegpt_reg" ~ "Censored TimeGPT",
      model == "lgbm" ~ "LightGBM",
      model == "lgbm_reg" ~ "Censored LightGBM",
      model == "sba" ~ "Syntetos-Boylan Approx",
      .default = model
    ),
    policy = recode(
      policy,
      "deterministic" = "Deterministic",
      "myopic (k=5)" = "Myopic (k=5)",
      "myopic (k=10)" = "Myopic (k=10)",
      "myopic (k=20)" = "Myopic (k=20)",
      "upper_q80" = "Quantile 80",
      "upper_q85" = "Quantile 85",
      "upper_q90" = "Quantile 90",
      "upper_q95" = "Quantile 95",
      "capped" = "Capped",
      "oracle" = "Oracle"
    ),
    series_type = str_to_title(series_type),
    censorship_type = as.character(censorship_type)
  ) |>
  filter(!policy %in% c("Capped", "Oracle"))

metrics_num <- read_rds(metrics_path) |>
  mutate(
    model = case_when(
      model == "lr" ~ "Linear Regression",
      model == "lr_reg" ~ "Censored Linear Regression",
      model == "moving_average" ~ "Mean",
      model == "moving_average_reg" ~ "Censored Mean",
      model == "naive" ~ "Naive",
      model == "tckf" ~ "TCKF",
      model == "arima" ~ "ARIMA",
      model == "arima_reg" ~ "Censored ARIMA",
      model == "ets" ~ "ETS",
      model == "timegpt" ~ "TimeGPT",
      model == "timegpt_reg" ~ "Censored TimeGPT",
      model == "lgbm" ~ "LightGBM",
      model == "lgbm_reg" ~ "Censored LightGBM",
      model == "sba" ~ "Syntetos-Boylan Approx",
      .default = model
    ),
    policy = recode(
      policy,
      "deterministic" = "Deterministic",
      "myopic (k=5)" = "Myopic (k=5)",
      "myopic (k=10)" = "Myopic (k=10)",
      "myopic (k=20)" = "Myopic (k=20)",
      "upper_q80" = "Quantile 80",
      "upper_q85" = "Quantile 85",
      "upper_q90" = "Quantile 90",
      "upper_q95" = "Quantile 95",
      "capped" = "Capped",
      "oracle" = "Oracle"
    ),
    series_type = str_to_title(series_type)
  ) |>
  filter(!policy %in% c("Capped", "Oracle"))


# styling -----------------------------------------------------------------

policy_levels <- c(
  "Deterministic", "Myopic (k=5)", "Myopic (k=10)", "Myopic (k=20)",
  "Quantile 80", "Quantile 85", "Quantile 90", "Quantile 95"
)

model_colour_values <- c(
  "ARIMA" = "#9aa5b1",
  "Censored ARIMA" = "#b8c0c8",
  "ETS" = "#7b8ea8",
  "LightGBM" = "#8f7fb8",
  "Censored LightGBM" = "#b39ddb",
  "Linear Regression" = "#4f6d8a",
  "Censored Linear Regression" = "#7f9bb3",
  "Mean" = "#8c8c8c",
  "Censored Mean" = "#b0b0b0",
  "Naive" = "#a17c6b",
  "Syntetos-Boylan Approx" = "#c49a6c",
  "TCKF" = "#2D68C4",
  "TimeGPT" = "#3c9d9b",
  "Censored TimeGPT" = "#78c2c4"
)

shape_pool <- c(16, 17, 15, 18, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9)
model_levels <- inv_df_num |>
  distinct(model) |>
  arrange(model) |>
  pull(model)

model_shape_values <- setNames(shape_pool[seq_along(model_levels)], model_levels)

plot_theme <- theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    panel.border = element_rect(colour = "#6d8196", fill = NA, linewidth = 0.4),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold", colour = "#1a2b48"),
    axis.title.y = element_text(face = "bold", colour = "#1a2b48"),
    axis.text.x = element_text(colour = "#1a2b48"),
    axis.text.y = element_text(colour = "#1a2b48"),
    legend.position = "right",
    legend.title = element_text(face = "bold", colour = "#1a2b48"),
    legend.text = element_text(colour = "#1a2b48"),
    plot.title = element_text(colour = "#1a2b48"),
    strip.text = element_text(face = "bold", colour = "#1a2b48")
  )


# summary -----------------------------------------------------------------

plot_df <- inv_df_num |>
  group_by(policy, model) |>
  summarise(
    csl = mean(met_demand, na.rm = TRUE),
    lost_demand_rate = sum(lost_sales, na.rm = TRUE) / sum(demand, na.rm = TRUE),
    avg_inventory = mean((opening_stock + closing_stock) / 2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    policy = factor(policy, levels = policy_levels),
    model = factor(model, levels = model_levels)
  )

summary_tbl <- plot_df |>
  transmute(
    policy,
    model,
    csl = round(csl, 3),
    lost_demand_rate = round(lost_demand_rate, 3),
    avg_inventory = round(avg_inventory, 2)
  )


# achieved CSL vs average inventory ---------------------------------------

ggplot(plot_df, aes(x = avg_inventory, y = csl)) +
  geom_point(aes(colour = model, shape = model), size = 2.5, alpha = 0.9) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Average inventory",
    y = "Achieved CSL",
    colour = "Method",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme


# lost-demand rate vs average inventory -----------------------------------

ggplot(plot_df, aes(x = avg_inventory, y = lost_demand_rate)) +
  geom_point(aes(colour = model, shape = model), size = 2.5, alpha = 0.9) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Average inventory",
    y = "Lost-demand rate",
    colour = "Method",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme


# achieved CSL vs lost-demand rate ----------------------------------------

ggplot(plot_df, aes(x = lost_demand_rate, y = csl)) +
  geom_point(aes(colour = model, shape = model), size = 2.5, alpha = 0.9) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Lost-demand rate",
    y = "Achieved CSL",
    colour = "Method",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme

