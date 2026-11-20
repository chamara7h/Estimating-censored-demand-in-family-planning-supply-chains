library(tidyverse)

# read data ---------------------------------------------------------------

inv_path <- if (file.exists("data/results/inv_df_emp_leadtime_sensitivity.rds")) {
  "data/results/inv_df_emp_leadtime_sensitivity.rds"
} else {
  "../../data/results/inv_df_emp_leadtime_sensitivity.rds"
}

metrics_path <- if (file.exists("data/results/metrics_inv_emp_leadtime_sensitivity.rds")) {
  "data/results/metrics_inv_emp_leadtime_sensitivity.rds"
} else {
  "../../data/results/metrics_inv_emp_leadtime_sensitivity.rds"
}

inv_df_emp_lt <- read_rds(inv_path) |>
  mutate(
    model = case_when(
      model == "tckf" ~ "TCKF",
      model == "demand_planner" ~ "Demand Planner",
      model == "system_gen" ~ "System Generated",
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
    scenario = recode(
      scenario,
      "lead_time_1" = "Lead time = 1",
      "lead_time_2" = "Lead time = 2",
      "lead_time_3" = "Lead time = 3",
      "lead_time_mix_123" = "Lead time mix {1,2,3}"
    ),
    censorship_type = as.character(censorship_type)
  ) |>
  filter(
    date != as.Date("2019-01-01"),
    censorship_type == "none",
    !policy %in% c("Capped", "Oracle")
  )

metrics_emp_lt <- read_rds(metrics_path) |>
  mutate(
    model = case_when(
      model == "tckf" ~ "TCKF",
      model == "demand_planner" ~ "Demand Planner",
      model == "system_gen" ~ "System Generated",
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
    scenario = recode(
      scenario,
      "lead_time_1" = "Lead time = 1",
      "lead_time_2" = "Lead time = 2",
      "lead_time_3" = "Lead time = 3",
      "lead_time_mix_123" = "Lead time mix {1,2,3}"
    )
  )


# plot styling ------------------------------------------------------------

model_colour_values <- c(
  "TCKF" = "#2c3e50",
  "System Generated" = "#e67e22",
  "Demand Planner" = "#568203"
)

model_shape_values <- c(
  "TCKF" = 1,
  "System Generated" = 2,
  "Demand Planner" = 3
)

policy_levels <- c(
  "Deterministic", "Myopic (k=5)", "Myopic (k=10)", "Myopic (k=20)",
  "Quantile 80", "Quantile 85", "Quantile 90", "Quantile 95",
  "Capped", "Oracle"
)

scenario_levels <- c(
  "Lead time = 1",
  "Lead time = 2",
  "Lead time = 3",
  "Lead time mix {1,2,3}"
)

policy_shape_values <- c(
  "Deterministic" = 16,
  "Myopic (k=5)" = 17,
  "Myopic (k=10)" = 15,
  "Myopic (k=20)" = 18,
  "Quantile 80" = 3,
  "Quantile 85" = 7,
  "Quantile 90" = 8,
  "Quantile 95" = 0,
  "Capped" = 1,
  "Oracle" = 2
)

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


# summary table -----------------------------------------------------------

plot_df <- inv_df_emp_lt |>
  group_by(scenario, policy, model) |>
  summarise(
    opening_mos = sum(opening_stock, na.rm = TRUE) / sum(demand, na.rm = TRUE),
    csl = mean(met_demand, na.rm = TRUE),
    stockout_rate = mean(!met_demand, na.rm = TRUE),
    lost_demand_rate = sum(lost_sales, na.rm = TRUE) / sum(demand, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(scenario, policy) |>
  mutate(
    stockout_rate_bench = stockout_rate[model == "TCKF"][1],
    mcpr_loss = 0.065 * 9 * (stockout_rate - stockout_rate_bench)
  ) |>
  ungroup() |>
  mutate(
    policy = factor(policy, levels = policy_levels),
    scenario = factor(scenario, levels = scenario_levels),
    model = factor(model, levels = c("TCKF", "System Generated", "Demand Planner"))
  )

summary_tbl <- plot_df |>
  transmute(
    scenario,
    policy,
    model,
    opening_mos = round(opening_mos, 2),
    csl = round(csl, 3),
    lost_demand_rate = round(lost_demand_rate, 3),
    mcpr_loss = round(mcpr_loss, 3)
  )


# lead-time degradation plots --------------------------------------------

ggplot(plot_df, aes(x = scenario, y = lost_demand_rate, group = model)) +
  geom_line(aes(colour = model), linewidth = 0.75, alpha = 0.9) +
  geom_point(aes(colour = model, shape = model), size = 2.6, alpha = 0.95) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free_y") +
  labs(
    x = "Lead-time scenario",
    y = "Lost-demand rate",
    colour = "Method",
    shape = "Method"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

ggplot(plot_df, aes(x = scenario, y = csl, group = model)) +
  geom_line(aes(colour = model), linewidth = 0.75, alpha = 0.9) +
  geom_point(aes(colour = model, shape = model), size = 2.6, alpha = 0.95) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free_y") +
  labs(
    x = "Lead-time scenario",
    y = "Achieved CSL",
    colour = "Method",
    shape = "Method"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

ggplot(plot_df, aes(x = scenario, y = opening_mos, group = model)) +
  geom_line(aes(colour = model), linewidth = 0.75, alpha = 0.9) +
  geom_point(aes(colour = model, shape = model), size = 2.6, alpha = 0.95) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free_y") +
  labs(
    x = "Lead-time scenario",
    y = "Opening months of stock",
    colour = "Method",
    shape = "Method"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

# main operational frontier ----------------------------------------------

ggplot(plot_df, aes(x = opening_mos, y = lost_demand_rate)) +
  geom_point(aes(colour = model, shape = policy), size = 2.6, alpha = 0.9) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = policy_shape_values) +
  facet_wrap(~scenario, ncol = 2, scales = "free") +
  labs(
    x = "Opening months of stock",
    y = "Lost-demand rate",
    colour = "Method",
    shape = "Inventory policy"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme


# public health translation ----------------------------------------------

ggplot(plot_df, aes(x = opening_mos, y = mcpr_loss)) +
  geom_hline(yintercept = 0, colour = "grey65", linetype = "dashed", linewidth = 0.35) +
  geom_point(aes(colour = model, shape = policy), size = 2.6, alpha = 0.9) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = policy_shape_values) +
  facet_wrap(~scenario, ncol = 2, scales = "free") +
  labs(
    x = "Opening months of stock",
    y = "Relative mCPR loss",
    colour = "Method",
    shape = "Inventory policy"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme


# CSL heatmap -------------------------------------------------------------

heatmap_df <- metrics_emp_lt |>
  group_by(scenario, policy, model) |>
  summarise(
    csl = mean(CSL, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    policy = factor(policy, levels = policy_levels),
    scenario = factor(scenario, levels = scenario_levels),
    model = factor(model, levels = c("TCKF", "System Generated", "Demand Planner"))
  )

ggplot(heatmap_df, aes(x = policy, y = scenario, fill = csl)) +
  geom_tile(colour = "white", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%.3f", csl)), size = 3, colour = "#1a2b48") +
  scale_fill_viridis_c(option = "viridis", end = 0.95) +
  facet_wrap(~model, ncol = 1) +
  labs(
    x = "Inventory policy",
    y = "Lead-time scenario",
    fill = "CSL"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  ) +
  plot_theme
