# Inventory value analysis

library(tidyverse)
library(scales)


# read data

inv_path <- if (file.exists("data/results/inv_df_emp_revised.rds")) {
  "data/results/inv_df_emp_revised.rds"
} else {
  "../../data/results/inv_df_emp_revised.rds"
}

inv_df_emp <- read_rds(inv_path) |>
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
      model == "demand_planner" ~ "Demand Planner",
      model == "system_gen" ~ "System Generated",
      model == "site_system" ~ "Site ERP",
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
      "upper_q95" = "Quantile 95"
    ),
    censorship_type = as.character(censorship_type)
  ) |>
  filter(
    model != "Site ERP",
    date != as.Date("2019-01-01"),
    censorship_type == "none",
    !policy %in% c("oracle", "capped")
  )

price_tbl <- tribble(
  ~product_code, ~proxy_price_usd,
  "AS27000", 0.220,
  "AS27132", 0.288,
  "AS27133", 0.637,
  "AS27134", 0.637,
  "AS27138", 10.500,
  "AS27137", 8.520,
  "AS42018", 0.393,
  "AS46000", 0.030,
  "AS17005", 0.490
)

plot_df <- inv_df_emp |>
  left_join(price_tbl, by = "product_code") |>
  group_by(policy, model) |>
  summarise(
    opening_mos = sum(opening_stock, na.rm = TRUE) / sum(demand, na.rm = TRUE),
    average_mos = sum((opening_stock + closing_stock) / 2, na.rm = TRUE) / sum(demand, na.rm = TRUE),
    csl = mean(met_demand, na.rm = TRUE),
    stockout_rate = mean(!met_demand, na.rm = TRUE),
    lost_sales_units = sum(lost_sales, na.rm = TRUE),
    lost_demand_rate = mean(lost_sales, na.rm = TRUE) / mean(demand, na.rm = TRUE),
    total_inventory_value = sum(proxy_price_usd * opening_stock, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(policy) |>
  mutate(
    stockout_rate_bench = stockout_rate[model == "TCKF"][1],
    mcpr_loss = 0.065 * 9 * (stockout_rate - stockout_rate_bench)
  ) |>
  ungroup() |>
  mutate(
    policy = factor(
      policy,
      levels = c("Deterministic", "Myopic (k=5)", "Myopic (k=10)", "Myopic (k=20)",
                 "Quantile 80", "Quantile 85", "Quantile 90", "Quantile 95")
    ),
    model_group = case_when(
      model == "TCKF" ~ "TCKF",
      model == "Demand Planner" ~ "Demand Planner",
      model == "System Generated" ~ "System Generated",
      .default = "Other"
    )
  )

model_colour_values <- c(
  "TCKF" = "#2D68C4",
  "Demand Planner" = "#6d8196",
  "System Generated" = "#ffb343",
  "Other" = "grey72"
)

shape_pool <- c(16, 17, 15, 18, 3, 7, 8, 0, 1, 2, 4, 5, 6, 9, 10, 12, 13, 14)
model_levels <- plot_df |>
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
    plot.title = element_text(colour = "#1a2b48")
  )

# Inventory summary

summary_tbl <- plot_df |>
  transmute(
    policy,
    model,
    opening_mos = round(opening_mos, 2),
    average_mos = round(average_mos, 2),
    csl = round(csl, 3),
    lost_demand_rate = round(lost_demand_rate, 3),
    mcpr_loss = round(mcpr_loss, 3),
    total_inventory_value = round(total_inventory_value, 0)
  )


# Main operational frontier using lost-demand rate and opening months of stock

frontier_lost_demand <- plot_df |>
  group_by(policy) |>
  mutate(
    frontier = !purrr::map_lgl(
      seq_len(n()),
      ~ any(
        opening_mos <= opening_mos[.x] &
          lost_demand_rate <= lost_demand_rate[.x] &
          (opening_mos < opening_mos[.x] | lost_demand_rate < lost_demand_rate[.x]),
        na.rm = TRUE
      )
    )
  ) |>
  ungroup()

frontier_path <- frontier_lost_demand |>
  filter(frontier) |>
  arrange(policy, opening_mos, lost_demand_rate)

ggplot(frontier_lost_demand, aes(x = opening_mos, y = lost_demand_rate)) +
  geom_path(
    data = frontier_path,
    aes(group = policy),
    colour = "black",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_point(aes(colour = model_group, shape = model), size = 2.4, alpha = 0.9) +
  geom_point(
    data = frontier_lost_demand |> filter(frontier),
    shape = 21,
    size = 3.6,
    stroke = 0.8,
    fill = NA,
    colour = "#1a2b48",
    show.legend = FALSE
  ) +
  geom_text(
    data = frontier_lost_demand |> filter(model_group != "Other"),
    aes(label = model, colour = model_group),
    check_overlap = TRUE,
    nudge_y = 0.02 * max(frontier_lost_demand$lost_demand_rate, na.rm = TRUE),
    size = 2.9,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Opening months of stock",
    y = "Lost-demand rate",
    colour = "Highlighted methods",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme


# Achieved CSL versus opening months of stock

ggplot(plot_df, aes(x = opening_mos, y = csl)) +
  geom_vline(xintercept = 3, colour = "grey65", linetype = "dashed", linewidth = 0.35) +
  geom_point(aes(colour = model_group, shape = model), size = 2.4, alpha = 0.9) +
  geom_text(
    data = plot_df |> filter(model_group != "Other"),
    aes(label = model, colour = model_group),
    check_overlap = TRUE,
    nudge_y = 0.004,
    size = 2.9,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Opening months of stock",
    y = "Achieved CSL",
    colour = "Highlighted methods",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme

# Public-health translation using relative mCPR loss and opening months of stock

frontier_mcpr <- plot_df |>
  group_by(policy) |>
  mutate(
    frontier = !purrr::map_lgl(
      seq_len(n()),
      ~ any(
        opening_mos <= opening_mos[.x] &
          mcpr_loss <= mcpr_loss[.x] &
          (opening_mos < opening_mos[.x] | mcpr_loss < mcpr_loss[.x]),
        na.rm = TRUE
      )
    )
  ) |>
  ungroup()

frontier_mcpr_path <- frontier_mcpr |>
  filter(frontier) |>
  arrange(policy, opening_mos, mcpr_loss)

ggplot(frontier_mcpr, aes(x = opening_mos, y = mcpr_loss)) +
  geom_path(
    data = frontier_mcpr_path,
    aes(group = policy),
    colour = "black",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_point(aes(colour = model_group, shape = model), size = 2.4, alpha = 0.9) +
  geom_point(
    data = frontier_mcpr |> filter(frontier),
    shape = 21,
    size = 3.6,
    stroke = 0.8,
    fill = NA,
    colour = "#1a2b48",
    show.legend = FALSE
  ) +
  geom_text(
    data = frontier_mcpr |> filter(model_group != "Other"),
    aes(label = model, colour = model_group),
    check_overlap = TRUE,
    nudge_y = 0.02 * max(abs(frontier_mcpr$mcpr_loss), na.rm = TRUE),
    size = 2.9,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Opening months of stock",
    y = "Relative mCPR loss",
    colour = "Highlighted methods",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme

# Supplementary valuation frontier using lost-demand rate and total opening inventory value.

frontier_lost_demand_value <- plot_df |>
  group_by(policy) |>
  mutate(
    frontier = !purrr::map_lgl(
      seq_len(n()),
      ~ any(
        total_inventory_value <= total_inventory_value[.x] &
          lost_demand_rate <= lost_demand_rate[.x] &
          (total_inventory_value < total_inventory_value[.x] | lost_demand_rate < lost_demand_rate[.x]),
        na.rm = TRUE
      )
    )
  ) |>
  ungroup()

frontier_lost_demand_value_path <- frontier_lost_demand_value |>
  filter(frontier) |>
  arrange(policy, total_inventory_value, lost_demand_rate)

ggplot(frontier_lost_demand_value, aes(x = total_inventory_value, y = lost_demand_rate)) +
  geom_path(
    data = frontier_lost_demand_value_path,
    aes(group = policy),
    colour = "black",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_point(aes(colour = model_group, shape = model), size = 2.4, alpha = 0.9) +
  geom_point(
    data = frontier_lost_demand_value |> filter(frontier),
    shape = 21,
    size = 3.6,
    stroke = 0.8,
    fill = NA,
    colour = "#1a2b48",
    show.legend = FALSE
  ) +
  geom_text(
    data = frontier_lost_demand_value |> filter(model_group != "Other"),
    aes(label = model, colour = model_group),
    check_overlap = TRUE,
    nudge_y = 0.02 * max(frontier_lost_demand_value$lost_demand_rate, na.rm = TRUE),
    size = 2.9,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  scale_x_continuous(labels = label_dollar()) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Total opening inventory value",
    y = "Lost-demand rate",
    colour = "Highlighted methods",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme

# Supplementary valuation diagnostic using achieved CSL and total opening inventory value.

ggplot(plot_df, aes(x = total_inventory_value, y = csl)) +
  geom_point(aes(colour = model_group, shape = model), size = 2.4, alpha = 0.9) +
  geom_text(
    data = plot_df |> filter(model_group != "Other"),
    aes(label = model, colour = model_group),
    check_overlap = TRUE,
    nudge_y = 0.004,
    size = 2.9,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = model_colour_values) +
  scale_shape_manual(values = model_shape_values) +
  scale_x_continuous(labels = label_dollar()) +
  facet_wrap(~policy, ncol = 4, scales = "free") +
  labs(
    x = "Total opening inventory value",
    y = "Achieved CSL",
    colour = "Highlighted methods",
    shape = "Method"
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 3, alpha = 1)),
    shape = guide_legend(order = 2, override.aes = list(size = 3, alpha = 1))
  ) +
  plot_theme


