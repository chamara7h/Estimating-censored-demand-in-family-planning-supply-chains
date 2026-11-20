# TCKF regime-separation diagnostic

library(tidyverse)
library(patchwork)


# read data ---------------------------------------------------------------

diag_path <- if (file.exists(
  "data/predictions/tckf_regime_separation_diag.rds"
)) {
  "data/predictions/tckf_regime_separation_diag.rds"
} else {
  "../../data/predictions/tckf_regime_separation_diag.rds"
}

tckf_regime_diag <- read_rds(diag_path) |>
  filter(fit_convergence == 0) |>
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "Short interruptions",
        "Moderate interruptions",
        "Persistent interruptions"
      )
    ),
    variant = factor(
      variant,
      levels = c("TCKF-Separated", "TCKF-Pooled")
    )
  )


# evaluation groups -------------------------------------------------------

overall_df <- tckf_regime_diag |>
  mutate(evaluation_period = "Overall")

after_stockout_df <- tckf_regime_diag |>
  filter(previous_regime == "censored") |>
  mutate(evaluation_period = "After stockout")

after_interruption_df <- tckf_regime_diag |>
  filter(previous_regime == "disruption") |>
  mutate(evaluation_period = "After interruption")

evaluation_df <- bind_rows(
  overall_df,
  after_stockout_df,
  after_interruption_df
) |>
  mutate(
    evaluation_period = factor(
      evaluation_period,
      levels = c("Overall", "After stockout", "After interruption")
    )
  )


# series-level and overall summaries -------------------------------------

regime_diagnostic_by_series <- evaluation_df |>
  group_by(scenario, evaluation_period, variant, id) |>
  summarise(
    n_forecasts = sum(!is.na(yhat) & !is.na(actual_demand)),
    n_intervals = sum(
      !is.na(lower_q90) & !is.na(upper_q90) & !is.na(actual_demand)
    ),
    mse = mean((yhat - actual_demand)^2, na.rm = TRUE),
    rmse = sqrt(mse),
    coverage_90 = if_else(
      n_intervals > 0,
      mean(
        actual_demand <= upper_q90,
        # actual_demand >= lower_q90 & actual_demand <= upper_q90,
        na.rm = TRUE
      ),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  filter(n_forecasts > 0)

regime_diagnostic_summary <- regime_diagnostic_by_series |>
  group_by(scenario, evaluation_period, variant) |>
  summarise(
    n_series = n(),
    n_series_coverage = sum(!is.na(coverage_90)),
    mean_mse = mean(mse, na.rm = TRUE),
    se_mse = sd(mse, na.rm = TRUE) / sqrt(n()),
    mean_rmse = mean(rmse, na.rm = TRUE),
    se_rmse = sd(rmse, na.rm = TRUE) / sqrt(n()),
    mean_coverage_90 = mean(coverage_90, na.rm = TRUE),
    se_coverage_90 = sd(coverage_90, na.rm = TRUE) /
      sqrt(sum(!is.na(coverage_90))),
    .groups = "drop"
  ) |>
  mutate(
    mse_lower = pmax(0, mean_mse - 1.96 * se_mse),
    mse_upper = mean_mse + 1.96 * se_mse,
    rmse_lower = pmax(0, mean_rmse - 1.96 * se_rmse),
    rmse_upper = mean_rmse + 1.96 * se_rmse,
    coverage_lower = pmax(0, mean_coverage_90 - 1.96 * se_coverage_90),
    coverage_upper = pmin(1, mean_coverage_90 + 1.96 * se_coverage_90)
  )

# Paired differences isolate the effect within the same numerical series.
regime_diagnostic_paired <- regime_diagnostic_by_series |>
  mutate(
    variant_key = recode(
      as.character(variant),
      "TCKF-Separated" = "separated",
      "TCKF-Pooled" = "pooled"
    )
  ) |>
  select(
    scenario, evaluation_period, id, variant_key,
    mse, rmse, coverage_90
  ) |>
  pivot_wider(
    names_from = variant_key,
    values_from = c(mse, rmse, coverage_90)
  ) |>
  drop_na(
    mse_separated, mse_pooled,
    rmse_separated, rmse_pooled
  ) |>
  mutate(
    mse_difference = mse_pooled - mse_separated,
    rmse_difference = rmse_pooled - rmse_separated,
    coverage_difference = coverage_90_separated - coverage_90_pooled
  )

regime_diagnostic_comparison <- regime_diagnostic_paired |>
  group_by(scenario, evaluation_period) |>
  summarise(
    n_series = n(),
    mean_mse_difference = mean(mse_difference),
    mean_rmse_difference = mean(rmse_difference),
    se_rmse_difference = sd(rmse_difference) / sqrt(n()),
    rmse_increase_percent = 100 * mean(rmse_difference) /
      mean(rmse_separated),
    mean_coverage_difference = mean(coverage_difference, na.rm = TRUE),
    se_coverage_difference = sd(coverage_difference, na.rm = TRUE) /
      sqrt(sum(!is.na(coverage_difference))),
    share_lower_rmse_separated = mean(rmse_separated < rmse_pooled),
    .groups = "drop"
  ) |>
  mutate(
    rmse_difference_lower = mean_rmse_difference - 1.96 * se_rmse_difference,
    rmse_difference_upper = mean_rmse_difference + 1.96 * se_rmse_difference,
    coverage_difference_lower = mean_coverage_difference -
      1.96 * se_coverage_difference,
    coverage_difference_upper = mean_coverage_difference +
      1.96 * se_coverage_difference
  )

summary_tbl <- regime_diagnostic_summary |>
  transmute(
    Scenario = scenario,
    `Evaluation period` = evaluation_period,
    Variant = variant,
    `Number of series` = n_series,
    MSE = round(mean_mse, 2),
    RMSE = round(mean_rmse, 2),
    `90% coverage` = round(mean_coverage_90, 3)
  )

# performance in the period immediately following each regime ------------

following_regime_by_series <- tckf_regime_diag |>
  filter(!is.na(previous_regime)) |>
  mutate(
    following_regime = recode(
      previous_regime,
      "none" = "After uncensored",
      "censored" = "After stockout",
      "disruption" = "After interruption"
    ),
    following_regime = factor(
      following_regime,
      levels = c(
        "After uncensored",
        "After stockout",
        "After interruption"
      )
    )
  ) |>
  group_by(scenario, following_regime, variant, id) |>
  summarise(
    n_forecasts = sum(!is.na(yhat) & !is.na(actual_demand)),
    mse = mean((yhat - actual_demand)^2, na.rm = TRUE),
    rmse = sqrt(mse),
    .groups = "drop"
  ) |>
  filter(n_forecasts > 0)

following_regime_summary <- following_regime_by_series |>
  group_by(scenario, following_regime, variant) |>
  summarise(
    n_series = n(),
    mean_mse = mean(mse, na.rm = TRUE),
    mean_rmse = mean(rmse, na.rm = TRUE),
    .groups = "drop"
  )

following_regime_tbl <- following_regime_summary |>
  transmute(
    Scenario = scenario,
    `Evaluation period` = following_regime,
    Variant = variant,
    `Number of series` = n_series,
    MSE = round(mean_mse, 2),
    RMSE = round(mean_rmse, 2)
  )

write_rds(
  regime_diagnostic_summary,
  "data/results/tckf_regime_separation_summary.rds"
)

write_rds(
  regime_diagnostic_comparison,
  "data/results/tckf_regime_separation_paired_comparison.rds"
)

write_rds(
  following_regime_summary,
  "data/results/tckf_regime_separation_following_regime_summary.rds"
)


# upper-quantile calibration ---------------------------------------------

quantile_levels <- c(0.80, 0.85, 0.90, 0.95)
quantile_names <- c(
  "upper_q80", "upper_q85", "upper_q90", "upper_q95"
)

# Use the same rolling absolute-error calibration as the existing 90% upper
# quantile, then evaluate only whether demand falls below the upper bound.
quantile_coverage_by_series <- tckf_regime_diag |>
  select(
    scenario, variant, id, date, censorship_type, previous_regime,
    actual_demand, yhat, conformal_window
  ) |>
  group_by(scenario, variant, id) |>
  group_modify(
    ~ {
      data <- .x |>
        arrange(date)

      upper_quantiles <- matrix(
        NA_real_,
        nrow = nrow(data),
        ncol = length(quantile_levels),
        dimnames = list(NULL, quantile_names)
      )

      residual_pool <- numeric(0)
      conformal_window <- first(data$conformal_window)

      for (t in seq_len(nrow(data))) {
        # Construct each quantile before adding the current forecast error.
        if (length(residual_pool) >= 10) {
          residual_window <- tail(residual_pool, conformal_window)

          upper_quantiles[t, ] <- pmax(
            0,
            data$yhat[t] + as.numeric(
              quantile(
                residual_window,
                probs = quantile_levels,
                na.rm = TRUE,
                type = 8
              )
            )
          )
        }

        if (data$censorship_type[t] == "none") {
          residual_pool <- c(
            residual_pool,
            abs(data$actual_demand[t] - data$yhat[t])
          )
        }
      }

      coverage_matrix <- upper_quantiles >= data$actual_demand

      evaluation_masks <- list(
        "Overall" = rep(TRUE, nrow(data)),
        "After stockout" = data$previous_regime %in% "censored",
        "After interruption" = data$previous_regime %in% "disruption"
      )

      map_dfr(
        names(evaluation_masks),
        function(period) {
          period_mask <- evaluation_masks[[period]]

          tibble(
            evaluation_period = period,
            quantile_level = quantile_levels,
            n_forecasts = map_int(
              seq_along(quantile_levels),
              ~ sum(
                period_mask & !is.na(upper_quantiles[, .x]) &
                  !is.na(data$actual_demand)
              )
            ),
            coverage = map_dbl(
              seq_along(quantile_levels),
              ~ {
                valid <- period_mask &
                  !is.na(upper_quantiles[, .x]) &
                  !is.na(data$actual_demand)

                if (sum(valid) == 0) {
                  return(NA_real_)
                }

                mean(coverage_matrix[valid, .x])
              }
            )
          )
        }
      )
    }
  ) |>
  ungroup() |>
  mutate(
    evaluation_period = factor(
      evaluation_period,
      levels = c("Overall", "After stockout", "After interruption")
    )
  )

quantile_coverage_summary <- quantile_coverage_by_series |>
  group_by(scenario, evaluation_period, variant, quantile_level) |>
  summarise(
    n_series = n(),
    mean_coverage = mean(coverage, na.rm = TRUE),
    se_coverage = sd(coverage, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    coverage_lower = pmax(0, mean_coverage - 1.96 * se_coverage),
    coverage_upper = pmin(1, mean_coverage + 1.96 * se_coverage)
  )

write_rds(
  quantile_coverage_summary,
  "data/results/tckf_regime_separation_quantile_coverage.rds"
)


# plot styling ------------------------------------------------------------

variant_colour_values <- c(
  "TCKF-Separated" = "#2D68C4",
  "TCKF-Pooled" = "#ffb343"
)

variant_shape_values <- c(
  "TCKF-Separated" = 16,
  "TCKF-Pooled" = 17
)

plot_theme <- theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    panel.border = element_rect(
      colour = "#6d8196",
      fill = NA,
      linewidth = 0.4
    ),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold", colour = "#1a2b48"),
    axis.title.y = element_text(face = "bold", colour = "#1a2b48"),
    axis.text.x = element_text(colour = "#1a2b48"),
    axis.text.y = element_text(colour = "#1a2b48"),
    legend.position = "right",
    legend.title = element_text(face = "bold", colour = "#1a2b48"),
    legend.text = element_text(colour = "#1a2b48"),
    plot.title = element_text(face = "bold", colour = "#1a2b48")
  )

position_variant <- position_dodge(width = 0.35)


# MSE ---------------------------------------------------------------------

mse_plot <- ggplot(
  regime_diagnostic_summary,
  aes(x = evaluation_period, y = mean_mse, colour = variant, shape = variant)
) +
  geom_errorbar(
    aes(ymin = mse_lower, ymax = mse_upper),
    width = 0.12,
    linewidth = 0.55,
    position = position_variant
  ) +
  geom_point(size = 3, position = position_variant) +
  scale_colour_manual(values = variant_colour_values) +
  scale_shape_manual(values = variant_shape_values) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "(a) Mean-squared error",
    x = NULL,
    y = "MSE",
    colour = "Variant",
    shape = "Variant"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


# RMSE --------------------------------------------------------------------

rmse_plot <- ggplot(
  regime_diagnostic_summary,
  aes(x = evaluation_period, y = mean_rmse, colour = variant, shape = variant)
) +
  geom_errorbar(
    aes(ymin = rmse_lower, ymax = rmse_upper),
    width = 0.12,
    linewidth = 0.55,
    position = position_variant
  ) +
  geom_point(size = 3, position = position_variant) +
  scale_colour_manual(values = variant_colour_values) +
  scale_shape_manual(values = variant_shape_values) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  labs(
    title = "(b) Root-mean-squared error",
    x = NULL,
    y = "RMSE",
    colour = "Variant",
    shape = "Variant"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


# empirical 90% coverage --------------------------------------------------

coverage_plot <- ggplot(
  regime_diagnostic_summary,
  aes(
    x = evaluation_period,
    y = mean_coverage_90,
    colour = variant,
    shape = variant
  )
) +
  geom_hline(
    yintercept = 0.90,
    colour = "#e74c3c",
    linetype = "dashed",
    linewidth = 0.65
  ) +
  geom_errorbar(
    aes(ymin = coverage_lower, ymax = coverage_upper),
    width = 0.12,
    linewidth = 0.55,
    position = position_variant
  ) +
  geom_point(size = 3, position = position_variant) +
  scale_colour_manual(values = variant_colour_values) +
  scale_shape_manual(values = variant_shape_values) +
  facet_wrap(~scenario, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "(c) Interval calibration",
    x = NULL,
    y = "Empirical 90% coverage",
    colour = "Variant",
    shape = "Variant"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


# combined diagnostic figure ---------------------------------------------

regime_diagnostic_plot <- mse_plot + rmse_plot + coverage_plot +
  plot_layout(ncol = 3, guides = "collect")

regime_diagnostic_plot


# upper-quantile coverage -------------------------------------------------

quantile_coverage_plot <- ggplot(
  quantile_coverage_summary,
  aes(
    x = quantile_level,
    y = mean_coverage,
    colour = variant,
    shape = variant,
    group = variant
  )
) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "#e74c3c",
    linetype = "dashed",
    linewidth = 0.65
  ) +
  geom_errorbar(
    aes(ymin = coverage_lower, ymax = coverage_upper),
    width = 0.004,
    linewidth = 0.45,
    alpha = 0.75
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2.7) +
  scale_colour_manual(values = variant_colour_values) +
  scale_shape_manual(values = variant_shape_values) +
  scale_x_continuous(
    breaks = quantile_levels,
    labels = scales::label_percent(accuracy = 0.1)
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  facet_grid(
    rows = vars(scenario),
    cols = vars(evaluation_period),
    scales = "free_y"
  ) +
  labs(
    x = "Nominal upper-quantile level",
    y = "Empirical upper-quantile coverage",
    colour = "Variant",
    shape = "Variant"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

quantile_coverage_plot

regime_diagnostic_plot <- rmse_plot + quantile_coverage_plot +
  plot_layout(ncol = 2, guides = "collect")
