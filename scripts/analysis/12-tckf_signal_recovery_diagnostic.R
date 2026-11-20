# TCKF signal-recovery diagnostic

library(tidyverse)


# read data ---------------------------------------------------------------

diag_path <- if (file.exists("data/predictions/tckf_diag.rds")) {
  "data/predictions/tckf_diag.rds"
} else {
  "../../data/predictions/tckf_diag.rds"
}

synthetic_path <- if (file.exists("data/tidy_data/synthetic_data.rds")) {
  "data/tidy_data/synthetic_data.rds"
} else {
  "../../data/tidy_data/synthetic_data.rds"
}

tckf_diag <- read_rds(diag_path)

simulated_regimes <- read_rds(synthetic_path) |>
  as_tibble() |>
  select(
    date, series_category, series_type, store, product,
    simulated_regime = censorship_type
  )


# final TCKF specification ------------------------------------------------

plot_data <- tckf_diag |>
  filter(
    diag_spec == "full",
    diag_lambda_choice == "hat",
    diag_cp_mode == "rolling",
    !is.na(yhat),
    !is.na(actual_demand)
  ) |>
  select(-censorship_type) |>
  left_join(
    simulated_regimes,
    by = c(
      "date", "series_category", "series_type", "store", "product"
    )
  ) |>
  mutate(
    # During interruptions, the stored state is the propagated prior state.
    yhat_signal = case_when(
      simulated_regime == "disruption" ~ pmax(0, level + seasonal),
      TRUE ~ yhat
    ),
    interval_radius_90 = upper_q90 - yhat,
    censorship_label = case_when(
      simulated_regime == "none" ~ "Uncensored (Normal Operations)",
      simulated_regime == "censored" ~ "Partial Censorship (Stockouts)",
      simulated_regime == "disruption" ~ "Full Censorship (Interruptions)"
    ),
    censorship_label = factor(
      censorship_label,
      levels = c(
        "Uncensored (Normal Operations)",
        "Partial Censorship (Stockouts)",
        "Full Censorship (Interruptions)"
      )
    )
  )

if (any(is.na(plot_data$simulated_regime))) {
  stop("Some diagnostic rows could not be matched to the numerical data.")
}


# diagnostic summary ------------------------------------------------------

signal_recovery_summary <- plot_data |>
  group_by(censorship_label) |>
  summarise(
    n_forecasts = n(),
    mse = mean((yhat_signal - actual_demand)^2, na.rm = TRUE),
    rmse = sqrt(mse),
    mean_error = mean(yhat_signal - actual_demand, na.rm = TRUE),
    coverage_90 = mean(
      actual_demand >= pmax(0, yhat_signal - interval_radius_90) &
        actual_demand <= yhat_signal + interval_radius_90,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    mse = round(mse, 2),
    rmse = round(rmse, 2),
    mean_error = round(mean_error, 2),
    coverage_90 = round(coverage_90, 3)
  )


# plot styling ------------------------------------------------------------

plot_theme <- theme_minimal(base_family = "sans", base_size = 16) +
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
    strip.text = element_text(face = "bold", colour = "#1a2b48"),
    plot.title = element_text(colour = "#1a2b48")
  )


# binned signal-recovery plot --------------------------------------------

plot_limit <- 700

signal_recovery_plot <- plot_data |>
  filter(
    actual_demand <= plot_limit,
    yhat_signal <= plot_limit
  ) |>
  ggplot(aes(x = actual_demand, y = yhat_signal)) +
  geom_bin_2d(bins = 35) +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "#e74c3c",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  scale_fill_viridis_c(
    trans = "log10",
    option = "C",
    name = "Number of\nforecasts"
  ) +
  facet_wrap(~censorship_label, nrow = 1) +
  coord_equal(
    xlim = c(0, plot_limit),
    ylim = c(0, plot_limit),
    expand = FALSE
  ) +
  labs(
    x = "True latent demand",
    y = "TCKF predicted demand"
  ) +
  plot_theme

signal_recovery_plot
