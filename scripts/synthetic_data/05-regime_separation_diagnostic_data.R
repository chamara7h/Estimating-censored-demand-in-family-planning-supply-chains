# Regime-separation diagnostic data

library(tidyverse)
library(tsibble)

set.seed(100)


# load latent numerical demand -------------------------------------------

synthetic_data <- read_rds("data/tidy_data/synthetic_data.rds") |>
  as_tibble()


# select a balanced set of numerical series ------------------------------

diagnostic_keys <- synthetic_data |>
  distinct(series_category, series_type, store, product) |>
  group_by(series_category, series_type) |>
  slice_sample(n = 50) |>
  ungroup() |>
  arrange(series_category, series_type, store, product) |>
  mutate(series_index = row_number())

base_demand <- synthetic_data |>
  semi_join(
    diagnostic_keys,
    by = c("series_category", "series_type", "store", "product")
  ) |>
  select(
    date, series_category, series_type, store, product, actual_demand
  ) |>
  left_join(
    diagnostic_keys,
    by = c("series_category", "series_type", "store", "product")
  ) |>
  arrange(series_index, date)


# controlled censoring scenarios ----------------------------------------

scenario_tbl <- tibble(
  scenario = c(
    "Short interruptions",
    "Moderate interruptions",
    "Persistent interruptions"
  ),
  interruption_periods = c(7L, 11L, 18L),
  interruption_lengths = list(1L, 2:3, 4:6)
)

train_n <- 30L
stockout_periods <- 14L
stockout_lengths <- 1:3


# place non-overlapping censoring spells ---------------------------------

place_spells <- function(available, target_periods, spell_lengths) {
  selected <- rep(FALSE, length(available))
  attempts <- 0L

  while (sum(selected) < target_periods && attempts < 10000L) {
    attempts <- attempts + 1L
    remaining <- target_periods - sum(selected)
    spell_length <- min(sample(spell_lengths, 1), remaining)

    possible_starts <- seq_len(length(available) - spell_length + 1L)
    valid_starts <- keep(possible_starts, function(start) {
      index <- start:(start + spell_length - 1L)
      neighbour_index <- seq(
        max(1L, start - 1L),
        min(length(available), start + spell_length)
      )

      all(available[index]) && !any(selected[neighbour_index])
    })

    if (length(valid_starts) == 0L) {
      break
    }

    start <- sample(valid_starts, 1)
    selected[start:(start + spell_length - 1L)] <- TRUE
  }

  if (sum(selected) < target_periods) {
    remaining_index <- which(available & !selected)
    fill_n <- min(target_periods - sum(selected), length(remaining_index))
    selected[sample(remaining_index, fill_n)] <- TRUE
  }

  selected
}


create_regime_scenarios <- function(data, series_index) {
  data <- data |>
    arrange(date)

  n_periods <- nrow(data)
  evaluation_period <- seq_len(n_periods) > train_n

  # The stockout schedule and censoring fractions are common to all scenarios.
  set.seed(1000 + series_index)
  stockout_mask <- place_spells(
    available = evaluation_period,
    target_periods = stockout_periods,
    spell_lengths = stockout_lengths
  )

  stockout_fraction <- rep(1, n_periods)
  stockout_fraction[stockout_mask] <- runif(sum(stockout_mask), 0.3, 0.8)

  map2_dfr(
    seq_len(nrow(scenario_tbl)),
    scenario_tbl$scenario,
    function(scenario_index, scenario_label) {
      set.seed(10000 + series_index * 10 + scenario_index)

      interruption_mask <- place_spells(
        available = evaluation_period & !stockout_mask,
        target_periods = scenario_tbl$interruption_periods[scenario_index],
        spell_lengths = scenario_tbl$interruption_lengths[[scenario_index]]
      )

      data |>
        mutate(
          scenario = scenario_label,
          observed_demand = case_when(
            interruption_mask ~ 0,
            stockout_mask ~ actual_demand * stockout_fraction,
            TRUE ~ actual_demand
          ),
          censorship_type = case_when(
            interruption_mask ~ "disruption",
            stockout_mask ~ "censored",
            TRUE ~ "none"
          )
        )
    }
  )
}


# generate diagnostic data -----------------------------------------------

regime_separation_data <- base_demand |>
  group_by(
    series_index, series_category, series_type, store, product
  ) |>
  group_split() |>
  map_dfr(function(data) {
    create_regime_scenarios(data, first(data$series_index))
  }) |>
  select(-series_index) |>
  mutate(
    scenario = factor(scenario, levels = scenario_tbl$scenario)
  ) |>
  as_tsibble(
    key = c(scenario, series_category, series_type, store, product),
    index = date
  )


# scenario descriptives ---------------------------------------------------

regime_separation_descriptives <- regime_separation_data |>
  as_tibble() |>
  mutate(evaluation_period = row_number() > train_n, .by = c(
    scenario, series_category, series_type, store, product
  )) |>
  filter(evaluation_period) |>
  count(scenario, censorship_type) |>
  group_by(scenario) |>
  mutate(share = n / sum(n)) |>
  ungroup()

write_rds(
  regime_separation_data,
  "data/tidy_data/synthetic_regime_separation_data.rds"
)

write_rds(
  regime_separation_descriptives,
  "data/results/synthetic_regime_separation_descriptives.rds"
)
