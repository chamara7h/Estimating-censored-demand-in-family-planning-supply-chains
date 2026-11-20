library(tidyverse)

stock_path <- if (file.exists("data/tidy_data/stock_tsb_filtered.rds")) {
  "data/tidy_data/stock_tsb_filtered.rds"
} else {
  "../../data/tidy_data/stock_tsb_filtered.rds"
}

stock_tsb <- read_rds(stock_path)

product_names <- c(
  AS27000 = "Microgynon / pill",
  AS27132 = "Microlut",
  AS27133 = "Depo Provera injectable",
  AS27134 = "Noristerat injectable",
  AS27137 = "Jadelle",
  AS27138 = "Implanon",
  AS42018 = "Copper T IUD",
  AS46000 = "Male condom",
  AS17005 = "Female condom"
)

product_order <- c(
  "AS27000", "AS27132", "AS27133", "AS27134", "AS27137",
  "AS27138", "AS42018", "AS46000", "AS17005"
)

stock_tsb_desc <- stock_tsb |>
  as_tibble() |>
  mutate(
    product = recode(product_code, !!!product_names, .default = product_name),
    achieved_csl = case_when(
      is.na(stock_out_indicator) | is.na(interruption_indicator) ~ NA,
      stock_out_indicator == 0 & interruption_indicator == 0 ~ 1,
      TRUE ~ 0
    )
  )

# overall row
empirical_actual_overall <- stock_tsb_desc |>
  summarise(
    product_code = "Overall",
    product = "Overall",
    avg_demand = mean(stock_distributed, na.rm = TRUE),
    avg_opening_stock_units = mean(stock_initial, na.rm = TRUE),
    avg_opening_mos = avg_opening_stock_units / avg_demand,
    achieved_csl = mean(achieved_csl, na.rm = TRUE)
  )

# product rows
empirical_actual_by_product <- stock_tsb_desc |>
  group_by(product_code, product) |>
  summarise(
    avg_demand = mean(stock_distributed, na.rm = TRUE),
    avg_opening_stock_units = mean(stock_initial, na.rm = TRUE),
    avg_opening_mos = avg_opening_stock_units / avg_demand,
    achieved_csl = mean(achieved_csl, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(product_code = factor(product_code, levels = product_order)) |>
  arrange(product_code)

# final 10-row table
empirical_service_tbl <- bind_rows(
  empirical_actual_overall,
  empirical_actual_by_product
) |>
  mutate(
    Product = if_else(
      product_code == "Overall",
      "Overall",
      paste0(product, " (", as.character(product_code), ")")
    )
  ) |>
  select(
    Product,
    `Average demand` = avg_demand,
    `Average opening stock (MOS)` = avg_opening_mos,
    `Achieved CSL` = achieved_csl
  ) |>
  mutate(
    `Average demand` = round(`Average demand`, 2),
    `Average opening stock (MOS)` = round(`Average opening stock (MOS)`, 2),
    `Achieved CSL` = round(`Achieved CSL`, 3)
  )