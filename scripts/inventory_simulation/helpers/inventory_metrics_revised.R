summarise_inventory_metrics_revised <- function(df,
                                                demand_col) {

  demand <- df[[demand_col]]

  met_demand <- if ("met_demand" %in% names(df)) {
    df$met_demand
  } else {
    demand <= df$opening_stock
  }

  stockout_ev <- if ("stockout_ev" %in% names(df)) {
    df$stockout_ev
  } else {
    !met_demand
  }

  # Average on-hand inventory should use opening and closing stock, not closing twice.
  avg_I <- mean((df$opening_stock + df$closing_stock) / 2, na.rm = TRUE)
  avg_D <- mean(demand, na.rm = TRUE)
  I_norm <- ifelse(avg_D > 0, avg_I / avg_D, NA_real_)

  CSL <- mean(met_demand, na.rm = TRUE)

  tibble(
    CSL = CSL,
    stockout_rate = mean(stockout_ev, na.rm = TRUE),
    avg_I = avg_I,
    avg_D = avg_D,
    I_norm = I_norm
  )
}
