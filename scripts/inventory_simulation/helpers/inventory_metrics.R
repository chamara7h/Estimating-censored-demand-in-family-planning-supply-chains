summarise_inventory_metrics <- function(df,
                                        demand_col,
                                        lambda_grid = c(0, 0.1, 0.25, 0.5, 1)) {
  
  # demand for this evaluation
  demand <- df[[demand_col]]
  
  # define met_demand robustly 
  met_demand <- if ("met_demand" %in% names(df)) {
    df$met_demand
  } else {
    demand <= df$opening_stock
  }
  
  # stockout event consistent with met_demand 
  stockout_ev <- if ("stockout_ev" %in% names(df)) {
    df$stockout_ev
  } else {
    !met_demand
  }
  
  # inventory burden
  avg_I  <- mean((df$closing_stock + df$closing_stock)/2, na.rm = TRUE)
  avg_D  <- mean(demand, na.rm = TRUE)
  I_norm <- ifelse(avg_D > 0, avg_I / avg_D, NA_real_)
  
  # service 
  CSL <- mean(met_demand, na.rm = TRUE)
  
  out <- tibble(
    CSL           = CSL,
    stockout_rate = mean(stockout_ev, na.rm = TRUE),
    avg_I         = avg_I,
    avg_D         = avg_D,
    I_norm        = I_norm
  )
  
  out_lambda <- tibble(lambda = lambda_grid) |>
    mutate(L = (1 - CSL) + lambda * I_norm)
  
  list(summary = out, loss_curve = out_lambda)
}
