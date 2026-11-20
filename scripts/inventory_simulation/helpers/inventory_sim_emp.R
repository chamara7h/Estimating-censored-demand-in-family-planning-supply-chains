# inventory simulation engine

# inventory simulation engine

simulate_inventory_policy <- function(df,
                                      demand_col,
                                      lead_time_col = "fixed_lead_time",
                                      policy_name = "policy",
                                      q_max = Inf) {
  
  df <- df |>
    arrange(date) |>
    mutate(
      policy        = policy_name,
      opening_stock = 0,
      received      = 0,
      closing_stock = 0,
      order_qty     = 0,
      delivery_due  = as.Date(NA)
    )
  
  deliveries <- list()
  
  for (i in seq_len(nrow(df))) {
    
    today <- as.Date(df$date[i])
    
    # receive arrivals due today (these are past orders arriving now)
    arrivals <- deliveries |>
      keep(~ .x$arrival_date == today) |>
      map_dbl("amount")
    
    total_received <- sum(arrivals)
    
    # opening stock at start of period t (after receiving)
    if (i > 1) {
      df$opening_stock[i] <- df$closing_stock[i - 1] + total_received
    } else {
      df$opening_stock[i] <- 0 + total_received
    }
    
    df$received[i] <- total_received
    
    # realise demand (lost-sales)
    dem <- df[[demand_col]][i]
    df$closing_stock[i] <- max(0, df$opening_stock[i] - dem)
    
    # Inventory position for L=1 at ordering epoch:
    # IP_t = I_t + q_{t-1}
    # In this engine, q_{t-1} has already arrived as total_received, so IP_t = opening_stock
    IP_t <- df$opening_stock[i]
    
    # order-up-to decision
    order_raw <- max(0, df$target_stock[i] - df$closing_stock[i])
    
    # capped base-stock (Policy 4)
    order_qty <- min(q_max, order_raw)
    
    # schedule delivery
    lead_time <- df[[lead_time_col]][i]
    delivery_date <- today %m+% months(lead_time)
    
    deliveries <- append(deliveries, list(list(arrival_date = delivery_date, amount = order_qty)))
    
    df$order_qty[i] <- order_qty
    df$delivery_due[i] <- delivery_date
  }
  
  # service + diagnostics
  df <- df |>
    mutate(
      demand      = .data[[demand_col]],
      met_demand  = demand <= opening_stock,
      stockout_ev = !met_demand,                 # consistent with CSL definition
      in_stock    = opening_stock > 0,           # diagnostic only (availability)
      lost_sales  = pmax(0, demand - opening_stock)
    )
  
  df
}

run_one_series_policy <- function(pred_master,
                                  id,
                                  demand_col = "stock_distributed",
                                  lead_time = 1,
                                  policy = c("deterministic", "quantile", "myopic", "capped", "oracle"),
                                  buffer = 3,
                                  quantile_col = "upper_q90",
                                  kappa = 10,
                                  q_max = 999999,
                                  q_cols = c("upper_q80", "upper_q85", "upper_q90", "upper_q95", "upper_q97.5"),
                                  q_levels = c(0.80, 0.85, 0.90, 0.95, 0.975)) {
  
  policy <- match.arg(policy)
  
  keys <- str_split(id, "\\|", simplify = TRUE)
  stopifnot(ncol(keys) == 3)
  
  df <- pred_master |>
    filter(site_code == keys[1],
           product_code     == keys[2],
           model           == keys[3]) |>
    arrange(date) |>
    mutate(fixed_lead_time = lead_time)
  
  if (nrow(df) == 0) return(tibble())  # safety
  
  # build targets U_t (target_stock)
  df <- switch(
    policy,
    deterministic = policy_target_deterministic(df, prediction_col = "prediction", buffer = buffer),
    quantile      = policy_target_quantile(df, quantile_col = quantile_col),
    myopic        = policy_target_myopic(df, kappa = kappa, q_cols = q_cols, q_levels = q_levels),
    capped        = policy_target_base_for_cap(df, base = "quantile", quantile_col = quantile_col),
    oracle        = policy_target_oracle(df, demand_col = demand_col, L = lead_time)
  )
  
  # if oracle created NA at tail, keep it safe
  df <- df |>
    mutate(target_stock = replace_na(target_stock, 0))
  
  # simulate
  simulate_inventory_policy(
    df          = df,
    demand_col  = demand_col,
    policy_name = policy,
    q_max       = ifelse(policy == "capped", q_max, Inf)
  )
}
