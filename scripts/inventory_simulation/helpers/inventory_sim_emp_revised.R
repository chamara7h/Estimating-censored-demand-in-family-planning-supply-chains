# revised inventory simulation engine

simulate_inventory_policy_revised_emp <- function(df,
                                                  demand_col,
                                                  lead_time_col = "fixed_lead_time",
                                                  policy_name = "policy",
                                                  q_max = Inf) {

  df <- df |>
    arrange(date) |>
    mutate(
      policy = policy_name,
      opening_stock = 0,
      received = 0,
      closing_stock = 0,
      order_qty = 0,
      delivery_due = as.Date(NA)
    )

  deliveries <- list()

  for (i in seq_len(nrow(df))) {
    today <- as.Date(df$date[i])

    arrivals <- deliveries |>
      purrr::keep(~ .x$arrival_date == today) |>
      purrr::map_dbl("amount")

    total_received <- sum(arrivals)

    if (i > 1) {
      df$opening_stock[i] <- df$closing_stock[i - 1] + total_received
    } else {
      df$opening_stock[i] <- total_received
    }

    df$received[i] <- total_received

    dem <- df[[demand_col]][i]
    df$closing_stock[i] <- max(0, df$opening_stock[i] - dem)

    # End-of-period review:
    # demand is realized during month t, then an order is placed at the end of month t,
    # and with lead_time = 1 it arrives at the start of month t+1.
    # Therefore the relevant inventory position at the ordering epoch is end-of-period on-hand stock.
    inventory_position_t <- df$closing_stock[i]
    order_raw <- max(0, df$target_stock[i] - inventory_position_t)
    order_qty <- min(q_max, order_raw)

    lead_time <- df[[lead_time_col]][i]
    delivery_date <- today %m+% months(lead_time)
    deliveries <- append(deliveries, list(list(arrival_date = delivery_date, amount = order_qty)))

    df$order_qty[i] <- order_qty
    df$delivery_due[i] <- delivery_date
  }

  df |>
    mutate(
      demand = .data[[demand_col]],
      met_demand = demand <= opening_stock,
      stockout_ev = !met_demand,
      in_stock = opening_stock > 0,
      lost_sales = pmax(0, demand - opening_stock)
    )
}

run_one_series_policy_revised_emp <- function(pred_master,
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

  keys <- stringr::str_split(id, "\\|", simplify = TRUE)
  stopifnot(ncol(keys) == 3)

  df <- pred_master |>
    filter(
      site_code == keys[1],
      product_code == keys[2],
      model == keys[3]
    ) |>
    arrange(date) |>
    mutate(fixed_lead_time = lead_time)

  if (nrow(df) == 0) return(tibble())

  df <- switch(
    policy,
    deterministic = policy_target_deterministic(df, prediction_col = "prediction", buffer = buffer),
    quantile = policy_target_quantile(df, quantile_col = quantile_col),
    myopic = policy_target_myopic(df, kappa = kappa, q_cols = q_cols, q_levels = q_levels),
    capped = policy_target_base_for_cap(df, base = "quantile", quantile_col = quantile_col),
    oracle = policy_target_oracle(df, demand_col = demand_col, L = lead_time)
  ) |>
    mutate(target_stock = replace_na(target_stock, 0))

  simulate_inventory_policy_revised_emp(
    df = df,
    demand_col = demand_col,
    policy_name = policy,
    q_max = ifelse(policy == "capped", q_max, Inf)
  )
}
