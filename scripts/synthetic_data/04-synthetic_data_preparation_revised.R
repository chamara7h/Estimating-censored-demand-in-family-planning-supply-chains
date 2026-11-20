# synthetic_demand_pipeline_calibrated_noise.R
library(tidyverse)
library(tsibble)
library(lubridate)

set.seed(100)

# ---- helpers -------------------------------------------------------------
rlogunif <- function(a, b) exp(runif(1, log(a), log(b)))   # CV^2 targets

strengths_stl <- function(x, freq = 12){
  xt  <- ts(x, frequency = freq)
  stl <- suppressWarnings(stats::stl(xt, s.window = "periodic", robust = TRUE))
  Tt  <- stl$time.series[, "trend"]
  St  <- stl$time.series[, "seasonal"]
  Rt  <- stl$time.series[, "remainder"]
  varR <- var(Rt, na.rm = TRUE)
  Ftrend  <- max(0, 1 - varR / var(Rt + Tt, na.rm = TRUE))
  Fseason <- max(0, 1 - varR / var(Rt + St, na.rm = TRUE))
  c(Ftrend = Ftrend, Fseason = Fseason)
}

# Noise-aware calibration: nudge slope & season_amp so STL strengths match targets
calibrate_strength_noise <- function(t_idx, seas_t, slope, season_amp,
                                     target_Ftrend, target_Fseason,
                                     cv_target, iters = 3){
  mt <- 1; ms <- 1
  sigma <- sqrt(log(1 + cv_target^2))   # CV^2 = exp(sigma^2)-1
  for (k in seq_len(iters)){
    trend_pattern   <- (slope * mt) * t_idx
    seasonal_effect <- (season_amp * ms) * seas_t
    base            <- trend_pattern + seasonal_effect
    base_pos        <- base - min(base) + 1e-6
    # multiplicative noise during calibration (no correlation needed here)
    eps  <- exp(rnorm(length(base_pos), 0, sigma) - 0.5*sigma^2)
    cal_series <- base_pos * eps
    s   <- strengths_stl(cal_series, 12)
    et  <- target_Ftrend  - s[["Ftrend"]]
    es  <- target_Fseason - s[["Fseason"]]
    mt  <- pmax(0, mt * (1 + 0.9*et))
    ms  <- pmax(0, ms * (1 + 0.9*es))
  }
  list(slope = slope * mt, season_amp = season_amp * ms)
}

# ---- PARAMETERS ----------------------------------------------------------
n_products <- 10
n_stores   <- 100
n_periods  <- 100
rho        <- 0.3

dates <- seq.Date(from = as.Date("2010-01-01"), by = "month", length.out = n_periods)

# ---- SHARED PATTERNS -----------------------------------------------------
shared_seasonal <- scale(rnorm(12), center = TRUE, scale = FALSE) |> as.numeric()
common_shock    <- scale(rnorm(n_periods), center = TRUE, scale = TRUE)

# ---- PARAMETER GRID ------------------------------------------------------
params <- expand_grid(
  product = paste0("P", 1:n_products),
  store   = paste0("S", 1:n_stores)
) |>
  mutate(
    series_category   = sample(c(rep("correlated", n()/2), rep("uncorrelated", n()/2)),
                               size = n(), replace = FALSE),
    series_type       = sample(c("smooth", "erratic", "lumpy"), n(), replace = TRUE),
    trend_strength    = sample(c("low", "medium", "high"), n(), replace = TRUE),
    seasonal_strength = sample(c("low", "medium", "high"), n(), replace = TRUE)
  ) |>
  mutate(
    slope = case_when(
      trend_strength == "low"    ~ runif(n(), -0.005, 0.01),
      trend_strength == "medium" ~ runif(n(),  0.01, 0.05),
      TRUE                       ~ runif(n(),  0.05, 0.2)
    ),
    season_amp = case_when(
      seasonal_strength == "low"    ~ runif(n(), 0.05, 0.2),
      seasonal_strength == "medium" ~ runif(n(), 0.2, 0.5),
      TRUE                          ~ runif(n(), 0.5, 0.8)
    )
  )

# ---- SIMULATION ----------------------------------------------------------
simulate_one <- function(product, store, category, type, slope, season_amp) {
  t_idx     <- seq_len(n_periods)
  month_idx <- lubridate::month(dates)
  
  # Your original rule: erratic has weaker deterministic structure
  if (type == "erratic") {
    slope      <- slope * 0.3
    season_amp <- season_amp * 0.5
  }
  
  seasonal_pattern <- if (category == "correlated") shared_seasonal
  else scale(rnorm(12), center = TRUE, scale = FALSE) |> as.numeric()
  seas_t <- seasonal_pattern[month_idx]
  
  # --- CV^2 by type (tune as you like) -----------------------------------
  CV_target <- if (type == "smooth") {
    rlogunif(0.06, 0.15)
  } else if (type == "erratic") {
    rlogunif(0.30, 1.20)
  } else { # lumpy
    rlogunif(0.20, 1.50)
  }
  
  # --- NEW: noise-aware calibration to fill the 0–1 strength square ------
  target_Ftrend  <- rbeta(1, 2, 2)     # centered ~0.5; use runif(1,0.05,0.95) for flat
  target_Fseason <- rbeta(1, 2, 2)
  adj <- calibrate_strength_noise(t_idx, seas_t, slope, season_amp,
                                  target_Ftrend, target_Fseason,
                                  cv_target = CV_target, iters = 3)
  slope      <- adj$slope
  season_amp <- adj$season_amp
  
  # Deterministic part
  trend_pattern   <- slope * t_idx
  seasonal_effect <- season_amp * seas_t
  baseline        <- trend_pattern + seasonal_effect
  
  # --- Multiplicative noise w/ correlation (preserves seasonality) -------
  baseline_pos <- baseline - min(baseline) + 1e-6
  
  raw_shock <- switch(type,
                      smooth  = rnorm(n_periods),
                      erratic = as.numeric(arima.sim(list(ar = runif(1, 0.3, 0.6)), n = n_periods)),
                      lumpy   = rt(n_periods, df = 3)
  )
  z_ind  <- scale(raw_shock) |> as.numeric()
  z_corr <- if (category == "correlated")
    (sqrt(rho) * as.numeric(common_shock) + sqrt(1 - rho) * z_ind)
  else z_ind
  
  sigma   <- sqrt(log(1 + CV_target^2))
  log_eps <- sigma * z_corr
  eps     <- exp(log_eps - 0.5 * sigma^2)      # E[eps]=1, Var=CV^2
  series_mul <- baseline_pos * eps
  
  # Spikes (multiplicative) for erratic/lumpy
  if (type %in% c("erratic", "lumpy")) {
    pct  <- if (type == "erratic") 0.20 else 0.10
    nb   <- floor(pct * n_periods)
    if (nb > 0) {
      idx  <- sample(n_periods, nb)
      mult <- if (type == "erratic") rlnorm(nb, log(2.0), 0.5) else rlnorm(nb, log(1.5), 0.4)
      series_mul[idx] <- series_mul[idx] * mult
    }
  }
  
  desired_mean <- runif(1, 20, 100)
  actual_all   <- series_mul / mean(series_mul) * desired_mean
  
  # Zeros (unchanged)
  actual <- if (type == "lumpy") {
    p_nz   <- runif(1, 0.3, 0.7)
    keep_i <- rbinom(n_periods, 1, p_nz)
    actual_all * keep_i
  } else {
    actual_all
  }
  if (type == "erratic") {
    p_nz   <- runif(1, 0.8, 1.0)   # almost always non-zero
    keep_i <- rbinom(n_periods, 1, p_nz)
    actual <- actual_all * keep_i
  }
  
  # Censoring (unchanged)
  observed        <- actual
  censorship_type <- rep("none", n_periods)
  prop_censor <- runif(1, 0.20, 0.40)
  n_censor    <- floor(prop_censor * n_periods)
  Ls <- integer(0)
  while (sum(Ls) < n_censor) {
    Ls <- c(Ls, if (runif(1) < 0.7) 1 else sample(2:5, 1))
  }
  excess <- sum(Ls) - n_censor
  if (excess > 0) Ls[length(Ls)] <- Ls[length(Ls)] - excess
  event_types <- sample(c("disruption","censored"), length(Ls), replace = TRUE, prob = c(0.3,0.7))
  occupied <- rep(FALSE, n_periods)
  for (i in seq_along(Ls)) {
    Li <- Ls[i]
    repeat {
      st  <- sample(1:(n_periods - Li + 1), 1)
      idx <- st:(st + Li - 1)
      if (!any(occupied[idx])) break
    }
    occupied[idx] <- TRUE
    if (event_types[i] == "disruption") {
      observed[idx]        <- 0
      censorship_type[idx] <- "disruption"
    } else {
      observed[idx]        <- actual[idx] * runif(length(idx), 0.3, 0.8)
      censorship_type[idx] <- "censored"
    }
  }
  
  tibble(
    date             = dates,
    series_category  = category,
    series_type      = type,
    store            = store,
    product          = product,
    actual_demand    = actual,
    observed_demand  = observed,
    censorship_type  = censorship_type
  )
}

# ---- GENERATE ------------------------------------------------------------
synthetic_df <- params |>
  mutate(data = pmap(
    list(product, store, series_category, series_type, slope, season_amp),
    simulate_one
  )) |>
  select(data) |>
  unnest(data)

synthetic_tsibble <- synthetic_df |>
  mutate(date = yearmonth(date)) |>
  as_tsibble(key = c(series_category, series_type, store, product),
             index = date)
# ------- Diagnostics ---------

idi_stat <- function(x){
  nz <- which(x > 0); if (length(nz) < 2) return(NA_real_); mean(diff(nz))
}
cv2_pos <- function(x){
  y <- x[x > 0]; if (length(y) < 2) return(NA_real_); (sd(y)/mean(y))^2
}

idi_cv2 <- synthetic_tsibble %>%
  as_tibble() %>%  # drop tsibble indexing behaviour
  group_by(series_category, series_type, store, product) %>%
  summarise(
    IDI = idi_stat(observed_demand),   # or actual_demand if you prefer
    CV2 = cv2_pos(observed_demand),
    .groups = "drop"
  )


synthetic_feat <- synthetic_tsibble |>
  features(observed_demand, feat_stl) # extract ts features

idi_cv2 |>
  mutate(product = factor(product,
                          levels = c('P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9', 'P10'))) |> 
  ggplot(mapping = aes(x= IDI, y = CV2, color = product)) +
  geom_point(size = 1, alpha = 0.75) + 
  # threshold lines
  geom_vline(xintercept = 4/3, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  # add text labels right next to the lines
  annotate("text",
           x    = 4/3,
           y    = max(idi_cv2$IDI, na.rm = TRUE),
           label = "IDI = 4/3",
           hjust = -0.1,          # nudge it just to the right
           vjust =  0,          # and align at the top
           size  = 4,
           family = "Assistant") +
  annotate("text",
           x    = max(idi_cv2$CV2, na.rm = TRUE),
           y    = 0.5,
           label = expression(CV^2 ~ "= 0.5"),
           hjust =  1.1,          # nudge it to the right edge
           vjust = 1.1,          # nudge it a bit above the line
           size  = 4,
           family = "Assistant") +
  
  # force the axes to include those exact ticks
  scale_x_continuous(breaks = scales::extended_breaks()(c(0, max(synthetic_feat$zero_run_mean))),
                     minor_breaks = NULL) +
  scale_y_continuous(breaks = scales::extended_breaks()(c(0, max(synthetic_feat$nonzero_squared_cv))),
                     minor_breaks = NULL) +
  
  labs(x = "IDI",
       y = expression(CV^2),
       caption = '(c)') +
  scale_color_viridis_d(option = "D", guide = FALSE) + # Use the viridis palette
  ggthemes::theme_few(base_family = "Assistant", base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing      = unit(0.5, "lines")
  ) +
  coord_cartesian(ylim = c(0, 5), xlim = c(0, 5))

synthetic_feat |>
  mutate(
    series_type     = str_to_sentence(series_type),
    series_category = str_to_sentence(series_category)
  ) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, colour = series_category)) +
  geom_point(alpha = 0.4, size = 1.2) +
  # facet_grid(
  #   rows  = vars(series_type),
  #   # cols  = vars(series_category),
  # ) +
  scale_x_continuous("Strength of trend", limits = c(0,1)) +
  scale_y_continuous("Strength of seasonality", limits = c(0,1)) +
  labs(shape = "Time series type") +
  theme_minimal(base_size = 10) +
  scale_color_colorblind() +
  theme(
    text             = element_text(family = "Assistant"),
    strip.text         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "lightgrey", fill = NA),
    panel.spacing      = unit(0.5, "lines")
  )

write_rds(synthetic_tsibble, 'data/tidy_data/synthetic_data.rds')
