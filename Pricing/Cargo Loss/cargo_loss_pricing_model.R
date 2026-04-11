# =============================================================
# Cargo Pricing Model
# =============================================================
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)
library(ggplot2)
library(VineCopula)
library(ismev)

set.seed(123)
dir.create("outputs", showWarnings = FALSE)

# =============================================================
# 1. Global settings
# =============================================================

EXPENSE_RATIO  <- 0.12   # proportion of premium covering expenses
PROFIT_RATIO   <- 0.07   # target profit margin
RISK_RATIO     <- 0.05   # risk loading on top of pure premium
N_SIMS         <- 50000  # main simulation size
SHORT_HORIZON  <- 3      # years for near-term projections
LONG_HORIZON   <- 10     # years for long-term projections
STRESS_SIMS    <- 30000  # simulations used in stress testing


# =============================================================
# 2. Data cleaning & Tail statistics
# =============================================================

# Cleaning columns and trimming whitespace
clean_labels <- function(df) {
  df %>%
    mutate(across(
      where(~ is.character(.x) || is.factor(.x)),
      ~ str_squish(str_remove(as.character(.x), "_.*$"))
    ))
}

clamp <- function(x, lo, hi) pmax(lo, pmin(x, hi))

# Summary statistics focused on the upper tail (e.g. losses and costs)
upper_tail_summary <- function(x) {
  q99 <- quantile(x, 0.99, na.rm = TRUE)
  tibble(
    mean     = mean(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    sd       = sd(x, na.rm = TRUE),
    p95      = quantile(x, 0.95, na.rm = TRUE),
    p99      = q99,
    tvar99   = mean(x[x >= q99], na.rm = TRUE),   # Tail Value-at-Risk
    min      = min(x, na.rm = TRUE),
    max      = max(x, na.rm = TRUE)
  )
}

# Summary statistics focused on the lower tail (e.g. returns and net revenue)
lower_tail_summary <- function(x) {
  q01 <- quantile(x, 0.01, na.rm = TRUE)
  tibble(
    mean     = mean(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    sd       = sd(x, na.rm = TRUE),
    p05      = quantile(x, 0.05, na.rm = TRUE),
    p01      = q01,
    es01     = mean(x[x <= q01], na.rm = TRUE),   # Expected Shortfall
    min      = min(x, na.rm = TRUE),
    max      = max(x, na.rm = TRUE)
  )
}


# =============================================================
# 3. Data loading
# =============================================================

load_claims <- function(exclude_cargo_types = character()) {
  
  freq_raw <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 1)
  sev_raw  <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 2)
  
  # Adding Filters
  apply_filters <- function(df, has_claim_count = TRUE) {
    df <- df %>%
      clean_labels() %>%
      filter(
        between(distance,          1,   100),
        between(cargo_value,   50000, 680000000),
        between(transit_duration,  1,    60),
        between(route_risk,        1,     5),
        between(pilot_experience,  1,    30),
        between(vessel_age,        1,    50),
        between(weight,         1500, 250000),
        between(solar_radiation,   0,     1),
        between(debris_density,    0,     1),
        exposure > 0, exposure <= 1
      )
    if (has_claim_count) df <- filter(df, between(claim_count, 0, 5))
    else                 df <- filter(df, between(claim_amount, 31000, 678000000))
    df
  }
  
  freq <- apply_filters(freq_raw, has_claim_count = TRUE)
  sev  <- apply_filters(sev_raw,  has_claim_count = FALSE)
  
  if (length(exclude_cargo_types) > 0) {
    freq <- filter(freq, !cargo_type %in% exclude_cargo_types)
    sev  <- filter(sev,  !cargo_type %in% exclude_cargo_types)
  }
  
  list(freq = freq, sev = sev)
}


# =============================================================
# 4. Model fitting
# =============================================================

fit_models <- function(freq_data, sev_data) {
  
  # Frequency model
  freq_data <- freq_data %>%
    mutate(
      claim_count    = round(claim_count),
      cargo_type     = factor(cargo_type),
      container_type = factor(container_type)
    )
  
  freq_formula <- claim_count ~ route_risk + pilot_experience +
    solar_radiation + debris_density +
    container_type + cargo_type + cargo_value +
    offset(log(exposure))
  
  model_poisson <- glm(freq_formula, family = poisson(link = "log"), data = freq_data)
  model_nb      <- glm.nb(freq_formula, data = freq_data)
  
  lambda <- predict(model_nb, type = "response")   # expected claims per policy
  theta  <- model_nb$theta                          # Negative-Binomial dispersion parameter
  
  # Match a single Negative-Binomial for the whole portfolio
  mu_total  <- sum(lambda)
  var_total <- sum(lambda + lambda^2 / theta)
  theta_eff <- if (var_total > mu_total) mu_total^2 / (var_total - mu_total) else 1e6
  theta_eff <- max(theta_eff, 1e-6)
  
  # Severity model
  numeric_cols <- c("route_risk", "solar_radiation", "debris_density", "cargo_value",
                    "weight", "vessel_age", "distance", "transit_duration", "pilot_experience")
  
  sev_data <- sev_data %>%
    mutate(
      cargo_type     = factor(cargo_type,     levels = levels(freq_data$cargo_type)),
      container_type = factor(container_type, levels = levels(freq_data$container_type)),
      claim_amount   = pmax(claim_amount, 0.01)
    ) %>%
    filter(!is.na(cargo_type), !is.na(container_type))
  
  # Standardise numeric inputs for severity model
  col_means  <- sapply(sev_data[numeric_cols], mean, na.rm = TRUE)
  col_sds    <- pmax(sapply(sev_data[numeric_cols], sd, na.rm = TRUE), 1e-6)
  sev_scaled <- sev_data
  for (col in numeric_cols) sev_scaled[[col]] <- (sev_scaled[[col]] - col_means[col]) / col_sds[col]
  
  model_sev <- glm(
    claim_amount ~ route_risk + solar_radiation + debris_density +
      cargo_value + weight + vessel_age + distance + transit_duration +
      pilot_experience + container_type + cargo_type,
    family = Gamma(link = "log"),
    data   = sev_scaled
  )
  
  sev_shape <- 1 / summary(model_sev)$dispersion
  sev_scale <- mean(sev_data$claim_amount, na.rm = TRUE) / sev_shape
  
  # GPD tail fit on observed severities
  tail_fit <- fit_gpd_tail(sev_data$claim_amount)
  
  list(
    freq_data     = freq_data,
    sev_data      = sev_data,
    model_poisson = model_poisson,
    model_nb      = model_nb,
    model_sev     = model_sev,
    lambda        = lambda,
    theta         = theta,
    mu_total      = mu_total,
    var_total     = var_total,
    theta_eff     = theta_eff,
    sev_shape     = sev_shape,
    sev_scale     = sev_scale,
    tail_fit      = tail_fit,
    total_exposure = sum(freq_data$exposure, na.rm = TRUE)
  )
}

# =============================================================
# 5. GPD tail fitting
# =============================================================

fit_gpd_tail <- function(claims, tail_quantile = 0.95) {
  
  claims    <- claims[is.finite(claims) & claims > 0]
  threshold <- quantile(claims, tail_quantile, na.rm = TRUE)
  excess    <- claims[claims > threshold] - threshold
  p_tail    <- length(excess) / length(claims)
  
  # Fit Gamma by moments
  bulk    <- claims[claims <= threshold]
  b_mean  <- mean(bulk)
  b_var   <- var(bulk)
  b_shape <- b_mean^2 / max(b_var, 1e-6)
  b_scale <- b_var   / max(b_mean, 1e-6)
  
  # Tail Fit GPD via MLE
  gpd_shape <- 0
  gpd_scale <- max(mean(excess), 1)
  
  if (length(excess) >= 50) {
    fit <- tryCatch(
      suppressWarnings(gpd.fit(excess, threshold = 0, show = FALSE)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      gpd_scale <- max(fit$mle[1], 1e-6)
      gpd_shape <- clamp(fit$mle[2], -0.40, 0.49)
    }
  }
  
  list(
    threshold = threshold,
    p_tail    = p_tail,
    b_shape   = b_shape,
    b_scale   = b_scale,
    gpd_scale = gpd_scale,
    gpd_shape = gpd_shape
  )
}


# =============================================================
# 6. Copula fitting (for frequency–severity dependence)
# =============================================================

fit_copula <- function(fit, max_obs = 60000) {
  
  freq_pos <- fit$freq_data$claim_count
  freq_pos <- freq_pos[freq_pos > 0]
  sev_vals <- fit$sev_data$claim_amount
  n        <- min(length(freq_pos), length(sev_vals), max_obs)
  
  u <- rank(freq_pos[1:n]) / (n + 1)
  v <- rank(sev_vals[1:n]) / (n + 1)
  
  cop <- tryCatch(BiCopSelect(u, v, familyset = NA), error = function(e) NULL)
  
  if (!is.null(cop)) {
    list(method = "VineCopula", family = cop$family,
         family_name = cop$familyname, par = cop$par, par2 = cop$par2,
         tau = cop$tau, u = u, v = v)
  } else {
    rho <- cor(qnorm(u), qnorm(v), use = "complete.obs")
    list(method = "Gaussian", family = NA, family_name = "Gaussian",
         par = rho, par2 = NA_real_,
         tau = cor(u, v, method = "kendall"), u = u, v = v)
  }
}

simulate_copula_uniforms <- function(copula, n) {
  if (copula$method == "VineCopula") {
    BiCopSim(n, copula$family, copula$par, copula$par2)
  } else {
    rho <- clamp(copula$par, -0.95, 0.95)
    z   <- mvrnorm(n, mu = c(0, 0), Sigma = matrix(c(1, rho, rho, 1), 2))
    cbind(pnorm(z[, 1]), pnorm(z[, 2]))
  }
}

# =============================================================
# 7. Aggregate loss simulation via Monte Carlo
# =============================================================

simulate_losses <- function(
    fit,
    n_sims,
    freq_multiplier  = 1,
    sev_multiplier   = 1,
    copula_uniforms  = NULL,   
    cat_prob         = 0,      
    cat_freq_mult    = 1,
    cat_sev_mult     = 1
) {
  mu    <- max(fit$mu_total * freq_multiplier, 1e-9)
  theta <- max(fit$theta_eff, 1e-6)
  
  if (is.null(copula_uniforms)) {
    # Independent simulation
    counts   <- rnbinom(n_sims, size = theta, mu = mu)
    sev_mult <- rep(sev_multiplier, n_sims)
  } else {
    # Copula-induced dependence
    counts   <- qnbinom(copula_uniforms[, 1], size = theta, mu = mu)
    sev_dep  <- qgamma(copula_uniforms[, 2], shape = fit$sev_shape, scale = 1 / fit$sev_shape)
    sev_mult <- sev_multiplier * sev_dep
  }
  
  # Overlay catastrophe years
  if (cat_prob > 0) {
    is_cat   <- rbinom(n_sims, 1, clamp(cat_prob, 0, 1)) == 1
    counts   <- round(counts * ifelse(is_cat, cat_freq_mult, 1))
    sev_mult <- sev_mult * ifelse(is_cat, cat_sev_mult, 1)
  }
  
  # Sum individual Gamma claims
  losses          <- numeric(n_sims)
  has_claims      <- counts > 0
  losses[has_claims] <- rgamma(
    sum(has_claims),
    shape = pmax(counts[has_claims] * fit$sev_shape, 1e-9),
    scale = fit$sev_scale * sev_mult[has_claims]
  )
  losses
}

# =============================================================
# 8. Investment return simulation
# =============================================================

simulate_returns <- function(n_sims, premium_cashflow, rf_base,
                             rf_vol = 0.005, premium_vol = 0.03) {
  h <- length(premium_cashflow)
  
  rate_shock    <- matrix(rnorm(n_sims * h, 0, rf_vol),     nrow = n_sims)
  premium_shock <- matrix(rlnorm(n_sims * h,
                                 meanlog = -0.5 * premium_vol^2,
                                 sdlog   = premium_vol),     nrow = n_sims)
  
  premiums  <- matrix(rep(premium_cashflow, each = n_sims), nrow = n_sims) * premium_shock
  rf_matrix <- matrix(rep(rf_base,          each = n_sims), nrow = n_sims) + rate_shock
  rf_matrix <- pmax(rf_matrix, -0.02)   # floor at -2%
  
  premiums * (1 + rf_matrix)
}

# =============================================================
# 9. Horizon summaries (3-year and 10-year)
# =============================================================

horizon_summary <- function(cost_matrix, return_matrix, annual_premium, scenario_name) {
  
  summarise_horizon <- function(h, label) {
    costs   <- rowSums(cost_matrix[,   1:h, drop = FALSE])
    returns <- rowSums(return_matrix[, 1:h, drop = FALSE])
    net     <- returns - costs
    
    bind_rows(
      upper_tail_summary(costs)   %>% mutate(stream = "costs"),
      upper_tail_summary(returns) %>% mutate(stream = "returns"),
      lower_tail_summary(net)     %>% mutate(stream = "net_revenue")
    ) %>%
      mutate(scenario = scenario_name, horizon = label,
             annual_premium = annual_premium, .before = 1)
  }
  
  bind_rows(
    summarise_horizon(SHORT_HORIZON, "3-year"),
    summarise_horizon(LONG_HORIZON,  "10-year")
  )
}

# =============================================================
# 10. Calculating Premiums
# =============================================================

run_cargo_pipeline <- function(
    exclude_cargo_types = character(),
    output_prefix       = "cargo",
    run_stress          = TRUE
) {
  
  cat("Loading claims data...\n")
  claims <- load_claims(exclude_cargo_types)
  
  cat("Fitting frequency and severity models...\n")
  fit <- fit_models(claims$freq, claims$sev)
  
  cat("Fitting copula...\n")
  fit$copula <- fit_copula(fit)
  
  # Premium calculation
  expected_annual_cost  <- mean(simulate_losses(fit, N_SIMS))
  pure_premium_rate     <- expected_annual_cost / fit$total_exposure
  technical_premium_rate <- (pure_premium_rate * (1 + RISK_RATIO)) /
    (1 - EXPENSE_RATIO - PROFIT_RATIO)
  annual_technical_premium <- technical_premium_rate * fit$total_exposure
  
  cat(sprintf("Annual technical premium: $%.0f\n", annual_technical_premium))
  
  # Baseline loss distribution
  cat("Simulating baseline losses...\n")
  baseline_losses <- simulate_losses(fit, N_SIMS)
  
  # Copula-adjusted losses
  uv             <- simulate_copula_uniforms(fit$copula, N_SIMS)
  copula_losses  <- simulate_losses(fit, N_SIMS, copula_uniforms = uv)
  
  # Economic projections
  cat("Loading economic projections...\n")
  source("inflation_forecast.R")
  econ <- forecast_inflation_and_rates(
    path    = "srcsc-2026-interest-and-inflation.xlsx",
    horizon = LONG_HORIZON
  ) %>%
    mutate(
      severity_trend = cumprod(1 + inflation),
      discount       = 1 / cumprod(1 + rf_1y),
      premium_cf     = annual_technical_premium * severity_trend,
      return_cf      = premium_cf * (1 + rf_1y),
      expected_cost  = expected_annual_cost * severity_trend,
      net_revenue    = return_cf - expected_cost,
      pv_net_revenue = net_revenue * discount
    )
  
  rf_vol <- max(sd(econ$rf_1y, na.rm = TRUE), 0.005)
  
  # Stretch single-year losses across all years with inflation
  cost_matrix   <- sweep(
    matrix(baseline_losses, nrow = N_SIMS, ncol = LONG_HORIZON),
    MARGIN = 2, STATS = econ$severity_trend, FUN = "*"
  )
  return_matrix <- simulate_returns(N_SIMS, econ$premium_cf, econ$rf_1y, rf_vol)
  
  baseline_horizon <- horizon_summary(
    cost_matrix, return_matrix,
    annual_premium = annual_technical_premium,
    scenario_name  = "Baseline"
  )
  
  # Scenario analysis
  cat("Running scenario analysis...\n")
  scenarios <- tibble(
    name          = c("Best Case", "Base Case", "Worst Case"),
    freq_mult     = c(0.85,  1.00,  1.40),
    sev_mult      = c(0.90,  1.00,  1.50),
    cat_prob      = c(0.000, 0.005, 0.020),
    cat_freq_mult = c(1.00,  1.50,  2.50),
    cat_sev_mult  = c(1.00,  1.50,  3.00)
  )
  
  scenario_summaries <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
    sc        <- scenarios[i, ]
    uv_sc     <- simulate_copula_uniforms(fit$copula, N_SIMS)
    sc_losses <- simulate_losses(fit, N_SIMS,
                                 freq_multiplier = sc$freq_mult,
                                 sev_multiplier  = sc$sev_mult,
                                 copula_uniforms = uv_sc,
                                 cat_prob        = sc$cat_prob,
                                 cat_freq_mult   = sc$cat_freq_mult,
                                 cat_sev_mult    = sc$cat_sev_mult)
    
    sc_premium <- annual_technical_premium * sc$freq_mult * sc$sev_mult
    sc_econ    <- mutate(econ, premium_cf = sc_premium * severity_trend)
    
    sc_cost_m   <- sweep(matrix(sc_losses, nrow = N_SIMS, ncol = LONG_HORIZON),
                         2, sc_econ$severity_trend, "*")
    sc_return_m <- simulate_returns(N_SIMS, sc_econ$premium_cf, sc_econ$rf_1y, rf_vol)
    
    horizon_summary(sc_cost_m, sc_return_m, sc_premium, sc$name)
  }))
  
  
  # Pricing summary
  precious_share <- fit$sev_data %>%
    summarise(share = sum(claim_amount[cargo_type %in% c("gold", "platinum")], na.rm = TRUE) /
                sum(claim_amount, na.rm = TRUE)) %>%
    pull(share)
  
  pricing_summary <- tibble(
    metric = c(
      "frequency_records", "severity_records", "total_exposure",
      "expected_claim_count", "claim_count_variance",
      "nb_theta", "effective_theta",
      "severity_gamma_shape", "severity_gamma_scale", "mean_observed_severity",
      "expected_annual_cost", "annual_pure_premium", "annual_technical_premium",
      "pure_premium_rate", "technical_premium_rate",
      "expense_ratio", "profit_ratio", "risk_ratio",
      "gpd_threshold_95pct", "p_tail", "gpd_shape", "gpd_scale",
      "precious_metals_severity_share"
    ),
    value = c(
      nrow(fit$freq_data), nrow(fit$sev_data), fit$total_exposure,
      fit$mu_total, fit$var_total,
      fit$theta, fit$theta_eff,
      fit$sev_shape, fit$sev_scale, mean(fit$sev_data$claim_amount, na.rm = TRUE),
      expected_annual_cost, expected_annual_cost, annual_technical_premium,
      pure_premium_rate, technical_premium_rate,
      EXPENSE_RATIO, PROFIT_RATIO, RISK_RATIO,
      fit$tail_fit$threshold, fit$tail_fit$p_tail,
      fit$tail_fit$gpd_shape,  fit$tail_fit$gpd_scale,
      precious_share
    )
  )
  
  model_diagnostics <- tibble(
    metric = c("poisson_aic", "nb_aic", "poisson_bic", "nb_bic",
               "observed_zero_rate", "fitted_zero_rate_nb",
               "gamma_dispersion", "mean_observed_severity",
               "baseline_mean_loss", "baseline_var99", "baseline_tvar99",
               "copula_mean_loss",   "copula_var99",   "copula_tvar99"),
    value  = c(
      AIC(fit$model_poisson), AIC(fit$model_nb),
      BIC(fit$model_poisson), BIC(fit$model_nb),
      mean(fit$freq_data$claim_count == 0, na.rm = TRUE),
      mean(dnbinom(0, size = fit$theta, mu = fit$lambda), na.rm = TRUE),
      summary(fit$model_sev)$dispersion,
      mean(fit$sev_data$claim_amount, na.rm = TRUE),
      mean(baseline_losses),
      quantile(baseline_losses, 0.99),
      mean(baseline_losses[baseline_losses >= quantile(baseline_losses, 0.99)]),
      mean(copula_losses),
      quantile(copula_losses, 0.99),
      mean(copula_losses[copula_losses >= quantile(copula_losses, 0.99)])
    )
  )
  
# =============================================================
# 11. Run
# =============================================================

results <- run_cargo_pipeline(
  exclude_cargo_types = character(),   # e.g. c("gold", "platinum") to exclude
  output_prefix       = "cargo",
  run_stress          = TRUE
)