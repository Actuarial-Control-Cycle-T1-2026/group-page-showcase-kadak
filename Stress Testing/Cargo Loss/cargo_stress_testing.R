source("cargo_loss_pricing_model.r")

# =============================================================
# Stress testing
# =============================================================

run_stress_tests <- function(fit, output_prefix, n_sim = STRESS_SIMS) {
  
  # Stress scenarios and multiplier levels
  scenarios <- list(
    "Frequency"             = c(0.85, 1.00, 1.15, 1.30, 1.50),
    "Severity"              = c(0.85, 1.00, 1.15, 1.30, 1.50),
    "Route Risk"            = c(0.90, 1.00, 1.10, 1.20, 1.35),
    "Solar Radiation"       = c(0.85, 1.00, 1.15, 1.30, 1.45),
    "Debris Density"        = c(0.85, 1.00, 1.15, 1.30, 1.45),
    "Claims Inflation"      = c(1.00, 1.05, 1.10, 1.20, 1.30, 1.40),
    "Freq-Sev Dependence"   = c(0.00, 0.20, 0.40, 0.60, 0.80),
    "Catastrophe Probability" = c(0.000, 0.005, 0.010, 0.020, 0.030)
  )
  
  # Running each scenario
  run_scenario <- function(name, level) {
    losses <- switch(name,
                     "Frequency" =
                       simulate_losses(fit, n_sim, freq_multiplier = level),
                     "Severity" =
                       simulate_losses(fit, n_sim, sev_multiplier = level),
                     "Route Risk" =
                       simulate_losses(fit, n_sim,
                                       freq_multiplier = level,
                                       sev_multiplier  = 1 + 0.25 * (level - 1)),
                     "Solar Radiation" =
                       simulate_losses(fit, n_sim,
                                       freq_multiplier = 1 + 0.35 * (level - 1),
                                       sev_multiplier  = 1 + 0.20 * (level - 1)),
                     "Debris Density" =
                       simulate_losses(fit, n_sim,
                                       freq_multiplier = 1 + 0.45 * (level - 1),
                                       sev_multiplier  = 1 + 0.25 * (level - 1)),
                     "Claims Inflation" =
                       simulate_losses(fit, n_sim, sev_multiplier = level),
                     "Freq-Sev Dependence" = {
                       z  <- mvrnorm(n_sim, c(0, 0), matrix(c(1, level, level, 1), 2))
                       uv <- cbind(pnorm(z[, 1]), pnorm(z[, 2]))
                       simulate_losses(fit, n_sim, copula_uniforms = uv)
                     },
                     "Catastrophe Probability" =
                       simulate_losses(fit, n_sim,
                                       copula_uniforms = simulate_copula_uniforms(fit$copula, n_sim),
                                       cat_prob        = level,
                                       cat_freq_mult   = 2.2,
                                       cat_sev_mult    = 2.8)
    )
    
    q99 <- quantile(losses, 0.99, na.rm = TRUE)
    tibble(
      test          = name,
      stress_factor = level,
      mean_loss     = mean(losses, na.rm = TRUE),
      var_loss      = var(losses,  na.rm = TRUE),
      VaR_95        = quantile(losses, 0.95, na.rm = TRUE),
      VaR_99        = q99,
      TVaR_99       = mean(losses[losses >= q99], na.rm = TRUE)
    )
  }
  
  # Run all scenarios
  stress_results <- bind_rows(lapply(names(scenarios), function(name) {
    bind_rows(lapply(scenarios[[name]], function(lvl) run_scenario(name, lvl)))
  }))
  
  # Summarise the range of impact per test
  stress_ranges <- stress_results %>%
    group_by(test) %>%
    summarise(
      min_mean  = min(mean_loss, na.rm = TRUE),
      max_mean  = max(mean_loss, na.rm = TRUE),
      min_var99 = min(VaR_99,    na.rm = TRUE),
      max_var99 = max(VaR_99,    na.rm = TRUE),
      min_tvar  = min(TVaR_99,   na.rm = TRUE),
      max_tvar  = max(TVaR_99,   na.rm = TRUE),
      .groups = "drop"
    )
  
  # Save outputs
  write.csv(stress_results, paste0("outputs/", output_prefix, "_stress_results.csv"), row.names = FALSE)
  write.csv(stress_ranges,  paste0("outputs/", output_prefix, "_stress_ranges.csv"),  row.names = FALSE)
  
  list(results = stress_results, ranges = stress_ranges)
}

# -----------------------------
# Stress Test: Severity Inflation
# -----------------------------

stress_scale <- sev_scale * 1.2

aggregate_loss_stress1 <- numeric(n_sim)
for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    severities <- rgamma(freq_sim[i],
                         shape = sev_shape,
                         scale = stress_scale)
    aggregate_loss_stress1[i] <- sum(severities)
  }
}

quantile(aggregate_loss_stress1, 0.99)
mean(aggregate_loss_stress1[aggregate_loss_stress1 > quantile(aggregate_loss_stress1,0.99)])

# =============================================================
# Additional Stress testing for scenarios
# =============================================================

# -----------------------------
# Stress Test: Frequency Surge
# -----------------------------

freq_stress <- round(freq_sim * 1.3)

aggregate_loss_stress2 <- numeric(n_sim)
for(i in 1:n_sim){
  if(freq_stress[i] > 0){
    severities <- rgamma(freq_stress[i],
                         shape = sev_shape,
                         scale = sev_scale)
    aggregate_loss_stress2[i] <- sum(severities)
  }
}

quantile(aggregate_loss_stress2, 0.99)

# -----------------------------
# Stress Test: Catastrophic Scenario
# -----------------------------

aggregate_loss_stress3 <- numeric(n_sim)

for(i in 1:n_sim){
  if(freq_stress[i] > 0){
    severities <- rgamma(freq_stress[i],
                         shape = sev_shape,
                         scale = stress_scale)
    aggregate_loss_stress3[i] <- sum(severities)
  }
}

quantile(aggregate_loss_stress3,0.99)
quantile(aggregate_loss_stress3,0.999)

data.frame(
  Scenario = c("Baseline","Severity Stress","Frequency Stress","Catastrophic"),
  VaR99 = c(
    quantile(aggregate_loss,0.99),
    quantile(aggregate_loss_stress1,0.99),
    quantile(aggregate_loss_stress2,0.99),
    quantile(aggregate_loss_stress3,0.99)
  )
)

hist(aggregate_loss_stress3,
     breaks=100,
     main="Catastrophic Stress Loss Distribution")

