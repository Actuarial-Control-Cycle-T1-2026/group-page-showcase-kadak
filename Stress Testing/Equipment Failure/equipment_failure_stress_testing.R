source("equipment_failure_pricing_model.r")

#### Stress Testing ############################################################
# Simulate aggregate losses given a frequency vector and Gamma parameters
sim_agg <- function(freq_vec, sh, sc) {
  vapply(freq_vec, function(n) {
    if (n > 0L) sum(rgamma(n, shape = sh, scale = sc)) else 0
  }, numeric(1))
}

# Compute risk metrics from a loss vector, returned as a one-row data frame
risk_metrics <- function(losses) {
  var99 <- as.numeric(quantile(losses, 0.99))
  data.frame(
    Mean_Loss = round(mean(losses)),
    VaR_99    = round(var99),
    TVaR_99   = round(mean(losses[losses > var99]))
  )
}

set.seed(2026)
base_freq <- rpois(n_sim, lambda = mean(lambda_hat))
loss_base <- sim_agg(base_freq, sev_shape, sev_scale)

# Sensitivity grid
shocks <- c(-0.50, -0.40, -0.30, -0.20, -0.10, 0.10, 0.20, 0.30, 0.40, 0.50)

# Claim frequency (λ)
freq_sens <- lapply(shocks, function(s) {
  f_stressed <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  cbind(data.frame(Assumption = "Frequency (lambda)",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sev_scale)))
})

# Severity scale (mean claim size)
sev_sens <- lapply(shocks, function(s) {
  cbind(data.frame(Assumption = "Severity (scale)",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(base_freq, sev_shape, sev_scale * (1 + s))))
})

# Maintenance interval 
maint_sens <- lapply(shocks, function(s) {
  f_stressed <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  cbind(data.frame(Assumption = "Maintenance Interval",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sev_scale)))
})

# Usage intensity (more hours/day causes higher frequency and severity)
usage_sens <- lapply(shocks, function(s) {
  f_stressed  <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  sc_stressed <- sev_scale * (1 + s * 0.5)
  cbind(data.frame(Assumption = "Usage Intensity",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sc_stressed)))
})

# Equipment age (older fleet causes higher frequency and severity)
age_sens <- lapply(shocks, function(s) {
  f_stressed  <- rpois(n_sim, lambda = mean(lambda_hat) * (1 + s))
  sc_stressed <- sev_scale * (1 + s * 0.5)
  cbind(data.frame(Assumption = "Equipment Age",
                   Shock = sprintf("%+.0f%%", s * 100)),
        risk_metrics(sim_agg(f_stressed, sev_shape, sc_stressed)))
})

# Combine into single sensitivity table ──
baseline_row <- cbind(
  data.frame(Assumption = "Baseline", Shock = "0%"),
  risk_metrics(loss_base)
)

sensitivity_table <- rbind(
  baseline_row,
  do.call(rbind, freq_sens),
  do.call(rbind, sev_sens),
  do.call(rbind, maint_sens),
  do.call(rbind, usage_sens),
  do.call(rbind, age_sens)
)
rownames(sensitivity_table) <- NULL
print(sensitivity_table)

# Rank assumptions by VaR impact ──
tornado_data <- sensitivity_table %>%
  filter(Shock != "0%") %>%
  group_by(Assumption) %>%
  summarise(
    VaR_low  = min(VaR_99),
    VaR_high = max(VaR_99),
    Range    = VaR_high - VaR_low,
    .groups  = "drop"
  ) %>%
  arrange(Range)

baseline_VaR <- sensitivity_table$VaR_99[sensitivity_table$Shock == "0%"]

ggplot(tornado_data,
       aes(y = reorder(Assumption, Range))) +
  geom_segment(aes(x = VaR_low, xend = VaR_high,
                   yend = reorder(Assumption, Range)),
               linewidth = 6, colour = "#ffb07c", alpha = 0.7) +
  geom_vline(xintercept = baseline_VaR,
             linetype = "dashed", colour = "navy", linewidth = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Tornado Chart – Sensitivity of VaR (99%) to ±10–50% Assumption Shocks",
       subtitle = "Dashed line = baseline VaR",
       x        = "VaR 99%",
       y        = NULL) +
  theme_minimal()

# Sensitivity line chart for VaR across full shock range per assumption
sens_plot_df <- sensitivity_table %>%
  filter(Shock != "0%") %>%
  mutate(Shock_num = as.numeric(sub("%", "", Shock)) / 100)

ggplot(sens_plot_df, aes(x = Shock_num, y = VaR_99,
                         colour = Assumption, group = Assumption)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = baseline_VaR,
             linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  scale_x_continuous(labels = scales::percent, breaks = seq(-0.5, 0.5, 0.1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "VaR (99%) Sensitivity – ±10% to ±50% Assumption Shocks",
       subtitle = "Dashed line = baseline VaR",
       x        = "Shock (%)",
       y        = "VaR 99%",
       colour   = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combined shocks when testing all assumptions simultaneously
loss_combined_up <- sim_agg(
  rpois(n_sim, lambda = mean(lambda_hat) * 1.10),
  sev_shape,
  sev_scale * 1.10
)
loss_combined_dn <- sim_agg(
  rpois(n_sim, lambda = mean(lambda_hat) * 0.90),
  sev_shape,
  sev_scale * 0.90
)

combined_table <- data.frame(
  Scenario  = c("All assumptions -10%", "Baseline", "All assumptions +10%"),
  Mean_Loss = c(mean(loss_combined_dn), mean(loss_base),    mean(loss_combined_up)),
  VaR_99    = c(quantile(loss_combined_dn, 0.99), quantile(loss_base, 0.99), quantile(loss_combined_up, 0.99)),
  TVaR_99   = c(
    mean(loss_combined_dn[loss_combined_dn > quantile(loss_combined_dn, 0.99)]),
    mean(loss_base[loss_base               > quantile(loss_base,        0.99)]),
    mean(loss_combined_up[loss_combined_up > quantile(loss_combined_up, 0.99)])
  )
)
combined_table[, -1] <- round(combined_table[, -1])
print(combined_table)

#### Multi-solar System Correlated Shock Model #################################

set.seed(2026)
n_shock <- 100000

system_params <- list(
  Helionis_Cluster = list(lambda = 0.10, shape = 2.0, scale = sev_scale * 1.0),
  Epsilon          = list(lambda = 0.14, shape = 1.8, scale = sev_scale * 1.2),
  Zeta             = list(lambda = 0.12, shape = 1.5, scale = sev_scale * 1.5)
)

run_system_sim <- function(freq_mult = 1, sev_mult = 1, apply_shock = FALSE) {
  
  shock <- if (apply_shock) {
    rbinom(n_shock, 1, 0.01) * rlnorm(n_shock, meanlog = 0, sdlog = 0.4)
  } else {
    rep(1, n_shock)
  }
  
  total <- numeric(n_shock)
  
  for (sys in names(system_params)) {
    p      <- system_params[[sys]]
    f_draw <- rpois(n_shock, p$lambda * freq_mult)
    
    if (apply_shock) f_draw <- f_draw + rbinom(n_shock, 1, 0.01) * rpois(n_shock, p$lambda * 4)
    
    sys_loss <- vapply(f_draw, function(n) {
      if (n > 0) sum(rgamma(n, shape = p$shape, scale = p$scale * sev_mult)) else 0
    }, numeric(1))
    
    total <- total + sys_loss * shock
  }
  return(total)
}

sys_baseline     <- run_system_sim()
sys_sev_stress   <- run_system_sim(sev_mult = 1.40)
sys_freq_stress  <- run_system_sim(freq_mult = 1.50)
sys_catastrophic <- run_system_sim(freq_mult = 1.50, sev_mult = 1.40, apply_shock = TRUE)

system_metrics <- function(losses, label) {
  var99 <- quantile(losses, 0.99)
  data.frame(
    Scenario  = label,
    Mean      = mean(losses),
    VaR_99    = var99,
    TVaR_99   = mean(losses[losses > var99]),
    Max_Loss  = max(losses)
  )
}

system_table <- rbind(
  system_metrics(sys_baseline,     "Baseline"),
  system_metrics(sys_sev_stress,   "Severity Stress (+40%)"),
  system_metrics(sys_freq_stress,  "Frequency Stress (+50%)"),
  system_metrics(sys_catastrophic, "Systemic Shock (Correlated)")
)
print(system_table)

# Visualise catastrophic scenario ──
sys_cat_nonzero <- sys_catastrophic[sys_catastrophic > 0]
cat(sprintf("Non-zero loss simulations: %d / %d (%.1f%%)\n",
            length(sys_cat_nonzero), length(sys_catastrophic),
            length(sys_cat_nonzero) / length(sys_catastrophic) * 100))

var99_cat <- as.numeric(system_table$VaR_99[4])

ggplot(data.frame(loss = sys_cat_nonzero), aes(x = loss)) +
  geom_density(fill = rgb(0.9, 0.3, 0.1, 0.4), colour = rgb(0.9, 0.3, 0.1),
               linewidth = 0.8) +
  geom_vline(xintercept = var99_cat,
             colour = "navy", linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = var99_cat, y = Inf,
           label = "99% VaR", colour = "navy",
           hjust = -0.15, vjust = 1.5, size = 3.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Multi-System Catastrophic Loss Distribution – Equipment Failure",
       subtitle = sprintf("Conditional on shock occurring (%.1f%% of simulations)",
                          length(sys_cat_nonzero) / length(sys_catastrophic) * 100),
       x        = "Aggregate Loss (Đ)",
       y        = "Density") +
  theme_minimal()

# Overlay all four scenarios as density curves ──
scenario_df <- rbind(
  data.frame(loss = sys_baseline[sys_baseline > 0],         Scenario = "Baseline"),
  data.frame(loss = sys_sev_stress[sys_sev_stress > 0],     Scenario = "Severity +40%"),
  data.frame(loss = sys_freq_stress[sys_freq_stress > 0],   Scenario = "Frequency +50%"),
  data.frame(loss = sys_catastrophic[sys_catastrophic > 0], Scenario = "Systemic Shock")
)

ggplot(scenario_df, aes(x = loss, colour = Scenario, fill = Scenario)) +
  geom_density(alpha = 0.15, linewidth = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  scale_colour_manual(values = c("Baseline"       = "steelblue",
                                 "Severity +40%"  = "darkorange",
                                 "Frequency +50%" = "forestgreen",
                                 "Systemic Shock" = "firebrick")) +
  scale_fill_manual(values  = c("Baseline"       = "steelblue",
                                "Severity +40%"  = "darkorange",
                                "Frequency +50%" = "forestgreen",
                                "Systemic Shock" = "firebrick")) +
  labs(title    = "Loss Distribution by Scenario – Equipment Failure",
       subtitle = "Non-zero simulations only",
       x        = "Aggregate Loss (Đ)",
       y        = "Density",
       colour   = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

