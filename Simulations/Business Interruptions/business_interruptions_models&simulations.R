# =============================================================
# Frequency Modelling
# =============================================================
bi_freq_model <- bi_freq_clean %>%
  mutate(log_exposure = log(exposure))

full_formula <- claim_count ~ solar_system + production_load +
  energy_backup_score + supply_chain_index +
  avg_crew_exp + maintenance_freq + safety_compliance +
  offset(log_exposure)

poisson_model <- glm(full_formula, family = poisson(link = "log"), data = bi_freq_model)

dispersion <- sum(residuals(poisson_model, type="pearson")^2) / poisson_model$df.residual

nb_model <- glm.nb(full_formula, data = bi_freq_model)

zinb_model <- zeroinfl(
  claim_count ~ solar_system + production_load +
    energy_backup_score + supply_chain_index +
    avg_crew_exp + maintenance_freq +
    safety_compliance + offset(log_exposure) | 1,
  data = bi_freq_model,
  dist = "negbin"
)

AIC(poisson_model, nb_model, zinb_model)

# =============================================================
# Severity Modelling
# =============================================================
bi_sev_model <- bi_sev_clean
bi_sev_model$claim_amount[bi_sev_model$claim_amount <= 0] <- 0.01

gamma_sev <- glm(
  claim_amount ~ solar_system + production_load +
    energy_backup_score + safety_compliance + exposure,
  data = bi_sev_model,
  family = Gamma(link = "log")
)

gamma_sev_step <- stepAIC(gamma_sev, direction="both")

# =============================================================
# Monte Carlo Portfolio Simulation & Risk Measures
# =============================================================
set.seed(123)
n_sim <- 10000

# Frequency & severity parameters
lambda_hat <- predict(nb_model, type = "response")
theta_hat  <- nb_model$theta
n_pol      <- length(lambda_hat)
sev_shape  <- 1 / summary(gamma_sev)$dispersion
sev_scale  <- mean(bi_sev_clean$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

for(s in 1:n_sim){
  freq_sim <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat)
  total_claims <- sum(freq_sim)
  if(total_claims > 0){
    severities <- rgamma(total_claims, shape = sev_shape, scale = sev_scale)
    aggregate_loss[s] <- sum(severities)
  } else {
    aggregate_loss[s] <- 0
  }
}

aggregate_loss <- aggregate_loss * 0.3380428

# Risk Measures
BI_mean_loss_mc <- mean(aggregate_loss)
BI_sd_loss_mc   <- sd(aggregate_loss)
BI_VaR_99_mc   <- quantile(aggregate_loss, 0.99)
BI_TVaR_99_mc  <- mean(aggregate_loss[aggregate_loss > BI_VaR_99_mc])
BI_VaR_995_mc  <- quantile(aggregate_loss, 0.995)
BI_VaR_999_mc  <- quantile(aggregate_loss, 0.999)

risk_summary_mc <- data.frame(
  Metric = c("Mean Loss", "Std Dev", "VaR 99", "TVaR 99", "VaR 99.5", "VaR 99.9"),
  Value  = c(BI_mean_loss_mc, BI_sd_loss_mc, BI_VaR_99_mc, BI_TVaR_99_mc,
             BI_VaR_995_mc, BI_VaR_999_mc)
)

print(risk_summary_mc)
