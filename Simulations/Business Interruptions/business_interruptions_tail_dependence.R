# =============================================================
# 10. Tail Dependence Analysis & Copulas
# =============================================================
freq <- bi_freq_clean$claim_count
freq_pos <- freq[freq>0]
sev <- bi_sev_clean$claim_amount
n <- min(length(freq_pos), length(sev))

freq_dep <- freq_pos[1:n]
sev_dep  <- sev[1:n]
dep_data <- data.frame(freq_dep, sev_dep)
u <- rank(dep_data$freq_dep) / (n + 1)
v <- rank(dep_data$sev_dep)  / (n + 1)
uv_data <- cbind(u, v)

# Fit t-Copula
t_cop <- tCopula(dim = 2)
fit_cop <- fitCopula(t_cop, uv_data, method = "ml")
summary(fit_cop)

# Simulate copula for tail analysis
set.seed(123)
n_sim <- 500000
cop_sim <- rCopula(n_sim, fit_cop@copula)
u_sim <- cop_sim[,1]
v_sim <- cop_sim[,2]

# Aggregate losses via copula
lambda_hat <- predict(zinb_model, type = "response")
lambda_mean <- mean(lambda_hat)
freq_sim <- qnbinom(u_sim, size = zinb_model$theta, mu = sample(lambda_hat, n_sim, replace = TRUE))
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(bi_sev_clean$claim_amount) / sev_shape
sev_sim <- qgamma(v_sim, shape = sev_shape, scale = sev_scale)

aggregate_loss <- freq_sim * sev_sim
for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    aggregate_loss[i] <- sum(rgamma(freq_sim[i], shape = sev_shape, scale = sev_scale))
  }
}

BI_mean_loss <- mean(aggregate_loss)
BI_sd_loss   <- sd(aggregate_loss)
BI_VaR_99   <- quantile(aggregate_loss, 0.99)
BI_TVaR_99  <- mean(aggregate_loss[aggregate_loss > BI_VaR_99])

# Fit and plot multiple copulas
norm_cop <- normalCopula(param = 0.5, dim = 2)
clay_cop <- claytonCopula(param = 2, dim = 2)
gumb_cop <- gumbelCopula(param = 2, dim = 2)
t_cop    <- tCopula(param = 0.5, df = 4, dim = 2)

fit_norm <- fitCopula(norm_cop, uv_data, method = "ml")
fit_clay <- fitCopula(clay_cop, uv_data, method = "ml")
fit_gumb <- fitCopula(gumb_cop, uv_data, method = "ml")
fit_t    <- fitCopula(t_cop, uv_data, method = "ml")

n_sim <- 5000
sim_norm <- rCopula(n_sim, fit_norm@copula)
sim_clay <- rCopula(n_sim, fit_clay@copula)
sim_gumb <- rCopula(n_sim, fit_gumb@copula)
sim_t    <- rCopula(n_sim, fit_t@copula)

par(mfrow = c(2,2))
plot(sim_norm, main = "Gaussian Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_clay, main = "Clayton Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_gumb, main = "Gumbel Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_t, main = "t-Copula", xlab = "Frequency", ylab = "Severity")

# Tail dependence plots
copula_df <- data.frame(u = u_sim, v = v_sim)

ggplot(copula_df, aes(u, v)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0.9,1), ylim = c(0.9,1)) +
  labs(title = "Upper Tail Dependence", x = "Frequency (u)", y = "Severity (v)") +
  theme_minimal()

ggplot(copula_df, aes(u, v)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,0.1)) +
  labs(title = "Lower Tail Dependence", x = "Frequency Rank (u)", y = "Severity Rank (v)") +
  theme_minimal()

# Tail dependence coefficient
mean(u_sim > 0.95 & v_sim > 0.95)
