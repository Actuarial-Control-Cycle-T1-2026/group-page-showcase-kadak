library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(MASS)
library(pscl)
library(VineCopula)
library(ggridges)
library(GGally)
library(evir)

##### Data Setup ###############################################################
setwd("~/OneDrive - UNSW/ACTL4001/Assignment/KADAK/KADAK")

#### Frequency ###
eq_freq_raw <- readxl::read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "freq")
eq_sev_raw  <- readxl::read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "sev")

eq_freq <- eq_freq_raw

# Strip corrupted suffixes from categorical fields ──
clean_category <- function(x) {
  trimws(sub("_\\?\\?\\?[0-9]+.*$", "", x))
}

eq_freq$equipment_type <- clean_category(eq_freq$equipment_type)
eq_freq$solar_system   <- clean_category(eq_freq$solar_system)

eq_freq$equipment_type <- dplyr::recode(eq_freq$equipment_type,
                                        "FexStram Carrier"  = "FluxStream Carrier",
                                        "Flux Rider"        = "Fusion Transport",
                                        "ReglAggregators"   = "Mag-Lift Aggregator"
)

# Keep only the three valid solar systems
valid_systems <- c("Helionis Cluster", "Epsilon", "Zeta")
eq_freq <- eq_freq %>% filter(solar_system %in% valid_systems)

# Align numerical values with data dictionary ──
eq_freq <- eq_freq %>%
  filter(
    equipment_age  >= 0  & equipment_age  <= 10,
    maintenance_int >= 100 & maintenance_int <= 5000,
    usage_int      >= 0  & usage_int      <= 24,
    exposure       >  0  & exposure       <= 1,
    claim_count    >= 0  & claim_count    <= 3
  ) %>%
  mutate(
    claim_count = as.integer(round(claim_count)),
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

cat("Frequency dataset rows after cleaning:", nrow(eq_freq), "\n")

#### Severity ###
eq_sev <- eq_sev_raw

eq_sev$equipment_type <- clean_category(eq_sev$equipment_type)
eq_sev$solar_system   <- clean_category(eq_sev$solar_system)

eq_sev$equipment_type <- dplyr::recode(eq_sev$equipment_type,
                                       "FexStram Carrier"  = "FluxStream Carrier",
                                       "Flux Rider"        = "Fusion Transport",
                                       "ReglAggregators"   = "Mag-Lift Aggregator"
)

cat("Severity dataset rows after cleaning:", nrow(eq_sev), "\n")

eq_sev <- eq_sev %>%
  filter(solar_system %in% valid_systems) %>%
  filter(
    equipment_age   >= 0     & equipment_age   <= 10,
    maintenance_int >= 100   & maintenance_int <= 5000,
    usage_int       >= 0     & usage_int       <= 24,
    exposure        >= 0     & exposure        <= 1,
    claim_amount    >= 11000 & claim_amount    <= 790000
  ) %>%
  mutate(
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

cat("Severity dataset rows after cleaning:", nrow(eq_sev), "\n")

### Exploratory Analysis ###
# Zero-claim proportion
zero_pct <- mean(eq_freq$claim_count == 0) * 100
cat(sprintf("Zero-claim records: %.1f%%\n", zero_pct))

# Claim count distribution
table(eq_freq$claim_count)

# Average claim by equipment type
eq_sev %>%
  group_by(equipment_type) %>%
  summarise(mean_claim = mean(claim_amount),
            median_claim = median(claim_amount),
            n = n()) %>%
  arrange(desc(mean_claim))


##### Model Datasets: Predictors ###############################################
freq_predictors <- c("equipment_type", "equipment_age", "solar_system",
                     "maintenance_int", "usage_int", "exposure")

model_eq_freq <- eq_freq %>%
  dplyr::select(all_of(c(freq_predictors, "claim_count"))) %>%
  drop_na() %>%
  filter(exposure > 0)
freq_numeric <- c("equipment_age", "maintenance_int", "usage_int")
model_eq_freq[freq_numeric] <- scale(model_eq_freq[freq_numeric])
model_eq_freq$log_exposure <- log(model_eq_freq$exposure)

model_eq_sev <- eq_sev %>%
  dplyr::select(equipment_type, equipment_age, solar_system,
                maintenance_int, usage_int, exposure, claim_amount) %>%
  drop_na()

#### Frequency Modelling #######################################################
# Stepwise variable selection on Poisson baseline ──
null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_eq_freq
)

full_freq <- glm(
  claim_count ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_eq_freq
)
summary(full_freq)

step_back <- stepAIC(full_freq, direction = "backward", trace = FALSE)
summary(step_back)

step_fwd <- stepAIC(
  null_freq,
  scope = list(
    lower = ~1,
    upper = ~ equipment_type + equipment_age + solar_system +
      maintenance_int + usage_int + offset(log(exposure))
  ),
  direction = "forward",
  trace = FALSE
)
summary(step_fwd)

AIC(full_freq, step_back, step_fwd)

# Poisson GLM (leading frequency model) ──
pois_freq <- glm(
  claim_count ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_eq_freq
)
summary(pois_freq)

# Zero-Inflated Poisson (robustness check) ──
zip_freq <- zeroinfl(
  claim_count ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int + offset(log_exposure) |
    equipment_type,
  data = model_eq_freq,
  dist = "poisson"
)
summary(zip_freq)

AIC(pois_freq, zip_freq)

#### Severity Modelling ########################################################
# Scale numeric predictors for numerical stability
sev_numeric <- c("equipment_age", "maintenance_int", "usage_int", "exposure")
model_eq_sev[sev_numeric] <- scale(model_eq_sev[sev_numeric])

gamma_sev <- glm(
  claim_amount ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int,
  family = Gamma(link = "log"),
  data   = model_eq_sev
)
summary(gamma_sev)

lognormal_sev <- glm(
  log(claim_amount) ~ equipment_type + equipment_age + solar_system +
    maintenance_int + usage_int,
  family = gaussian(link = "identity"),
  data   = model_eq_sev
)
summary(lognormal_sev)

AIC(gamma_sev)
AIC(lognormal_sev)

#### Monte Carlo Simulation: Aggregate Losses ##################################
set.seed(2026)
n_sim <- 10000

lambda_hat <- predict(pois_freq, type = "response")
n_pol      <- length(lambda_hat)

# ── Severity inputs from Gamma GLM ──
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(eq_sev$claim_amount) / sev_shape   # uses unscaled original mean

# ── Simulation loop ──
aggregate_loss <- numeric(n_sim)

for (s in 1:n_sim) {
  
  freq_draw    <- rpois(n_pol, lambda = lambda_hat)
  total_claims <- sum(freq_draw)
  
  if (total_claims > 0) {
    aggregate_loss[s] <- sum(rgamma(total_claims,
                                    shape = sev_shape,
                                    scale = sev_scale))
  }
}

# ── Risk Metrics ──
EF_mean_loss <- mean(aggregate_loss)
EF_sd_loss   <- sd(aggregate_loss)
EF_VaR_99    <- quantile(aggregate_loss, 0.99)
EF_TVaR_99   <- mean(aggregate_loss[aggregate_loss > EF_VaR_99])

cat("\n--- Equipment Failure Risk Metrics ---\n")
cat(sprintf("Mean Loss  : %.0f\n", EF_mean_loss))
cat(sprintf("SD Loss    : %.0f\n", EF_sd_loss))
cat(sprintf("VaR  99%%   : %.0f\n", EF_VaR_99))
cat(sprintf("TVaR 99%%   : %.0f\n", EF_TVaR_99))

#### Tail Behaviour ############################################################
# Empirical tail quantiles ──
tail_quantiles <- quantile(eq_sev$claim_amount,
                           probs = c(0.90, 0.95, 0.975, 0.99, 0.995, 1.00))
print(round(tail_quantiles))

# 9b. Log-Normal tail fit (MLE) ──
lnorm_fit <- fitdistr(eq_sev$claim_amount, "lognormal")
print(lnorm_fit)

lnorm_tail <- data.frame(
  quantile  = c(0.90, 0.95, 0.99),
  empirical = quantile(eq_sev$claim_amount, c(0.90, 0.95, 0.99)),
  lognormal = qlnorm(c(0.90, 0.95, 0.99),
                     meanlog = lnorm_fit$estimate["meanlog"],
                     sdlog   = lnorm_fit$estimate["sdlog"])
)
print(round(lnorm_tail))

# Gamma tail fit (MLE) ──
gamma_start <- list(shape = (mean(eq_sev$claim_amount) / sd(eq_sev$claim_amount))^2,
                    scale =  sd(eq_sev$claim_amount)^2  / mean(eq_sev$claim_amount))
gamma_fit <- fitdistr(eq_sev$claim_amount, "gamma", start = gamma_start)
print(gamma_fit)

gamma_tail <- data.frame(
  quantile  = c(0.90, 0.95, 0.99),
  empirical = quantile(eq_sev$claim_amount, c(0.90, 0.95, 0.99)),
  gamma     = qgamma(c(0.90, 0.95, 0.99),
                     shape = gamma_fit$estimate["shape"],
                     scale = gamma_fit$estimate["scale"])
)
print(round(gamma_tail))

# AIC comparison ──
x <- eq_sev$claim_amount

aic_lnorm <- -2 * sum(dlnorm(x,
                             meanlog = lnorm_fit$estimate["meanlog"],
                             sdlog   = lnorm_fit$estimate["sdlog"],
                             log = TRUE)) + 2 * 2
aic_gamma <- -2 * sum(dgamma(x,
                             shape = gamma_fit$estimate["shape"],
                             scale = gamma_fit$estimate["scale"],
                             log = TRUE)) + 2 * 2

cat(sprintf("AIC log-Normal : %.1f\n", aic_lnorm))
cat(sprintf("AIC Gamma      : %.1f\n", aic_gamma))
cat(sprintf("Preferred model: %s\n",
            ifelse(aic_lnorm < aic_gamma, "Log-Normal", "Gamma")))

# Mean excess plot (visual diagnostic) ──
meplot(eq_sev$claim_amount,
       main = "Mean Excess Plot – Equipment Failure (bounded at 790K)")

#### Tail Dependence: Copula ###################################################
freq_raw <- model_eq_freq$claim_count
freq_pos <- freq_raw[freq_raw > 0]
sev_raw  <- eq_sev$claim_amount

n_dep <- min(length(freq_pos), length(sev_raw))

dep_df <- data.frame(
  f = freq_pos[1:n_dep],
  s = sev_raw[1:n_dep]
)

# ── Pseudo-observations ──
set.seed(2026)
f_jitter <- dep_df$f + runif(n_dep, -0.5, 0.5)

u_dep <- rank(f_jitter) / (n_dep + 1)
v_dep <- rank(dep_df$s) / (n_dep + 1)

# Automatic copula family selection
fit_cop <- BiCopSelect(u_dep, v_dep, familyset = NA)
summary(fit_cop)

# ── Copula-based simulation ──
set.seed(2026)
n_sim_cop <- 500000

cop_sim  <- BiCopSim(n_sim_cop, fit_cop$family, fit_cop$par, fit_cop$par2)
u_sim    <- cop_sim[, 1]
v_sim    <- cop_sim[, 2]

# Map uniforms to marginals
# Poisson is the selected frequency model; use qpois() for the quantile transform.
lambda_all   <- predict(pois_freq, type = "response")

freq_cop_sim <- qpois(u_sim,
                      lambda = sample(lambda_all, n_sim_cop, replace = TRUE))

agg_cop <- vapply(freq_cop_sim, function(n_claims) {
  if (n_claims > 0L) sum(rgamma(n_claims, shape = sev_shape, scale = sev_scale))
  else 0
}, numeric(1))

EF_VaR_cop   <- quantile(agg_cop, 0.99)
EF_TVaR_cop  <- mean(agg_cop[agg_cop > EF_VaR_cop])

cat("\n--- Copula-Based Risk Metrics ---\n")
cat(sprintf("VaR  99%%  : %.0f\n", EF_VaR_cop))
cat(sprintf("TVaR 99%%  : %.0f\n", EF_TVaR_cop))

# Extreme quantiles
cat(sprintf("VaR  99.5%% (Solvency)     : %.0f\n", quantile(agg_cop, 0.995)))
cat(sprintf("VaR  99.9%% (Catastrophic) : %.0f\n", quantile(agg_cop, 0.999)))

# ── Copula family comparison plots ──
par(mfrow = c(2, 2))

fit_gauss <- BiCopEst(u_dep, v_dep, family = 1)
sim_gauss <- BiCopSim(5000, 1, fit_gauss$par)
plot(sim_gauss, main = "Gaussian Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(0, 0, 1, 0.2))

fit_clay <- BiCopEst(u_dep, v_dep, family = 3)
sim_clay <- BiCopSim(5000, 3, fit_clay$par)
plot(sim_clay, main = "Clayton Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(1, 0, 0, 0.2))

fit_gumb <- BiCopEst(u_dep, v_dep, family = 4)
sim_gumb <- BiCopSim(5000, 4, fit_gumb$par)
plot(sim_gumb, main = "Gumbel Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(0, 0.6, 0, 0.2))

fit_t <- BiCopEst(u_dep, v_dep, family = 2)
sim_t <- BiCopSim(5000, 2, fit_t$par, fit_t$par2)
plot(sim_t, main = "t-Copula",
     xlab = "Frequency", ylab = "Severity", pch = 20, col = rgb(0.5, 0, 0.5, 0.2))

par(mfrow = c(1, 1))

#### Segment Analysis by Equipment Type ########################################
high_wear_types <- c("Ion Pulverizer", "Quantum Bore")

freq_hw   <- model_eq_freq %>% filter(equipment_type %in% high_wear_types)
sev_hw    <- model_eq_sev  %>% filter(equipment_type %in% high_wear_types)
freq_std  <- model_eq_freq %>% filter(!(equipment_type %in% high_wear_types))
sev_std   <- model_eq_sev  %>% filter(!(equipment_type %in% high_wear_types))

# Severity sub-models
sev_model_hw <- glm(claim_amount ~ equipment_age + solar_system +
                      maintenance_int + usage_int,
                    family = Gamma(link = "log"), data = sev_hw)
sev_model_std <- glm(claim_amount ~ equipment_age + solar_system +
                       maintenance_int + usage_int,
                     family = Gamma(link = "log"), data = sev_std)

shape_hw  <- 1 / summary(sev_model_hw)$dispersion
scale_hw  <- mean(eq_sev$claim_amount[eq_sev$equipment_type %in% high_wear_types]) / shape_hw

shape_std <- 1 / summary(sev_model_std)$dispersion
scale_std <- mean(eq_sev$claim_amount[!(eq_sev$equipment_type %in% high_wear_types)]) / shape_std

set.seed(2026)
n_seg <- 50000
agg_hw  <- numeric(n_seg)
agg_std <- numeric(n_seg)

mu_hw  <- mean(predict(pois_freq, newdata = freq_hw,  type = "response"))
mu_std <- mean(predict(pois_freq, newdata = freq_std, type = "response"))

for (s in 1:n_seg) {
  
  f_hw  <- rpois(1, lambda = mu_hw  * nrow(freq_hw))
  f_std <- rpois(1, lambda = mu_std * nrow(freq_std))
  
  if (f_hw  > 0) agg_hw[s]  <- sum(rgamma(f_hw,  shape = shape_hw,  scale = scale_hw))
  if (f_std > 0) agg_std[s] <- sum(rgamma(f_std, shape = shape_std, scale = scale_std))
}

segment_results <- data.frame(
  Metric        = c("Mean Loss", "VaR 99%", "TVaR 99%"),
  High_Wear     = c(mean(agg_hw),
                    quantile(agg_hw, 0.99),
                    mean(agg_hw[agg_hw > quantile(agg_hw, 0.99)])),
  Standard      = c(mean(agg_std),
                    quantile(agg_std, 0.99),
                    mean(agg_std[agg_std > quantile(agg_std, 0.99)]))
)
print(segment_results)

