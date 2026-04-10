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
library(copula)

# Import datasets
worker_data_freq <- read_excel("srcsc-2026-claims-workers-comp.xlsx", sheet = 1)
worker_data_sev <- read_excel("srcsc-2026-claims-workers-comp.xlsx", sheet = 2)

##################################################################
# DATA CLEANING
##################################################################
clean_data <- function(df) {
  df |>
    filter(!is.na(solar_system), !is.na(occupation)) |>
    arrange(solar_system)
}

worker_data_freq <- clean_data(worker_data_freq)
worker_data_sev  <- clean_data(worker_data_sev)

## add claim_count to sev dataset
worker_data_sev <- worker_data_sev %>%
  left_join(worker_data_freq %>%
              dplyr::select(policy_id, worker_id, claim_count),
            by = c("policy_id", "worker_id"))

# Align all variables with provided value range / levels
worker_data_sev <- worker_data_sev |>
  filter(
    experience_yrs >= 0,
    accident_history_flag >= 0,
    psych_stress_index >= 1,
    hours_per_week >= 20,
    supervision_level >= 0,
    gravity_level >= 0.75,
    safety_training_index >= 1,
    protective_gear_quality >= 1,
    base_salary >= 20000,
    exposure >= 0,
    claim_count >= 0,
    claim_length >= 3,
    claim_amount >= 5
  )

worker_data_sev <- worker_data_sev |>
  mutate(across(
    where(~ is.character(.x) | is.factor(.x)),
    ~ str_remove(as.character(.x), "_.*$")
  ))

worker_data_freq <- worker_data_freq |>
  mutate(across(
    where(~ is.character(.x) | is.factor(.x)),
    ~ str_remove(as.character(.x), "_.*$")
  ))

model_worker_freq <- worker_data_freq
model_worker_sev <- worker_data_sev

# Fix exposure: replace zeros or NAs
model_worker_freq$exposure[model_worker_freq$exposure <= 0 | is.na(model_worker_freq$exposure)] <- 0.001
model_worker_freq$claim_count <- round(model_worker_freq$claim_count)
model_worker_freq$exposure[model_worker_freq$exposure <= 0] <- 0.001
model_worker_freq$log_exposure <- log(model_worker_freq$exposure)
model_worker_freq <- model_worker_freq[!is.na(model_worker_freq$claim_count), ]

predictors <- c("occupation","employment_type","experience_yrs",
                "accident_history_flag","psych_stress_index",
                "hours_per_week","supervision_level","gravity_level",
                "safety_training_index","protective_gear_quality","base_salary",
                "exposure")

# Remove any rows with NA in these columns
model_worker_freq <- model_worker_freq %>%
  dplyr::filter(!if_any(all_of(predictors), is.na))

############################################################
# Modelling
############################################################

#######################################
# Forward Stepwise and Backwards Stepwise Functions 
#######################################
null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_worker_freq
)

full_freq <- glm(
  claim_count ~ occupation + employment_type + experience_yrs +
    accident_history_flag + psych_stress_index + hours_per_week +
    supervision_level + gravity_level + safety_training_index +
    protective_gear_quality + base_salary +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_worker_freq
)
summary(full_freq)

step_backward <- stepAIC(full_freq,
                         direction = "backward")
summary(step_backward)

upper_formula <- claim_count ~ occupation + employment_type + experience_yrs +
  accident_history_flag + psych_stress_index + hours_per_week +
  supervision_level + gravity_level + safety_training_index +
  protective_gear_quality + base_salary +
  offset(log(exposure))

# Forward stepwise selection
step_forward <- stepAIC(
  null_freq,
  scope = list(
    lower = ~1,
    upper = upper_formula
  ),
  direction = "forward"
)

summary(step_forward)

AIC(full_freq, step_forward, step_backward)


#######################################
# Frequency
#######################################
# GLM
glm_freq <- glm(
  claim_count ~ occupation + solar_system + accident_history_flag + psych_stress_index +
    safety_training_index + offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_worker_freq
)

summary(glm_freq)


### Negative binomial model
freq_model_nb <- glm.nb(
  claim_count ~ occupation + solar_system + accident_history_flag + psych_stress_index +
    safety_training_index + offset(log(exposure)),
  data = model_worker_freq
)

summary(freq_model_nb)

# ZINB Model
zinb_model <- zeroinfl(
  claim_count ~ occupation + accident_history_flag + psych_stress_index + safety_training_index + solar_system |
    employment_type,
  data = model_worker_freq,
  dist = "negbin",
  offset = log(exposure + 0.001)
)
summary(zinb_model)

nb_freq <- glm.nb(
  claim_count ~ occupation + solar_system + employment_type +
    accident_history_flag + psych_stress_index + safety_training_index +
    supervision_level + protective_gear_quality + base_salary,
  data = worker_data_freq
)

summary(nb_freq)

#######################################
# Severity GLM
#######################################
model_worker_sev$claim_count <- round(model_worker_sev$claim_count)
worker_data_sev$claim_amount[worker_data_sev$claim_amount <= 0] <- 0.01
worker_data_sev <- na.omit(worker_data_sev)

numeric_vars <- c("experience_yrs", "psych_stress_index", "hours_per_week",
                  "supervision_level", "gravity_level", "safety_training_index",
                  "protective_gear_quality", "base_salary", "exposure")

worker_data_sev[numeric_vars] <- scale(worker_data_sev[numeric_vars])

gamma_sev <- glm(
  claim_amount ~ occupation + solar_system + employment_type +
    accident_history_flag + psych_stress_index + supervision_level + 
    protective_gear_quality + base_salary + claim_length,
  family = Gamma(link = "log"),
  data = worker_data_sev
)

summary(gamma_sev)

lognormal_sev <- glm(
  log(claim_amount) ~ occupation + solar_system + employment_type +
    accident_history_flag + psych_stress_index + supervision_level +
    protective_gear_quality + base_salary + claim_length,
  family = gaussian(link = "identity"),
  data = worker_data_sev
)

summary(lognormal_sev)

#######################################
# Monte Carlo Simulation (Independent)
#######################################

set.seed(123)

n_sim <- 10000

# --- Frequency model parameters ---
lambda_hat <- predict(nb_freq, type = "response")
theta_hat  <- nb_freq$theta

n_pol <- length(lambda_hat)

# --- Severity model parameters (Gamma approximation) ---
sev_shape <- 1 / summary(lognormal_sev)$dispersion
sev_mean  <- mean(worker_data_sev$claim_amount)
sev_scale <- sev_mean / sev_shape

# Storage objects
aggregate_loss <- numeric(n_sim)
freq_sim <- numeric(n_sim)
sev_sim  <- numeric(n_sim)

for(s in 1:n_sim){
  
  # Simulate frequency for all policies
  freq_vec <- rnbinom(n_pol,
                      size = theta_hat,
                      mu   = lambda_hat)
  
  total_claims <- sum(freq_vec)
  
  freq_sim[s] <- total_claims
  
  if(total_claims > 0){
    
    severities <- rgamma(total_claims,
                         shape = sev_shape,
                         scale = sev_scale)
    
    total_loss <- sum(severities)
    
    sev_sim[s] <- mean(severities)
    
    aggregate_loss[s] <- total_loss
    
  } else {
    
    sev_sim[s] <- 0
    aggregate_loss[s] <- 0
    
  }
}

# --- Risk Metrics ---
WC_mean_loss <- mean(aggregate_loss)
WC_sd_loss   <- sd(aggregate_loss)

WC_VaR_99  <- quantile(aggregate_loss, 0.99)
WC_TVaR_99 <- mean(aggregate_loss[aggregate_loss > WC_VaR_99])

WC_mean_loss
WC_sd_loss
WC_VaR_99
WC_TVaR_99