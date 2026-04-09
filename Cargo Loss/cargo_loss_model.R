library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)
library(stringr)
library(MASS)
library(fpp3)
library(pscl)
library(ggplot2)
library(dplyr)
library(scales)
library(VineCopula)

# -----------------------------
# Load Data
# -----------------------------

cargo_freq <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 1)
cargo_sev  <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 2)

#clean data to be consistent with given value range
cargo_freq <- cargo_freq |>
  filter(
    !is.na(distance) & distance >= 1 & distance <= 100,
    !is.na(cargo_value) & cargo_value >= 50000 & cargo_value <= 680000000,
    !is.na(transit_duration) & transit_duration >= 1 & transit_duration <= 60,
    !is.na(route_risk) & route_risk >= 1 & route_risk <= 5,
    pilot_experience >= 1 & pilot_experience <= 30,
    vessel_age >= 1 & vessel_age <= 50,
    weight >= 1500 & weight <= 250000,
    solar_radiation >= 0 & solar_radiation <= 1,
    debris_density >= 0 & debris_density <= 1,
    exposure >= 0 & exposure <= 1,
    claim_count >= 0 & claim_count <= 5
  )

cargo_sev <- cargo_sev |>
  filter(
    !is.na(distance) & distance >= 1 & distance <= 100,
    !is.na(cargo_value) & cargo_value >= 50000 & cargo_value <= 680000000,
    !is.na(transit_duration) & transit_duration >= 1 & transit_duration <= 60,
    !is.na(route_risk) & route_risk >= 1 & route_risk <= 5,
    pilot_experience >= 1 & pilot_experience <= 30,
    vessel_age >= 1 & vessel_age <= 50,
    weight >= 1500 & weight <= 250000,
    solar_radiation >= 0 & solar_radiation <= 1,
    debris_density >= 0 & debris_density <= 1,
    exposure >= 0 & exposure <= 1,
    claim_amount >= 31000 & claim_amount <= 678000000
  )

# Clean variable names (removes unwanted characters from strings _???XXXX)
cargo_freq <- cargo_freq |>
  mutate(across(
    where(is.character),
    ~ str_trim(str_remove(.x, "_.*"))
  ))

cargo_sev <- cargo_sev |>
  mutate(across(
    where(is.character),
    ~ str_trim(str_remove(.x, "_.*"))
  ))


######################################################################
# Forward Stepwise and Backwards Stepwise Functions
######################################################################

# Data Cleaning
quantile(cargo_sev$claim_amount, probs = c(0.95,0.99,0.995))

model_cargo_freq <- cargo_freq
model_cargo_sev <- cargo_sev


# Fix exposure: replace zeros or NAs
model_cargo_freq$exposure[model_cargo_freq$exposure <= 0 | is.na(model_cargo_freq$exposure)] <- 0.001
model_cargo_freq$claim_count <- round(model_cargo_freq$claim_count)
model_cargo_freq$exposure[model_cargo_freq$exposure <= 0] <- 0.001
model_cargo_freq$log_exposure <- log(model_cargo_freq$exposure)
model_cargo_freq <- model_cargo_freq[!is.na(model_cargo_freq$claim_count), ]


predictors <- c("cargo_type","cargo_value","weight",
                "route_risk","distance",
                "transit_duration","pilot_experience","vessel_age",
                "container_type","solar_radiation","debris_density",
                "exposure")

# Remove any rows with NA in these columns
model_cargo_freq <- model_cargo_freq %>%
  dplyr::filter(!if_any(all_of(predictors), is.na))


######################################################################
# Forward Stepwise and Backwards Stepwise Functions 
######################################################################
null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_cargo_freq
)

full_freq <- glm(
  claim_count ~ cargo_type + cargo_value + weight + route_risk + solar_radiation +
    container_type + debris_density + distance + transit_duration +
    pilot_experience + vessel_age + weight +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_cargo_freq
)
summary(full_freq)

step_backward <- stepAIC(full_freq,
                         direction = "backward")
summary(step_backward)

upper_formula <- claim_count ~ cargo_type + cargo_value + weight + route_risk + solar_radiation +
  container_type + debris_density + distance + transit_duration +
  pilot_experience + vessel_age + weight +
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

######################################################################
# Frequency Model GLM
######################################################################

freq_model <- glm(
  claim_count ~ route_risk + pilot_experience + solar_radiation + debris_density +
    container_type + cargo_type + cargo_value + offset(log(exposure)),
  family = poisson(link = "log"),
  offset = log(exposure),
  data = model_cargo_freq
)

summary(freq_model)

### Negative binomial model
freq_model_nb <- glm.nb(
  claim_count ~ route_risk + pilot_experience + solar_radiation + debris_density +
    container_type + cargo_type + cargo_value + offset(log(exposure)),
  data = model_cargo_freq
)

summary(freq_model_nb)

AIC(freq_model_nb, step_forward, step_backward)

######################################################################
# Severity Model GLM
######################################################################

model_cargo_sev$claim_amount[model_cargo_sev$claim_amount <= 0] <- 0.01
model_cargo_sev <- na.omit(model_cargo_sev)

numeric_vars <- c("route_risk", "solar_radiation", "debris_density",
                  "cargo_value", "weight", "vessel_age",
                  "distance", "transit_duration", "pilot_experience")

model_cargo_sev[numeric_vars] <- scale(model_cargo_sev[numeric_vars])

sev_model <- glm(
  claim_amount ~ route_risk + solar_radiation + debris_density +
    cargo_value + weight + vessel_age +  distance + transit_duration + pilot_experience + container_type + 
    cargo_type,
  family = Gamma(link = "log"),
  data = model_cargo_sev
)

summary(sev_model)
