# KADAK Report: Insurance Pricing and Risk Modelling for Interstellar Mining
## Methods & Data Documentation

> _"Tell me and I forget. Teach me and I remember. Involve me and I learn." – Benjamin Franklin_

---

## Executive Summary

Interstellar mining presents significant economic opportunities but also exposes operators to extreme and unpredictable risks. Cosmic Quarry Mining operates across the Helionis Cluster, Bayesia System, and Oryn Delta, where hazards such as orbital debris, radiation surges, equipment strain and workforce safety challenges can disrupt operations and cause substantial financial losses.

This project outlines how Galaxy General Insurance can enhance financial resilience through four core products: Business Interruption, Equipment Failure, Cargo Loss and Workers’ Compensation. These products collectively address key risks including downtime, infrastructure damage, transport losses and employee injuries.

The insurance solutions are designed to be modular and adaptable, with coverage tailored to system-specific risk profiles. Pricing, limits and deductibles adjust based on operational and environmental factors, while optional features (e.g., precious metal cargo riders) help manage concentration risk. ESG-linked incentives further support sustainable and effective risk management.

---

## Products Developed
Galaxy General's product design is scoped to the three solar systems previously mentioned in which Cosmic Quarry Mining Corporation operates. While the historical claims dataset spans five systems including Epsilon and Zeta, those systems are used only for model calibration and are not subject to coverage under this contract.

### Coverage Areas & Key Parameters

| Product | Primary Exposure | Key Risk Drivers | System-Specific Focus |
|---------|------------------|------------------|----------------------|
| **Business Interruption** | Production disruptions from environmental hazards | energy_backup_score, production_load, safety_compliance | Helionis: supply chain; Bayesia: radiation; Oryn: orbital shear |
| **Cargo Loss** (excl. gold/platinum) | Mineral transport losses | route_risk, debris_density, solar_radiation | All systems; highest concentration in Oryn Delta |
| **Workers' Compensation** | Workforce injuries under hazardous conditions | occupation, psych_stress_index, safety_training_index | All systems; Helionis exhibits highest claim frequency |
| **Equipment Failure** | Complex mining machinery failures | equipment_age, usage_intensity, maintenance_int | Helionis: highest claim rates; Bayesia: radiation-induced failures |

---

## Methodology Overview

### Frequency-Severity Framework

The analysis employs a **collective risk model** combining separate frequency and severity distributions for each hazard line, calibrated to historical claims data via Generalized Linear Models (GLMs).

**Frequency Models** (Claim Counts)
- **Business Interruption**: Negative Binomial GLM (dispersion = 1.73)
- **Cargo Loss**: Negative Binomial GLM (dispersion = 4.51, accommodating high overdispersion)
- **Workers' Compensation**: Poisson GLM (dispersion = 0.115)
- **Equipment Failure**: Poisson GLM (dispersion = 1.0956, near-unity justifies Poisson)

**Severity Models** (Claim Amounts)
- **Business Interruption**: Gamma GLM (log-link), shape = 0.3538, scale = 12.3M
- **Cargo Loss**: Beta GLM on severity ratio (α = 1.3832, β = 14.3850), then scaled by cargo_value
- **Workers' Compensation**: Lognormal (μ = 7.1982, σ = 1.0786)
- **Equipment Failure**: Gamma GLM (log-link), shape = 4.32, scale = 11,100

#### Cargo Loss Frequency Model Specification

```R
freq_formula = (
    'claim_count ~ C(route_risk_cat, Treatment(reference="1"))'
    ' + C(container_type_cat, Treatment(reference="longhaul vault canister"))'
    ' + debris_density_z + pilot_experience_z + solar_radiation_z'
)

freq_glm = smf.glm(
    formula=freq_formula,
    data=freq_model_df,
    family=sm.families.NegativeBinomial()
).fit()

# Parameters: r = 0.5370, p = 0.6868
```

**Key Drivers**: Route risk (primary), debris density, pilot experience, solar radiation, container type

#### Equipment Failure Frequency Model Specification

```R
freq_formula = (
    'claim_count ~ '
    'C(equipment_type_cat, Treatment(reference="reglaggregators")) + '
    'C(solar_system_cat, Treatment(reference="helionis cluster")) + '
    'equipment_age_z + maintenance_int_z + usage_int_z'
)

freq_glm = smf.glm(
    formula=freq_formula,
    data=freq_model_df,
    family=sm.families.Poisson(),
    offset=freq_model_df['log_exposure']
).fit()

# Parameter: λ = 0.1840
```

**Key Drivers**: Equipment type, solar system, equipment age, maintenance intervals, usage intensity

#### Workers' Compensation Severity Model Specification

```R
model_lognormal = smf.ols(
    formula="""
    log_claim ~ occupation + psych_stress_index + gravity_level + 
                safety_training_index + protective_gear_quality
    """,
    data=sev_clean
).fit()

# Parameters: μ = 7.1982, σ = 1.0786
```

**Key Drivers**: Occupation type, psychological stress, gravity level, safety training, protective gear quality

#### Business Interruption Frequency Model Specification

```R
freq_formula = (
    'claim_count ~ C(solar_system_cat, Treatment(reference="zeta"))'
    ' + C(energy_backup_score_cat, Treatment(reference="1"))'
    ' + supply_chain_index_z'
    ' + maintenance_freq_z'
    ' + avg_crew_exp_z'
)

freq_glm = smf.glm(
    formula=freq_formula,
    data=freq_model_df,
    family=sm.families.NegativeBinomial(),
    offset=freq_model_df['log_exposure']
).fit()

# Parameters: r = 0.1216, p = 0.5472
```

**Key Drivers**: Solar system, energy backup score, supply chain index, maintenance frequency, crew experience

---

### Monte Carlo Simulation Framework

Monte Carlo simulation was conducted to estimate potential losses and capture extreme tail risks that cannot be assessed using simple averages or deterministic methods. By running 100,000 iterations for each line of business, the model generates a wide range of possible outcomes using a collective risk framework; claim frequencies are simulated from fitted distributions (Poisson or Negative Binomial) and claim severities are drawn from appropriate distributions (Gamma, Lognormal, or Beta). These are combined to produce aggregate annual losses, from which key risk metrics such as Value-at-Risk (VaR) and Tail Value-at-Risk (TVaR) at the 95th, 99th and 99.5th percentiles are calculated. An annual inflation adjustment of 2.46% is applied to ensure loss estimates remain realistic over time.

```r
# Collective Risk Model: S = Σ Xi (i = 1 to N)
cargo_loss <- numeric(n_sim)

for(i in 1:n_sim){
  # 1. Simulate claim count from frequency distribution
  n_claims <- rnbinom(1, size = r_cargo, prob = p_cargo)
  
  if(n_claims > 0){
    # 2. Simulate claim severities from fitted severity distribution
    damage_ratio <- rbeta(n_claims, shape1 = a_cargo, shape2 = b_cargo)
    
    # 3. Sample cargo values and compute losses
    sampled_values <- sample(cargo_values, size = n_claims, replace = TRUE)    
    losses <- damage_ratio * sampled_values
    
    # 4. Aggregate annual loss: S = Σ Xi
    cargo_loss[i] <- sum(losses)
  }
}

# 5. Extract tail risk metrics
var95_cargo  <- quantile(cargo_loss, 0.95)
var99_cargo  <- quantile(cargo_loss, 0.99)
var995_cargo <- quantile(cargo_loss, 0.995)
```
---

### Exploratory Data Analysis (EDA)

Exploratory Data Analysis (EDA) was conducted prior to modelling to identify the most significant risk drivers across each line of business and guide model development. The key findings are summarised below:

- Cargo Loss: Claim frequency increases with route risk (tiers 1→5) and debris density, with the highest density band generating around 2× baseline frequency; solar radiation has a secondary effect (approximately 40% uplift). Lithium and cobalt drive severity when excluding gold and platinum.

- Business Interruption: Claim rates decline as exposure increases, while solar system location is a key differentiator (Epsilon > Zeta > Helionis Cluster). Maintenance frequency shows limited impact on total claims.

- Workers’ Compensation: Drill operators have the highest claim frequency and severity. Claim amounts are heavily right-skewed, with more stress-related injuries observed in the Epsilon system and relatively higher rates in Helionis Cluster after adjusting for exposure.

- Equipment Failure: Claim frequency rises with equipment age before plateauing, while severity remains stable. Usage intensity significantly increases both frequency and severity, with certain equipment types consistently generating higher losses.


**EDA Key Findings**:

**Cargo Loss**: 
- Route risk shows near-monotonic increase in claim frequency (tiers 1→5)
- Debris density bands (0.0–1.0) demonstrate similar gradient, with highest-density band generating 2x baseline frequency
- Solar radiation shows secondary effect (approx. 40% uplift for high bands)
- Gold and platinum excluded; lithium and cobalt carry highest severity loading

**Business Interruption**:
- Exposure level inversely correlated with claim rate (0.75 at low exposure → 0.10 at high exposure)
- Maintenance frequency shows little effect on total claims (counterintuitive finding)
- Solar system emerges as strong differentiator (Epsilon > Zeta > Helionis Cluster)

**Workers' Compensation**:
- Drill operators exhibit highest claim frequency and severity across all systems
- Claim amounts heavily right-skewed with maximum of Đ193,357
- Stress-related injuries more prominent in Epsilon system
- Exposure-adjusted results reveal Helionis Cluster has relatively higher claim rates

**Equipment Failure**:
- Claim frequency rises steadily with equipment age (0–10 years), then plateaus
- Severity remains stable across age bands (aging → frequency driver, not severity)
- Usage intensity amplifies both frequency and severity (near-continuous operation worst)
- Equipment type consistent across all three solar systems (Mag-Lift Aggregators, Quantum Bores highest)

---

## Aggregate Loss Results (Baseline)


| Line of Business | Expected Annual Loss | Std Dev | 99% VaR | 99% TVaR |
|------------------|----------------------|---------|---------|----------|
| Business Interruption | Đ2.65 billion | Đ41.4M | Đ2.75B | Đ2.76B |
| Cargo Loss | Đ15.38 billion | Đ153.8M | Đ15.70B | Đ15.73B |
| Workers' Compensation | Đ14.9 million | Đ465K | Đ16.10M | Đ16.28M |
| Equipment Failure | Đ234.04 million | Đ5.13M | Đ246M | Đ248.13M |
| **Portfolio Total** | **Đ17.70 billion** | — | **Đ25.9B** (1-in-200) | **Đ27.8B** (1-in-200) |

Aggregate losses for each portfolio were estimated using Monte Carlo simulation based on calibrated frequency and severity models. The results above show that Business Interruption is a major exposure with significant tail risk, while Workers’ Compensation, despite lower expected losses, is vulnerable to occasional high-severity claims. Equipment Failure and Cargo Loss exhibit higher expected losses overall and remain exposed to adverse tail outcomes, highlighting the importance of managing extreme risk across all lines.

---
---

## Libraries & Technical Stack
### R Libraries (Capital Modeling, Risk Metrics)

```r
library(readxl)          # Excel file reading
library(ggplot2)         # Data visualization
library(copula)          # Copula models for dependency
library(tidyr)           # Data reshaping
library(dplyr)           # Data manipulation
library(fable)           # Forecasting
library(forecast)        # Time series forecasting
library(tsibble)         # Time series tibbles
```

---

## Model Selection & Goodness-of-Fit

### Model Comparison Framework

For each hazard line, multiple distributions were evaluated based on:

1. **AIC / BIC**: Akaike and Bayesian Information Criteria (lower is better)
2. **Adjusted R²**: Explanatory power of covariates
3. **Pearson Dispersion / Pearson Chi-Square**: Goodness-of-fit
4. **Visual Inspection**: Quantile-quantile plots, residual plots

### Frequency Model Selection

| Hazard | Model Tested | Dispersion | Decision | Rationale |
|--------|--------------|-----------|----------|-----------|
| **Cargo Loss** | Poisson | 4.51 | **Rejected** | High overdispersion; counts clustered at extremes |
| **Cargo Loss** | Negative Binomial | 4.51 | Adopted | Accommodates overdispersion; best AIC/BIC |
| **Equipment Failure** | Poisson | 1.0956 | Adopted | Dispersion ≈ 1; simpler model preferred |
| **Equipment Failure** | Negative Binomial | 1.0956 | **Rejected** | Poisson sufficient; no efficiency gain |
| **Workers' Comp** | Poisson | 0.115 | Adopted | Minimal underdispersion; Poisson suitable |
| **Business Interruption** | Poisson | 1.73 | **Rejected** | Overdispersion present; Neg Binom better |
| **Business Interruption** | Negative Binomial | 1.73 | Adopted | Accommodates clustering; best AIC |

### Severity Model Selection

| Hazard | Approach | Distribution | Parameters | Rationale |
|--------|----------|--------------|-----------|-----------|
| **Cargo Loss** | Severity Ratio | Beta GLM | α=1.3832, β=14.3850 | Claim < cargo_value; Beta on (0,1); then rescale |
| **Equipment Failure** | Direct | Gamma GLM (log-link) | shape=4.32, scale=11.1K | Right-skewed; multiplicative covariates |
| **Workers' Comp** | Log-transformed | Lognormal OLS | μ=7.1982, σ=1.0786 | Heavy right tail; fat tails accommodated |
| **Business Interruption** | Direct | Gamma GLM (log-link) | shape=0.3538, scale=12.3M | Right-skewed; operational factors multiplicative |

**Key Finding**: Negative Binomial for frequency outperformed Poisson in 2 of 4 lines due to overdispersion. Gamma and Lognormal severity models captured tail behavior better than exponential alternatives.

---

## Data Limitations & Mitigation Strategies

| Limitation | Description | Severity | Mitigation |
|-----------|-------------|----------|-----------|
| **System Mismatch** | Training data spans Epsilon & Zeta; RFP targets Helionis, Bayesia, Oryn Delta | High | Proxy calibration using encyclopedia; sensitivity analysis |
| **Gold/Platinum Exclusion** | 18,673 severity records vs 85,332 frequency records (4.6:1 ratio) | High | Beta severity model with GLM anchor calibration; subsample bias accepted |
| **No System Tagging** | Cargo claims lack explicit solar system identifiers | Medium | Use route_risk, debris_density, solar_radiation as proxies |
| **Cross-Sectional Data** | No time dimension; cannot model trend or development | Medium | Inflation adjustment at 2.46% p.a.; separate stress testing |
| **Parametric Bounds** | Data dictionary constraints too restrictive; would remove valid observations | Low | Modified to lower bounds only; justified in assumptions |

---

All analysis uses the following datasets:

1. **[Cargo Claims Data](https://www.soa.org/globalassets/assets/files/research/opportunities/2026/student-research-case-study/srcsc-2026-claims-cargo.xlsx)** — Cargo Loss frequency & severity
   - 85,332 frequency records (post-exclusion of gold/platinum)
   - 18,673 severity records (4.6:1 ratio creates subsample bias; mitigated via GLM anchor calibration)
   - Variables: route_risk, debris_density, solar_radiation, cargo_type, cargo_value, weight, transit_duration, pilot_experience, container_type, vessel_age, distance

2. **[Business Interruption Claims Data](https://www.soa.org/globalassets/assets/files/research/opportunities/2026/student-research-case-study/srcsc-2026-claims-business-interruption.xlsx)** — BI frequency & severity
   - Frequency data: solar_system, production_load, energy_backup_score, supply_chain_index, avg_crew_exp, maintenance_freq, safety_compliance
   - Severity data: claim amounts (in Đ millions)
   - Exposure metric for offset modeling

3. **[Workers' Compensation Calims Data](https://www.soa.org/globalassets/assets/files/research/opportunities/2026/student-research-case-study/srcsc-2026-claims-workers-comp.xlsx)** — Workers' Compensation frequency & severity
   - Frequency: occupation, accident_history_flag, psych_stress_index, safety_training_index, solar_system
   - Severity: wage-replacement basis, claim_length (duration in days), employment_type, supervision_level, protective_gear_quality
   - Claims range: Đ120.76 (minimum) to Đ193,357.20 (maximum per occurrence)

4. **[Equipment Failure Claims Data](https://www.soa.org/globalassets/assets/files/research/opportunities/2026/student-research-case-study/srcsc-2026-claims-equipment-failure.xlsx)** — Equipment Failure frequency & severity
   - Frequency: equipment_type (Quantum Bore, Mag-Lift Aggregator, FluxStream Carrier, Fusion Transport, Ion Pulverizer, Graviton Extractor)
   - Severity: equipment_age (0–10 years), maintenance_int (100–5,000 hours), usage_int (0–24 hours/day)
   - Claim counts & amounts by system and equipment class

5. **[Cosmic Quarry Inventory Data](https://www.soa.org/globalassets/assets/files/research/opportunities/2026/student-research-case-study/srcsc-2026-cosmic-quarry-inventory.xlsx)** — Exposure Bases
   - Equipment inventory: approx. 505 units across three systems (exposure base for EF frequency model)
   - Personnel headcount: approx. 86,765 workers across three systems (exposure base for WC frequency model)
   - System-level operational metrics (production_load, supply_chain_index, etc.)

6. **[Interest and Inflation Rates](https://www.soa.org/globalassets/assets/files/research/opportunities/2026/student-research-case-study/srcsc-2026-interest-and-inflation.xlsx)** — Macroeconomic Assumptions
   - Historical inflation rates, overnight bank lending rates, 1-year and 10-year risk-free spot rates
   - ARIMA forecasts for 10-year projection horizon
   - Inflation rate (baseline): 4.23% per annum
   - Short-term investment rate: 3.71%
   - Long-term discount rate: 3.76%

---

## Key Modeling Decisions & Rationale

### GLM Specification

Generalised Linear Models (GLMs) were used to model both claim frequency and severity across all lines of business, with specifications chosen to reflect the underlying risk characteristics observed in the data. An exposure offset was incorporated into all frequency models to ensure that claim counts scale appropriately with the level of operational activity (e.g. time in operation or production volume). This allows for consistent comparison across systems with differing exposure levels.

For claim frequency, distributional assumptions were selected based on dispersion properties. Negative Binomial models were applied to Business Interruption, Workers’ Compensation, and Cargo Loss, as these portfolios exhibited overdispersion driven by occasional high-frequency events. In contrast, Equipment Failure was modelled using a Poisson distribution, as its claim counts showed relatively stable variance and did not require the additional flexibility of a Negative Binomial structure.

For claim severity, distributions were chosen to capture the right-skewed and heavy-tailed nature of losses. Gamma distributions were used for Cargo Loss and Equipment Failure, where claim sizes are strongly influenced by operational factors such as cargo value, weight, and usage intensity. Lognormal distributions were selected for Business Interruption and Workers’ Compensation, reflecting the multiplicative nature of disruption costs and wage-related claims, which tend to produce more extreme high-end outcomes.

### Data Limitations & Mitigations

| Limitation | Impact | Mitigation |
|-----------|--------|-----------|
| Gold/platinum exclusion reduces CL severity data to 18,673 of 85,332 frequency observations | Severity model trained on non-random subsample; potential underweighting of low-value claims | GLM anchor calibration ties severity scale to mean(claim_amount)/sev_shape |
| No explicit multi-system claim tagging in cargo data | System-specific pricing relies on route_risk, debris_density, solar_radiation as proxies rather than direct system identifiers | Route_risk tiers (1–5) calibrated to three systems via encyclopedia; Oryn Delta's tail risk stress-tested separately |
| Historical data spans Epsilon & Zeta systems (not in RFP scope) | Frequency intercepts potentially diluted by external systems' operating conditions | Conservatively accepted; if Epsilon/Zeta have lower risk, estimates are conservative bias |
| Cross-sectional data lacks time dimension | Cannot model claims trend, development, or latent demand surge | Inflation loading stress-tested up to 40% separately; claims inflation driver included in stress scenarios |
| Some data dictionary parameter bounds too restrictive | Would remove excessive valid observations during cleaning | Modified to use lower bounds only (e.g., Workers' Compensation) with documented justification |

---

## Pricing & Financial Framework

### Premium Calculation Methodology

**Technical Premium Formula**:
```
Technical Premium = (Pure Premium + Risk Margin) / (1 – Expense Ratio – Profit Margin)
```

**Standard Assumptions**:
- **Expense Ratio**: 15% (administration, reinsurance, commissions)
- **Profit Margin**: 10% (based on industry benchmarks and tail risk)
- **Risk Margin**: 5% of pure premium (uncertainty loading for parameter uncertainty)
- **Implied Load Factor**: 1.176 (yielding 75% permissible loss ratio)

**Inflation Adjustment**:
```R
# Applied to all severity distributions at 2.46% per annum
inflation_rate = 0.0246
claim_severity_adjusted = claim_severity * (1 + inflation_rate) ** year
```

---

### 10-Year Present Value Summary

| Line of Business | PV Premiums (Đ) | PV Losses (Đ) | PV Expenses (Đ) | PV Profit (Đ) | Profit Margin |
|------------------|-----------------|--------------|-----------------|---------------|---------------|
| Business Interruption | 41.01B | 24.62B | 12.30B | 4.09B | 10.0% |
| Cargo Loss (ex. gold/platinum) | 2,379.78B | 1,427.87B | 713.93B | 237.98B | 10.0% |
| Equipment Failure | 39.06B | 23.56B | 11.72B | 3.78B | 9.7% |
| Workers' Compensation | 230.7M | 138.4M | 69.2M | 23.1M | 10.0% |
| **Portfolio Total** | **2,831.24B** | **1,699.01B** | **849.37B** | **282.86B** | **10.0%** |

### Capital Adequacy

**Recommended Reserves** (99% TVaR-based):
- Business Interruption: Đ2.76B (15% of portfolio reserves)
- Cargo Loss: Đ159.20B (84% of portfolio reserves, reflecting tail concentration)
- Equipment Failure: Đ2.77B (1.5% of portfolio reserves)
- Workers' Compensation: Đ18.43M (0.1% of portfolio reserves)
- **Total Portfolio Reserve**: Đ189.76B (10-year PV basis)

---

## Stress Testing Results & Code

### Stress Test Methodology

Two complementary stress scenarios were implemented:

```R
# Stress testing: Frequency and Severity Shocks
stress_scenarios = {
    'Baseline': {'freq_multiplier': 1.0, 'sev_multiplier': 1.0},
    'Moderate Stress': {'freq_multiplier': 1.25, 'sev_multiplier': 1.10},
    'Extreme Stress': {'freq_multiplier': 1.50, 'sev_multiplier': 1.30}
}

for scenario_name, multipliers in stress_scenarios.items():
    # Apply multipliers to simulated claim counts and severities
    n_claims_stressed = int(n_claims * multipliers['freq_multiplier'])
    losses_stressed = losses * multipliers['sev_multiplier']
    
    # Recalculate VaR metrics
    var99_stressed = np.quantile(aggregate_loss_stressed, 0.99)
    tvar99_stressed = aggregate_loss_stressed[
        aggregate_loss_stressed >= var99_stressed
    ].mean()
```

### Stress Testing Results Summary

![Actuarial Control Cycle](/Stress%20Testing/Equipment%20Failure/stress_test.png)

| Product | Baseline VaR 99% | Moderate Stress (+25% freq, +10% sev) | Extreme Stress (+50% freq, +30% sev) | Change |
|---------|-----------------|----------------------------------------|---------------------------------------|--------|
| Equipment Failure | $102,773 | $129,148 | $157,252 | +53% |
| Business Interruption | $59,961,210 | $76,315,150 | $94,751,300 | +58% |
| Cargo Loss | $13,684,390 | $18,918,430 | $26,962,710 | +97% |
| Workers' Compensation | $1,673 | $2,494 | $3,430 | +105% |

**Key Insight**: Business Interruption and Cargo Loss exhibit the steepest VaR amplification under stress, revealing these two products as the primary capital drivers and ideal candidates for stop-loss reinsurance protection.

### Cargo Loss Stress Scenarios

```R
# Great Flare Scenario: Correlated solar storm across all systems
# Frequency +50%, Severity +50% simultaneously

cargo_stress_results = {
    'Baseline': {'mean': 1,978,730, 'var99': 13,684,390, 'tvar99': 21,261,136},
    'Frequency +50%': {'mean': 2,968,095, 'var99': 20,526,585, 'tvar99': 31,891,704},
    'Severity +50%': {'mean': 2,968,095, 'var99': 20,526,585, 'tvar99': 31,891,704},
    'Combined +50%': {'mean': 4,457,142, 'var99': 30,789,878, 'tvar99': 47,837,551}
}

# Great Flare produces 124% increase in tail risk
flare_vir99_increase = (47,837,551 - 21,261,136) / 21,261,136  # +124%
```

---

## Correlated Risk Scenarios & Copula Analysis

### Gaussian Copula Modeling

A Gaussian copula framework is implemented to capture dependency between hazard lines arising from systemic shocks (e.g. solar storms impacting multiple portfolios simultaneously). This approach allows the marginal frequency–severity models for each line of business to be combined into a joint distribution, preserving their individual characteristics while introducing realistic cross-line correlations.

By modelling these dependencies, the framework provides more accurate aggregate loss estimates and improves the assessment of portfolio-level tail risk, particularly under extreme but plausible scenarios where multiple hazard exposures are affected at the same time.

```r
# Gaussian Copula: Dependency Structure Testing
# Tested Clayton, Gumbel, t-copula, and Gaussian

library(copula)

# Fit Gaussian copula with varying correlation levels
rho_values <- seq(-0.5, 0.5, by = 0.1)

for(rho in rho_values) {
    
    # Generate correlated uniform samples
    normal_copula <- normalCopula(rho, dim = 4)
    u_correlated <- rCopula(50000, normal_copula)
    
    # Map to hazard-specific marginal distributions
    bi_loss <- qgamma(u_correlated[, 1], shape = bi_shape, scale = bi_scale)
    cl_loss <- qgamma(u_correlated[, 2], shape = cl_shape, scale = cl_scale)
    wc_loss <- qgamma(u_correlated[, 3], shape = wc_shape, scale = wc_scale)
    ef_loss <- qgamma(u_correlated[, 4], shape = ef_shape, scale = ef_scale)
    
    # Aggregate portfolio loss under correlation
    portfolio_loss <- bi_loss + cl_loss + wc_loss + ef_loss
    
    # Extract VaR metrics
    var_quantiles[rho] <- quantile(portfolio_loss, 0.995)
}
```

**Results**: Great Flare scenario (ρ = +0.5) adds approximately **Đ530 million** in capital requirement at the 1-in-200-year level, driven by simultaneous disruption across all three systems and all four hazard lines.

---

---

## Risk Assessment & System-Specific Profiles

### Helionis Cluster
**Profile**: High-Traffic Frequency Hub
- **BI Risk**: High variability in losses; supply chain disruption primary exposure
- **CL Risk**: Highest debris density (avg 1.13g), but moderate claim severity
- **WC Risk**: Moderate frequency; serious injuries (amputations, burns) drive severity
- **EF Risk**: Lowest claim rates; suggests stronger maintenance discipline

### Bayesia System
**Profile**: Radiative Tail Risk
- **BI Risk**: Radiation shutdown events; moderate frequency, high variability
- **EF Risk**: Highest equipment claim volumes (Mag-Lift Aggregators, Quantum Bores)
- **CL Risk**: Solar radiation index (≥0.7) activates radiation endorsement surcharge
- **WC Risk**: Slightly elevated claim frequency; stress-related injuries prominent

### Oryn Delta
**Profile**: Severity Frontier
- **CL Risk**: Longest transit distances (up to 240 AU); 60-month durations; 2.5% inflation compounds severity by 13.1% per transit
- **Orbital Shear Risk**: Asymmetric asteroid ring causes "crushing" events; equipment losses maximized
- **WC Risk**: Rapid expansion outpaces safety governance; safety_training_index = 2.8/5 (low)
- **BI Risk**: Orbital-shear blackout scenarios; correlated with cargo losses

---

## Scenario Testing (3-Year & 10-Year Horizons)

| Scenario | Annual Technical Premium | 3-Year Expected Cost | 3-Year Net Revenue | 10-Year 99% VaR (Costs) |
|----------|---------------------------|----------------------|-------------------|-------------------------|
| **Best Case** (Attritional claims, stable routes) | Đ20.3B | Đ37.4B | Đ50.9B | — |
| **Moderate Case** (Baseline GLM fit) | Đ26.5B | Đ49.2B | Đ30.3B | — |
| **Worst Case** (Great Flare + debris cascade) | Đ55.6B | Đ115.9B | Đ23.4B | **Đ2.56T** |

---

## Methodological Innovations

### 1. Proxy Calibration for System Risk
Mapped historical claims from Epsilon & Zeta to target systems (Helionis, Bayesia, Oryn Delta) using:
- Route risk tiers (1–5) calibrated to debris/collision rates
- Solar radiation bands (0–1) tied to star characteristics (G2V, binary EM, M3V)
- Supply chain indices reflecting infrastructure maturity

### 2. Severity Subsample Handling
Gold/platinum exclusion created asymmetric frequency-severity dataset (4.6:1 ratio):
- Applied GLM anchor calibration: `sev_scale = mean(claim_amount) / sev_shape`
- Tested sensitivity across 8 cargo severity drivers (Appendix D.2)
- Result: robust estimates despite data imbalance

### 3. Copula-Based Correlated Catastrophe Modeling
**Great Flare Scenario** (portfolio-wide solar storm):
- Simultaneous frequency doubling & severity doubling across all systems
- Gaussian copula with ρ = +0.5 captures systemic contagion
- Produces 1-in-200 VaR of Đ26.43B vs Đ25.90B independent (additional capital need: Đ530M)

### 4. 10,000-Iteration Stochastic Stability
Monte Carlo simulations run 10,000 times per line to ensure:
- 99th & 99.5th percentile stability (< ±0.5% variation between runs)
- Accurate tail quantile estimation for capital adequacy
- Validated coefficient of variation < 5% at extreme tail levels

---

## Key Deliverables
**Four Pricing Models**: Frequency & severity GLMs fully specified with rating factors  
**Monte Carlo Framework**: 10,000 iterations per line generating stable tail estimates  
**Stress Testing Suite**: 30+ scenarios covering frequency, severity, operational, workforce, and systemic risks  
**Copula Modeling**: Dependency structure & correlated catastrophe assessment  
**10-Year Financial Projections**: Present value of premiums, losses, expenses, reserves, and profit  
**Capital Adequacy Assessment**: 99% TVaR-based reserves ensuring 1-in-200-year solvency  
**System-Specific Guidance**: Tailored deductibles, endorsements, and risk loadings by solar system  

---

## Assumptions & Documentation

### Core Assumptions
- **Frequency-Severity Independence**: Limited evidence of dependence within hazard lines; modeled via copula where applicable
- **Stable Underwriting & Portfolio Mix**: Assumes consistent claims experience and exposure composition over projection
- **Economic Projections**: ARIMA-based forecasts for inflation and discount rates (10-year horizon)
- **Deductible Bands**: Cargo debris_density thresholds (0.6, 0.8) applied consistently across systems
- **Reinsurance Exclusions**: Gold & platinum shipments excluded from base policy; cobalt & lithium retained with high severity loading

### Secondary Assumptions
- Claims settle within same financial period (no long-tail development modeled)
- Parameter uncertainty not explicitly modeled
- Investment returns on reserves not modeled
- Catastrophe probability scenarios (0 to 3%) overlaid separately on baseline

---


## Appendices & References

All detailed analyses, visualizations, and supplementary calculations documented in:
- **Appendix A**: Full rating factor structures (GLM formulas, deductible schedules)
- **Appendix B**: Pricing, profitability, and cash flow projections
- **Appendix C**: Dependency & copula testing
- **Appendix D**: Comprehensive stress testing results (all four hazards)
- **Appendix E**: Exploratory data analysis & risk identification by system
- **Appendix F**: Correlated risk scenario results
- **Appendix G**: Threat table & risk ranking
- **Appendix H**: Secondary assumptions & computational efficiency notes
- **Appendix I**: Data sources & limitations detail

**Code & Reproducibility**:
- All models fitted using R base/specialized packages
- Monte Carlo simulations: 100,000 iterations per line for tail stability
- Model selection: AIC/BIC, Pearson dispersion, visual diagnostics
- Stress testing: Multiplicative frequency & severity shocks across baseline scenarios

---

## Full Report

**[ Download the Complete KADAK Report (PDF)](KADAK_Report.pdf)**

The full technical report includes all appendices, detailed analyses, visualizations, and supplementary calculations referenced throughout this documentation.

---

**For questions or additional information, refer to the complete KADAK Report document.**
