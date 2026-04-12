# KADAK Report: Insurance Pricing and Risk Modelling for Interstellar Mining
## Methods & Data Documentation

> _"Tell me and I forget. Teach me and I remember. Involve me and I learn." – Benjamin Franklin_

---

## Executive Overview

This project develops a comprehensive insurance pricing strategy for **Galaxy General Insurance Company** to cover the operational risks of **Cosmic Quarry Mining Corporation** across three solar systems: **Helionis Cluster**, **Bayesia System**, and **Oryn Delta**. Four primary insurance products address material exposures: Business Interruption (BI), Cargo Loss (CL), Workers' Compensation (WC), and Equipment Failure (EF).

The analysis synthesizes advanced actuarial modeling, statistical estimation, and enterprise risk management frameworks to deliver robust pricing, capital adequacy assessment, and tail risk quantification for extreme but rare events.

---

## Products Developed

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

**100,000 Monte Carlo iterations** per line of business generate stable tail risk estimates through the collective risk model:

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

**Process**:
1. Claim counts drawn from fitted frequency distribution (Poisson or Negative Binomial)
2. Individual claim severities sampled from fitted severity distribution (Gamma, Lognormal, or Beta)
3. Aggregate annual loss computed via collective risk formula: **S = Σ Xi** (i = 1 to N)
4. 95th, 99th, and 99.5th percentiles extracted for Value-at-Risk (VaR) and Tail Value-at-Risk (TVaR)
5. Inflation adjustment applied at 2.46% per annum to claim severities

---

### Exploratory Data Analysis (EDA)

Before model fitting, comprehensive EDA was conducted to identify key risk drivers:

![Actuarial Control Cycle](ACC.png)

**EDA Key Findings**:

**Cargo Loss**: 
- Route risk shows near-monotonic increase in claim frequency (tiers 1→5)
- Debris density bands (0.0–1.0) demonstrate similar gradient, with highest-density band generating 2x baseline frequency
- Solar radiation shows secondary effect (~40% uplift for high bands)
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

## 📈 Aggregate Loss Results (Baseline)

Monte Carlo-derived summary statistics across all four hazard lines:

| Line of Business | Expected Annual Loss | Std Dev | 99% VaR | 99% TVaR |
|------------------|----------------------|---------|---------|----------|
| Business Interruption | Đ2.65 billion | Đ41.4M | Đ2.75B | Đ2.76B |
| Cargo Loss | Đ15.38 billion | Đ153.8M | Đ15.70B | Đ15.73B |
| Workers' Compensation | Đ14.9 million | Đ465K | Đ16.10M | Đ16.28M |
| Equipment Failure | Đ234.04 million | Đ5.13M | Đ246M | Đ248.13M |
| **Portfolio Total** | **Đ17.70 billion** | — | **Đ25.9B** (1-in-200) | **Đ27.8B** (1-in-200) |

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
| **Cargo Loss** | Poisson | 4.51 | ❌ Rejected | High overdispersion; counts clustered at extremes |
| **Cargo Loss** | Negative Binomial | 4.51 | ✅ Adopted | Accommodates overdispersion; best AIC/BIC |
| **Equipment Failure** | Poisson | 1.0956 | ✅ Adopted | Dispersion ≈ 1; simpler model preferred |
| **Equipment Failure** | Negative Binomial | 1.0956 | ❌ Rejected | Poisson sufficient; no efficiency gain |
| **Workers' Comp** | Poisson | 0.115 | ✅ Adopted | Minimal underdispersion; Poisson suitable |
| **Business Interruption** | Poisson | 1.73 | ❌ Rejected | Overdispersion present; Neg Binom better |
| **Business Interruption** | Negative Binomial | 1.73 | ✅ Adopted | Accommodates clustering; best AIC |

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

All analysis uses **five provided project datasets**:

1. **srcsc-2026-claims-cargo.xlsx** — Cargo Loss frequency & severity
   - 85,332 frequency records (post-exclusion of gold/platinum)
   - 18,673 severity records (4.6:1 ratio creates subsample bias; mitigated via GLM anchor calibration)
   - Variables: route_risk, debris_density, solar_radiation, cargo_type, cargo_value, weight, transit_duration, pilot_experience, container_type, vessel_age, distance

2. **srcsc-2026-claims-business-interruption.xlsx** — BI frequency & severity
   - Frequency data: solar_system, production_load, energy_backup_score, supply_chain_index, avg_crew_exp, maintenance_freq, safety_compliance
   - Severity data: claim amounts (in Đ millions)
   - Exposure metric for offset modeling

3. **srcsc-2026-claims-workers-comp.xlsx** — Workers' Compensation frequency & severity
   - Frequency: occupation, accident_history_flag, psych_stress_index, safety_training_index, solar_system
   - Severity: wage-replacement basis, claim_length (duration in days), employment_type, supervision_level, protective_gear_quality
   - Claims range: Đ120.76 (minimum) to Đ193,357.20 (maximum per occurrence)

4. **srcsc-2026-claims-equipment-failure.xlsx** — Equipment Failure frequency & severity
   - Frequency: equipment_type (Quantum Bore, Mag-Lift Aggregator, FluxStream Carrier, Fusion Transport, Ion Pulverizer, Graviton Extractor)
   - Severity: equipment_age (0–10 years), maintenance_int (100–5,000 hours), usage_int (0–24 hours/day)
   - Claim counts & amounts by system and equipment class

5. **Cosmic Quarry Inventory Data** — Exposure Bases
   - Equipment inventory: ~505 units across three systems (exposure base for EF frequency model)
   - Personnel headcount: ~86,765 workers across three systems (exposure base for WC frequency model)
   - System-level operational metrics (production_load, supply_chain_index, etc.)

### Economic & Environmental Data

6. **srcsc-2026-interest-and-inflation.xlsx** — Macroeconomic Assumptions
   - Historical inflation rates, overnight bank lending rates, 1-year and 10-year risk-free spot rates
   - ARIMA forecasts for 10-year projection horizon
   - Inflation rate (baseline): 4.23% per annum
   - Short-term investment rate: 3.71%
   - Long-term discount rate: 3.76%

---

## Key Modeling Decisions & Rationale

### GLM Specification

**Offset Usage**: Exposure period included as offset in frequency models to allow claim counts to scale proportionally with operational exposure (time or unit volume)

**Frequency Distribution Choices**:
- **Negative Binomial for BI, WC, CL**: Accommodates overdispersion (variance > mean) observed in claim counts, particularly for systems with sporadic high-frequency events
- **Poisson for EF**: Lower overdispersion in equipment failure counts justifies simpler Poisson structure

**Severity Distribution Choices**:
- **Gamma for CL & EF**: Accommodates right-skewed, heavy-tailed claim amounts with operational rating factors (cargo_value, weight, usage_intensity)
- **Lognormal for BI & WC**: Reflects multiplicative nature of operational disruption costs and wage-replacement expenses

### Data Limitations & Mitigations

| Limitation | Impact | Mitigation |
|-----------|--------|-----------|
| Gold/platinum exclusion reduces CL severity data to 18,673 of 85,332 frequency observations | Severity model trained on non-random subsample; potential underweighting of low-value claims | GLM anchor calibration ties severity scale to mean(claim_amount)/sev_shape; sensitivity tested in Appendix D.2 |
| No explicit multi-system claim tagging in cargo data | System-specific pricing relies on route_risk, debris_density, solar_radiation as proxies rather than direct system identifiers | Route_risk tiers (1–5) calibrated to three systems via encyclopedia; Oryn Delta's tail risk stress-tested separately |
| Historical data spans Epsilon & Zeta systems (not in RFP scope) | Frequency intercepts potentially diluted by external systems' operating conditions | Conservatively accepted; if Epsilon/Zeta have lower risk, estimates are conservative bias |
| Cross-sectional data lacks time dimension | Cannot model claims trend, development, or latent demand surge | Inflation loading stress-tested up to 40% separately (Appendix D); claims inflation driver included in stress scenarios |
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
    'Baseline': {'mean': 1_978_730, 'var99': 13_684_390, 'tvar99': 21_261_136},
    'Frequency +50%': {'mean': 2_968_095, 'var99': 20_526_585, 'tvar99': 31_891_704},
    'Severity +50%': {'mean': 2_968_095, 'var99': 20_526_585, 'tvar99': 31_891_704},
    'Combined +50%': {'mean': 4_457_142, 'var99': 30_789_878, 'tvar99': 47_837_551}
}

# Great Flare produces 124% increase in tail risk
flare_vir99_increase = (47_837_551 - 21_261_136) / 21_261_136  # +124%
```

---

## Correlated Risk Scenarios & Copula Analysis

### Gaussian Copula Modeling

To capture cross-hazard dependency from systemic shocks (e.g., solar storms affecting all four hazard lines simultaneously):

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

✅ **Four Pricing Models**: Frequency & severity GLMs fully specified with rating factors  
✅ **Monte Carlo Framework**: 10,000 iterations per line generating stable tail estimates  
✅ **Stress Testing Suite**: 30+ scenarios covering frequency, severity, operational, workforce, and systemic risks  
✅ **Copula Modeling**: Dependency structure & correlated catastrophe assessment  
✅ **10-Year Financial Projections**: Present value of premiums, losses, expenses, reserves, and profit  
✅ **Capital Adequacy Assessment**: 99% TVaR-based reserves ensuring 1-in-200-year solvency  
✅ **System-Specific Guidance**: Tailored deductibles, endorsements, and risk loadings by solar system  

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

---

## Visualizations & Graphics

### Recommended Visualizations from EDA, Pricing, & Stress Testing

The following visualizations provide critical insights into model behavior and risk dynamics. If available in your project folders, include them in this README:

**Exploratory Data Analysis Visualizations**:
- **Claim Frequency by Route Risk Tier** (Cargo Loss) — Near-monotonic increase in frequency tiers 1→5
- **Debris Density vs Claim Rate** (Cargo Loss) — Highest-density band generates 2x baseline frequency
- **Exposure-Adjusted Claim Rates by System** (Business Interruption) — Inverse relationship: low exposure → high frequency
- **Claim Severity Distribution** (Workers' Compensation) — Heavy right tail; maximum Đ193,357
- **Equipment Age vs Claim Frequency & Severity** (Equipment Failure) — Frequency rises with age; severity stable

**Pricing & Capital Modeling**:
- **Aggregate Loss Distributions** (All Four Hazards) — Monte Carlo simulation results (100,000 iterations)
- **Tail Risk Comparison** — 99% VaR and 99.5% TVaR across all hazard lines
- **10-Year Cash Flow Projections** — Premium, loss, expense, and profit trajectories

**Stress Testing Visualizations**:
- **VaR Amplification Under Stress** — Baseline vs Moderate (+25% freq, +10% sev) vs Extreme (+50% freq, +30% sev)
- **Cargo Loss Great Flare Scenario** — 124% amplification in 99% VaR
- **Sensitivity Lines** (All Hazards) — Risk driver sensitivities showing steepest gradients
- **Correlated vs Independent Scenarios** — Gaussian copula at various correlation levels (ρ = -0.5 to +0.5)

**Risk Assessment**:
- **System-Specific Risk Profiles** — Heat maps or radar charts comparing Helionis, Bayesia, Oryn Delta
- **Threat Matrix** — Likelihood vs Impact for top 8 threats across all hazards
- **Correlated Risk Heatmap** — Cross-hazard dependency structure under different correlation assumptions

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

## About This Report

**Project**: 2026 SOA Student Research Case Study Challenge  
**Organization**: Galaxy General Insurance Company & Cosmic Quarry Mining Corporation  
**Scope**: Insurance pricing for interstellar mining operations across three solar systems  
**Analysis Horizon**: 10-year financial projections with stress testing to 1-in-200-year events  

**Key Insights**:
- Cargo Loss dominates portfolio tail risk (84% of capital reserves)
- Great Flare correlated catastrophe requires Đ530M additional capital buffer
- System-specific risk profiles justify differentiated pricing and coverage terms
- ESG integration (safety training, debris mitigation) provides premium relief opportunity

---

## Technical Highlights

- **Advanced Actuarial Methods**: Collective risk models, GLM-based pricing, Monte Carlo simulation
- **Statistical Rigor**: Copula dependency testing, overdispersion accommodation, tail risk quantification
- **Risk Management Framework**: 99% TVaR capital adequacy, stress testing across 30+ scenarios, correlated catastrophe modeling
- **Professional Documentation**: Transparent assumptions, comprehensive appendices, reproducible methodology

---

<div align="center">

### Built with Actuarial Excellence

_Comprehensive pricing strategy for the final frontier_

**Prepared for**: Galaxy General Insurance Company  
**Data Sources**: Cosmic Quarry operational & claims data (2026)  
**Analysis Date**: April 2026

</div>

---

## How to Use This Documentation

1. **Executive Overview** → Start here for 5-minute summary
2. **Methodology Section** → Understand data sources & modeling approach
3. **Aggregate Loss Results** → Review baseline statistics & capital needs
4. **Stress Testing** → Examine sensitivities to key risk drivers
5. **Full Appendices** → Detailed technical specifications in KADAK_Report.docx

---

---

## Full Report

**[ Download the Complete KADAK Report (PDF)](KADAK_Report.pdf)**

The full technical report includes all appendices, detailed analyses, visualizations, and supplementary calculations referenced throughout this documentation.

---

**For questions or additional information, refer to the complete KADAK Report document.**
