# KADAK Report: Insurance Pricing and Risk Modelling for Interstellar Mining
## Methods & Data Documentation

> _"Tell me and I forget. Teach me and I remember. Involve me and I learn." – Benjamin Franklin_

---

## 📊 Executive Overview

This project develops a comprehensive insurance pricing strategy for **Galaxy General Insurance Company** to cover the operational risks of **Cosmic Quarry Mining Corporation** across three solar systems: **Helionis Cluster**, **Bayesia System**, and **Oryn Delta**. Four primary insurance products address material exposures: Business Interruption (BI), Cargo Loss (CL), Workers' Compensation (WC), and Equipment Failure (EF).

The analysis synthesizes advanced actuarial modeling, statistical estimation, and enterprise risk management frameworks to deliver robust pricing, capital adequacy assessment, and tail risk quantification for extreme but rare events.

---

## 🎯 Products Developed

### Coverage Areas & Key Parameters

| Product | Primary Exposure | Key Risk Drivers | System-Specific Focus |
|---------|------------------|------------------|----------------------|
| **Business Interruption** | Production disruptions from environmental hazards | energy_backup_score, production_load, safety_compliance | Helionis: supply chain; Bayesia: radiation; Oryn: orbital shear |
| **Cargo Loss** (excl. gold/platinum) | Mineral transport losses | route_risk, debris_density, solar_radiation | All systems; highest concentration in Oryn Delta |
| **Workers' Compensation** | Workforce injuries under hazardous conditions | occupation, psych_stress_index, safety_training_index | All systems; Helionis exhibits highest claim frequency |
| **Equipment Failure** | Complex mining machinery failures | equipment_age, usage_intensity, maintenance_int | Helionis: highest claim rates; Bayesia: radiation-induced failures |

---

## 🔬 Methodology Overview

### Frequency-Severity Framework

The analysis employs a **collective risk model** combining:

**Frequency Models** (Claim Counts)
- **Business Interruption**: Negative Binomial GLM (log-link)
- **Cargo Loss**: Negative Binomial GLM (overdispersion accommodation)
- **Workers' Compensation**: Negative Binomial GLM
- **Equipment Failure**: Poisson GLM

**Severity Models** (Claim Amounts)
- **Business Interruption**: Gamma GLM (log-link), with Đ2.65 billion expected annual loss
- **Cargo Loss**: Gamma GLM, with Đ15.38 billion expected annual loss (largest exposure)
- **Workers' Compensation**: Lognormal, with Đ14.9 million expected annual loss
- **Equipment Failure**: Lognormal, with Đ234.04 million expected annual loss

### Monte Carlo Simulation

**10,000 Monte Carlo iterations** per line of business generate stable tail risk estimates:
- Claim counts drawn from fitted frequency distribution
- Claim severities sampled from fitted severity distribution
- Aggregate annual loss computed via collective risk formula: **S = Σ Xi** (i = 1 to N)
- 99% and 99.5% percentiles (Value-at-Risk & Tail Value-at-Risk) extracted for capital adequacy

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

## 📊 Data Sources & Specifications

### Primary Datasets

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

## 🔍 Key Modeling Decisions & Rationale

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

## 💰 Pricing & Financial Framework

### Premium Calculation

**Technical Premium Formula**:
```
Technical Premium = (Pure Premium + Risk Margin) / (1 – Expense Ratio – Profit Margin)
```

**Standard Assumptions**:
- **Expense Ratio**: 30% (administration, reinsurance, commissions)
- **Profit Margin**: 7% (based on industry benchmarks)
- **Risk Margin**: 5% of pure premium (uncertainty loading)
- **Implied Load Factor**: 1.538 (yielding 65% permissible loss ratio)

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

## ⚡ Stress Testing Results

### Cargo Loss (Primary Tail Driver)

Under extreme scenarios, CL exhibits the largest amplification:

| Scenario | Mean Loss | 99% VaR | % Change vs Baseline |
|----------|-----------|---------|----------------------|
| Baseline | Đ15.38B | Đ15.70B | — |
| **Frequency +50% (1-in-100)** | Đ23.07B | Đ23.45B | +49.6% |
| **Severity +50% (1-in-100)** | Đ23.07B | Đ23.54B | +50.0% |
| **Great Flare (Freq+Sev +50%)** | Đ34.61B | Đ35.19B | **+124.0%** |

**Route Risk Surge (+35% debris deterioration)**: Mean Đ22.58B, VaR Đ23.07B

### Business Interruption

BI highly sensitive to frequency and severity shocks:
- **Baseline**: Đ2.6–Đ2.7B losses
- **Severe stress (both freq & sev shocked)**: Đ5.4B+ losses (~doubling)
- **Key drivers**: claim frequency, production_load, safety_compliance

### Workers' Compensation

Moderate baseline (Đ15.044M) but extreme sensitivity to environmental hazards:
- **Baseline 99% VaR**: Đ16.1M
- **Gravity hazard (2x severity)**: Đ64.0M (4x increase)
- **Psychological stress (2x severity)**: Đ47.9M (3x increase)

### Equipment Failure

Severity dominates over frequency:
- **Baseline 99% VaR**: Đ131.4M
- **Combined +10% shock**: VaR Đ151.0M (+15%)
- **Combined -10% shock**: VaR Đ110.3M (-16%)

---

## 🌍 Correlated Risk Scenarios

### Gaussian Copula Framework

**Dependency Testing**: Gaussian, Clayton, Gumbel, and t-copulas evaluated; limited evidence of strong frequency-severity dependence observed.

**Cross-Hazard Correlation** (ρ = -0.5 to +0.5):

| Correlation (ρ) | Scenario | VaR 1-in-40 | Δ vs Independence | VaR 1-in-200 |
|------------------|----------|------------|------------------|------------|
| -0.5 | Diversifying | Đ23.28B | +3.5% | Đ26.73B |
| 0.0 | **Independence (baseline)** | **Đ22.50B** | — | **Đ25.90B** |
| +0.5 | **Great Flare (high correlation)** | **Đ22.86B** | +1.6% | **Đ26.43B** |

**Interpretation**: Great Flare scenario (ρ = +0.5) adds ~Đ530M in capital requirement at 1-in-200 level, driven by simultaneous disruption across all three systems and all four hazard lines.

---

## 🎯 Risk Assessment & System-Specific Profiles

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

## 📋 Scenario Testing (3-Year & 10-Year Horizons)

| Scenario | Annual Technical Premium | 3-Year Expected Cost | 3-Year Net Revenue | 10-Year 99% VaR (Costs) |
|----------|---------------------------|----------------------|-------------------|-------------------------|
| **Best Case** (Attritional claims, stable routes) | Đ20.3B | Đ37.4B | Đ50.9B | — |
| **Moderate Case** (Baseline GLM fit) | Đ26.5B | Đ49.2B | Đ30.3B | — |
| **Worst Case** (Great Flare + debris cascade) | Đ55.6B | Đ115.9B | Đ23.4B | **Đ2.56T** |

---

## 🧠 Methodological Innovations

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

## 🚀 Key Deliverables

✅ **Four Pricing Models**: Frequency & severity GLMs fully specified with rating factors
✅ **Monte Carlo Framework**: 10,000 iterations per line generating stable tail estimates
✅ **Stress Testing Suite**: 30+ scenarios covering frequency, severity, operational, workforce, and systemic risks
✅ **Copula Modeling**: Dependency structure & correlated catastrophe assessment
✅ **10-Year Financial Projections**: Present value of premiums, losses, expenses, reserves, and profit
✅ **Capital Adequacy Assessment**: 99% TVaR-based reserves ensuring 1-in-200-year solvency
✅ **System-Specific Guidance**: Tailored deductibles, endorsements, and risk loadings by solar system

---

## 📚 Assumptions & Documentation

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

## 🔗 Appendices & References

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

---

## 📞 About This Report

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

## 🎓 Technical Highlights

- **Advanced Actuarial Methods**: Collective risk models, GLM-based pricing, Monte Carlo simulation
- **Statistical Rigor**: Copula dependency testing, overdispersion accommodation, tail risk quantification
- **Risk Management Framework**: 99% TVaR capital adequacy, stress testing across 30+ scenarios, correlated catastrophe modeling
- **Professional Documentation**: Transparent assumptions, comprehensive appendices, reproducible methodology

---

<div align="center">

### 🌟 Built with Actuarial Excellence

_Comprehensive pricing strategy for the final frontier_

**Prepared for**: Galaxy General Insurance Company
**Data Sources**: Cosmic Quarry operational & claims data (2026)
**Analysis Date**: April 2026

</div>

---


**For questions or additional information, refer to the complete KADAK Report document.**
**[📥 Download the Complete KADAK Report (PDF)](KADAK_Report.pdf)**