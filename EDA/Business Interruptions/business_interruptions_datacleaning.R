# =============================================================
# 1. Setup
# =============================================================

# -----------------------------
# 1.1 Libraries
# -----------------------------
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

# -----------------------------
# 1.2 Working Directory & Data Import
# -----------------------------
setwd("C:/Users/khush/OneDrive - UNSW/Desktop/ACTL4001")

bi_freq_raw <- read_excel("KADAK/srcsc-2026-claims-business-interruption.xlsx", sheet = 1)
bi_sev_raw  <- read_excel("KADAK/srcsc-2026-claims-business-interruption.xlsx", sheet = 2)

# =============================================================
# 2. Data Cleaning
# =============================================================
# -----------------------------
# 2.1 Standardise Solar System Names
# -----------------------------
bi_freq_raw <- bi_freq_raw %>%
  mutate(solar_system = case_when(
    str_detect(solar_system, "^Zeta") ~ "Zeta",
    str_detect(solar_system, "^Epsilon") ~ "Epsilon",
    str_detect(solar_system, "^Helionis") ~ "Helionis Cluster",
    TRUE ~ solar_system
  ))

bi_sev_raw <- bi_sev_raw %>%
  mutate(solar_system = case_when(
    str_detect(solar_system, "^Zeta") ~ "Zeta",
    str_detect(solar_system, "^Epsilon") ~ "Epsilon",
    str_detect(solar_system, "^Helionis") ~ "Helionis Cluster",
    TRUE ~ solar_system
  ))

# -----------------------------
# 2.2 Clean Frequency & Severity Data
# -----------------------------
bi_freq_clean <- bi_freq_raw %>%
  filter(!is.na(policy_id) & policy_id != "",
         !is.na(station_id) & station_id != "",
         solar_system %in% c("Helionis Cluster", "Epsilon", "Zeta"),
         production_load >= 0 & production_load <= 1,
         energy_backup_score %in% 1:5,
         supply_chain_index >= 0 & supply_chain_index <= 1,
         avg_crew_exp >= 1 & avg_crew_exp <= 30,
         maintenance_freq >= 0 & maintenance_freq <= 6,
         safety_compliance %in% 1:5,
         exposure > 0 & exposure <= 1,
         claim_count >= 0 & claim_count <= 4)

bi_sev_clean <- bi_sev_raw %>%
  filter(!is.na(policy_id) & policy_id != "",
         !is.na(station_id) & station_id != "",
         solar_system %in% c("Helionis Cluster", "Epsilon", "Zeta"),
         production_load >= 0 & production_load <= 1,
         energy_backup_score %in% 1:5,
         safety_compliance %in% 1:5,
         exposure > 0 & exposure <= 1,
         claim_amount >= 28000 & claim_amount <= 1426000)

# =============================================================
# 3. Dataset Construction
# =============================================================
# -----------------------------
# 3.1 Aggregate Severity Data
# -----------------------------
bi_sev_agg <- bi_sev_clean %>%
  group_by(policy_id) %>%
  summarise(total_claim_amount = sum(claim_amount, na.rm = TRUE),
            avg_claim_amount   = mean(claim_amount, na.rm = TRUE))

# -----------------------------
# 3.2 Combine Frequency and Severity
# -----------------------------
bi_full <- bi_freq_clean %>%
  left_join(bi_sev_agg, by = "policy_id") %>%
  mutate(total_claim_amount = coalesce(total_claim_amount, 0),
         avg_claim_amount   = coalesce(avg_claim_amount, 0))
