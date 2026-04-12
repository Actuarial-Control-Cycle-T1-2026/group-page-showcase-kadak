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


##### PART ONE: EDA ############################################################
##### Data Setup ###############################################################
# Load ──────────────────────────────────────────────────────────────────────
setwd("~/OneDrive - UNSW/ACTL4001/Assignment/KADAK/KADAK")

equip_freq <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = 1)
equip_sev  <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = 2)

# Clean ─────────────────────────────────────────────────────────────────────

# Strip corrupted suffixes from categorical fields ──
clean_category <- function(x) {
  trimws(sub("_\\?\\?\\?[0-9]+.*$", "", x))
}

equip_freq$equipment_type <- clean_category(equip_freq$equipment_type)
equip_freq$solar_system   <- clean_category(equip_freq$solar_system)
equip_sev$equipment_type  <- clean_category(equip_sev$equipment_type)
equip_sev$solar_system    <- clean_category(equip_sev$solar_system)

# Fix mis-spellings
equip_freq$equipment_type <- dplyr::recode(equip_freq$equipment_type,
                                           "FexStram Carrier" = "FluxStream Carrier",
                                           "Flux Rider"       = "Fusion Transport",
                                           "ReglAggregators"  = "Mag-Lift Aggregator"
)
equip_sev$equipment_type <- dplyr::recode(equip_sev$equipment_type,
                                          "FexStram Carrier" = "FluxStream Carrier",
                                          "Flux Rider"       = "Fusion Transport",
                                          "ReglAggregators"  = "Mag-Lift Aggregator"
)

# Keep only valid solar systems
valid_systems <- c("Helionis Cluster", "Epsilon", "Zeta")
equip_freq <- equip_freq %>% filter(solar_system %in% valid_systems)
equip_sev  <- equip_sev  %>% filter(solar_system %in% valid_systems)

# Fix numeric values to provided data dictionary range  ──────────────────────
equip_freq <- equip_freq %>%
  filter(
    equipment_age   >= 0   & equipment_age   <= 10,
    maintenance_int >= 100 & maintenance_int <= 5000,
    usage_int       >= 0   & usage_int       <= 24,
    exposure        >  0   & exposure        <= 1,
    claim_count     >= 0   & claim_count     <= 3
  ) %>%
  mutate(
    claim_count    = as.integer(round(claim_count)),
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

equip_sev <- equip_sev %>%
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

cat("Frequency dataset rows after cleaning:", nrow(equip_freq), "\n")
cat("Severity dataset rows after cleaning:",  nrow(equip_sev),  "\n")


# Shared pastel palettes ────────────────────────────────────────────────────
pal_system <- c(
  "Epsilon"          = "#FFCBA4",   # warm peach
  "Helionis Cluster" = "#A8D8C8",   # soft mint
  "Zeta"             = "#F9C6CF"    # baby pink
)

# Scatter/line colours
pal_system_dark <- c(
  "Epsilon"          = "#E8885A",
  "Helionis Cluster" = "#5FB89A",
  "Zeta"             = "#D97AA6"
)

# Equipment type: 6 muted, distinct pastels
equip_types_sorted <- sort(unique(equip_sev$equipment_type))
pal_equip <- setNames(
  c("#FFCBA4", "#A8D8C8", "#F9C6CF", "#BFD7ED", "#D4C5E2", "#C8E6B0"),
  equip_types_sorted
)


##### EDA ######################################################################
# Distribution of Claim Amount ──────────────────────────────────────────
p95 <- quantile(equip_sev$claim_amount, 0.95)

p1 <- ggplot(equip_sev, aes(x = claim_amount)) +
  geom_histogram(binwidth = 5000, fill = "#FFCBA4", color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count) * 5000), color = "#c95b1a", linewidth = 1) +
  coord_cartesian(xlim = c(0, p95)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = "Distribution of Claim Amount (95th percentile view)",
    subtitle = paste0("X-axis clipped at 95th pct (", scales::comma(round(p95)), ")"),
    x = "Claim Amount", y = "Count"
  ) +
  theme_minimal()

p1_log <- ggplot(equip_sev, aes(x = log(claim_amount))) +
  geom_histogram(binwidth = 0.2, fill = "#FFCBA4", color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count) * 0.2), color = "#c95b1a", linewidth = 1) +
  labs(title = "Distribution of Log Claim Amount", x = "Log(Claim Amount)", y = "Count") +
  theme_minimal()

grid.arrange(p1, p1_log, ncol = 2)


# Equipment Type vs Solar System ────────────────────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type), !is.na(solar_system)),
       aes(x = equipment_type, fill = solar_system)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Number of Equipment Claims by Type and Solar System",
       x = "Equipment Type", y = "Count", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# Frequency of Claims by Equipment Type and Solar System ─────────────────
claims_summary <- equip_freq |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  group_by(equipment_type, solar_system) |>
  summarise(total_claims = sum(claim_count, na.rm = TRUE), .groups = "drop")

ggplot(claims_summary, aes(x = equipment_type, y = total_claims, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Frequency of Claims by Equipment Type and Solar System",
       x = "Equipment Type", y = "Total Claim Frequency", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())


# Exposure-Adjusted Claim Rate ──────────────────────────────────────────
claim_rate_summary <- equip_freq |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  group_by(equipment_type, solar_system) |>
  summarise(
    total_claims   = sum(claim_count, na.rm = TRUE),
    total_exposure = sum(exposure,    na.rm = TRUE),
    claim_rate     = total_claims / total_exposure,
    .groups = "drop"
  )

ggplot(claim_rate_summary, aes(x = equipment_type, y = claim_rate, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Claim Frequency Rate (Exposure Adjusted)",
       x = "Equipment Type", y = "Claims per Exposure Unit", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Average Claim Amount by Equipment Type ─────────────────────────────────
equip_summary <- equip_sev |>
  filter(!is.na(equipment_type)) |>
  group_by(equipment_type) |>
  summarise(mean_claim = mean(claim_amount, na.rm = TRUE))

ggplot(equip_summary,
       aes(x = fct_reorder(equipment_type, mean_claim), y = mean_claim,
           fill = equipment_type)) +
  geom_col() +
  scale_fill_manual(values = pal_equip) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Amount by Equipment Type",
       x = "Equipment Type", y = "Mean Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Claim Frequency Rate by Equipment Type ─────────────────────────────────
equip_freq_rate <- equip_freq |>
  filter(!is.na(equipment_type)) |>
  group_by(equipment_type) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(equip_freq_rate |> filter(!is.na(equipment_type)),
       aes(x = fct_reorder(equipment_type, claim_rate), y = claim_rate,
           fill = equipment_type)) +
  geom_col() +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Claim Frequency Rate by Equipment Type",
       x = "Equipment Type", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Equipment Age vs Claim Frequency (banded) ─────────────────────────────
equip_freq <- equip_freq |>
  mutate(age_band = cut(
    equipment_age,
    breaks = seq(0, 10, by = 2),
    labels = c("0-2 yrs", "2-4 yrs", "4-6 yrs", "6-8 yrs", "8-10 yrs"),
    include.lowest = TRUE
  ))

age_freq <- equip_freq |>
  filter(!is.na(age_band)) |>
  group_by(age_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(age_freq, aes(x = age_band, y = claim_rate, group = 1)) +
  geom_line(colour = "#E8885A", linewidth = 1.2) +
  geom_point(colour = "#E8885A", size = 3.5, fill = "#FFCBA4", shape = 21, stroke = 1.5) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Claim Frequency Rate by Equipment Age",
       x = "Equipment Age", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# Equipment Age vs Claim Severity (banded) ───────────────────────────────
equip_sev <- equip_sev |>
  mutate(age_band = cut(
    equipment_age,
    breaks = seq(0, 10, by = 2),
    labels = c("0-2 yrs", "2-4 yrs", "4-6 yrs", "6-8 yrs", "8-10 yrs"),
    include.lowest = TRUE
  ))

age_sev <- equip_sev |>
  filter(!is.na(age_band)) |>
  group_by(age_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(age_sev, aes(x = age_band, y = mean_severity, group = 1)) +
  geom_line(colour = "#D97AA6", linewidth = 1.2) +
  geom_point(colour = "#D97AA6", size = 3.5, fill = "#F9C6CF", shape = 21, stroke = 1.5) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Average Claim Severity by Equipment Age",
       x = "Equipment Age", y = "Average Claim Amount") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


# Maintenance Interval vs Claim Frequency ────────────────────────────────
equip_freq <- equip_freq |>
  mutate(maint_band = cut(
    maintenance_int,
    breaks = c(100, 500, 1000, 1500, 2000),
    labels = c("100-500", "500-1000", "1000-1500", "1500-2000"),
    include.lowest = TRUE, right = TRUE
  ))

maint_freq <- equip_freq |>
  filter(!is.na(maint_band)) |>
  group_by(maint_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(maint_freq, aes(x = maint_band, y = claim_rate)) +
  geom_col(fill = "#FFCBA4", width = 0.65) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Claim Frequency Rate by Maintenance Interval Band",
       x = "Maintenance Interval (Earth Hours)", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Maintenance Interval vs Claim Severity ────────────────────────────────
equip_sev <- equip_sev |>
  mutate(maint_band = cut(
    maintenance_int,
    breaks = c(100, 500, 1000, 1500, 2000, 5000),
    labels = c("100-500", "500-1000", "1000-1500", "1500-2000", "2000-5000"),
    include.lowest = TRUE, right = TRUE
  ))

maint_sev <- equip_sev |>
  filter(!is.na(maint_band)) |>
  group_by(maint_band) |>
  summarise(
    mean_severity = mean(claim_amount, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(maint_band))

ggplot(maint_sev, aes(x = maint_band, y = mean_severity)) +
  geom_col(fill = "#F9C6CF", width = 0.65) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Average Claim Severity by Maintenance Interval Band",
       x = "Maintenance Interval (Earth Hours)", y = "Average Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Usage Intensity vs Claim Frequency ──────────────────────────
equip_freq <- equip_freq |>
  mutate(usage_band = cut(
    usage_int,
    breaks = c(0, 8, 16, 24),
    labels = c("Low (0-8 hrs)", "Mid (8-16 hrs)", "High (16-24 hrs)"),
    include.lowest = TRUE
  ))

usage_freq <- equip_freq |>
  filter(!is.na(usage_band)) |>
  group_by(usage_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(usage_freq, aes(x = usage_band, y = claim_rate)) +
  geom_col(fill = "#A8D8C8", width = 0.65) +
  labs(title = "Claim Frequency Rate by Daily Usage Intensity Band",
       x = "Usage Intensity", y = "Claims per Exposure Year") +
  theme_minimal()


# Usage Intensity vs Claim Severity  ───────────────────────────
equip_sev <- equip_sev |>
  mutate(usage_band = cut(
    usage_int,
    breaks = c(0, 8, 16, 24),
    labels = c("Low (0-8 hrs)", "Mid (8-16 hrs)", "High (16-24 hrs)"),
    include.lowest = TRUE
  ))

usage_sev <- equip_sev |>
  filter(!is.na(usage_band)) |>
  group_by(usage_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(usage_sev, aes(x = usage_band, y = mean_severity)) +
  geom_col(fill = "#D4C5E2", width = 0.65) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Daily Usage Intensity Band",
       x = "Usage Intensity", y = "Average Claim Amount") +
  theme_minimal()


# Heatmap of Equipment Type vs Solar System ───────────────────────────────
heatmap_counts <- equip_sev |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  count(equipment_type, solar_system)

ggplot(heatmap_counts,
       aes(x = fct_reorder(equipment_type, n, .fun = sum),
           y = fct_reorder(solar_system,   n, .fun = sum),
           fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#E8F5F0", high = "#c95b1a") +
  labs(title = "Heatmap: Equipment Type vs Solar System",
       x = "Equipment Type", y = "Solar System", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Equipment Type Distribution Across Solar Systems ──────────────────────
system_hues <- list(
  "Epsilon"          = c("#FFE8D6","#FFD4B5","#FFBE90","#FFAA6E","#F08E52","#D4703A"),
  "Helionis Cluster" = c("#D6EFE6","#B8E2D4","#96D4C0","#72C5AB","#4EB596","#2EA480"),
  "Zeta"             = c("#FCDDE8","#FAC4D8","#F8ABC6","#F490B3","#E8759E","#D45A88")
)

solar_systems_14 <- sort(unique(equip_sev$solar_system))
equip_types_14   <- sort(unique(equip_sev$equipment_type))
n_types_14       <- length(equip_types_14)

colour_vec <- unlist(lapply(solar_systems_14, function(ss) {
  pal    <- system_hues[[ss]]
  shades <- pal[seq_len(min(n_types_14, length(pal)))]
  setNames(shades, paste0(ss, "__", equip_types_14[seq_len(length(shades))]))
}))

equip_sev_plot <- equip_sev |>
  filter(!is.na(equipment_type), !is.na(solar_system)) |>
  mutate(fill_key = paste0(solar_system, "__", equipment_type))

ggplot(equip_sev_plot,
       aes(x = fct_infreq(equipment_type), fill = fill_key)) +
  geom_bar(color = "white", linewidth = 0.2) +
  facet_wrap(~ solar_system) +
  scale_fill_manual(values = colour_vec) +
  labs(title = "Equipment Type Distribution Across Solar Systems",
       x = "Equipment Type", y = "Number of Claims") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold"),
    legend.position    = "none"
  )

# Correlation heatmap ───────────────────────────────────────────────────
num_vars <- equip_sev |>
  dplyr::select(claim_amount, equipment_age, maintenance_int, usage_int, exposure) |>
  cor(use = "complete.obs")

cor_long <- as.data.frame(as.table(num_vars)) |>
  rename(Correlation = Freq)

ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 4) +
  scale_fill_gradient2(low = "#A8D8C8", mid = "white", high = "#F9C6CF",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap of Continuous Variables", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Boxplot of Log Claim Amount by Equipment Type ───────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type)),
       aes(x    = fct_reorder(equipment_type, claim_amount, median),
           y    = log(claim_amount),
           fill = equipment_type)) +
  geom_boxplot(alpha = 0.85, outlier.size = 1.2, outlier.alpha = 0.4) +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Log Claim Amount Distribution by Equipment Type",
       x = "Equipment Type", y = "Log(Claim Amount)") +
  theme_minimal() +
  theme(axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Log Claim Amount by Equipment Type ────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type)),
       aes(x    = log(claim_amount),
           y    = fct_reorder(equipment_type, claim_amount, median),
           fill = equipment_type)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Claim Amount Distribution by Equipment Type (Ridge Plot)",
       x = "Log(Claim Amount)", y = "Equipment Type") +
  theme_minimal() +
  theme(legend.position = "none")

# Claim Amount by Equipment Type ────────────────────────────
ggplot(equip_sev |> filter(!is.na(equipment_type)),
       aes(x    = claim_amount,
           y    = fct_reorder(equipment_type, claim_amount, median),
           fill = equipment_type)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_equip) +
  labs(title = "Claim Amount Distribution by Equipment Type (Ridge Plot)",
       x = "Claim Amount", y = "Equipment Type") +
  theme_minimal() +
  theme(legend.position = "none")


# Claim Amount by Solar System ──────────────────────────────
ggplot(equip_sev |> filter(!is.na(solar_system)),
       aes(x    = log(claim_amount),
           y    = fct_reorder(solar_system, claim_amount, median),
           fill = solar_system)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Claim Amount Distribution by Solar System (Ridge Plot)",
       x = "Log(Claim Amount)", y = "Solar System") +
  theme_minimal() +
  theme(legend.position = "none")


# Claim Amount by Solar System ──────────────────────────────
ggplot(equip_sev |> filter(!is.na(solar_system)),
       aes(x    = claim_amount,
           y    = fct_reorder(solar_system, claim_amount, median),
           fill = solar_system)) +
  geom_density_ridges(alpha = 0.78, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_manual(values = pal_system) +
  labs(title = "Claim Amount Distribution by Solar System (Ridge Plot)",
       x = "Claim Amount", y = "Solar System") +
  theme_minimal() +
  theme(legend.position = "none")


