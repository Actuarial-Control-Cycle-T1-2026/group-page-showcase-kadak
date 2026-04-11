# =============================================================
# Exploratory Data Analysis (EDA)
# =============================================================
# -----------------------------
# 1 Setup Colors
# -----------------------------
blue_pal <- c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")

# -----------------------------
# 2 Frequency Analysis
# -----------------------------
# Number of Policies by Solar System
ggplot(bi_freq_clean, aes(x=solar_system, fill=solar_system)) +
  geom_bar() +
  scale_fill_manual(values=blue_pal) +
  labs(title="Number of Policies by Solar System", x="Solar System", y="Count") +
  theme_minimal()

# Total Claim Frequency by Solar System
freq_summary <- bi_freq_clean %>%
  group_by(solar_system) %>%
  summarise(total_claims = sum(claim_count, na.rm=TRUE), .groups="drop")

ggplot(freq_summary, aes(x=solar_system, y=total_claims, fill=solar_system)) +
  geom_col() +
  labs(title="Total Claim Frequency by Solar System", x="Solar System", y="Total Claims") +
  theme_minimal()

# Heatmap: Total Claims by Production Load & Solar System
heatmap_summary <- bi_full %>%
  group_by(solar_system, production_load_bin = cut(production_load, breaks=seq(0,1,0.1))) %>%
  summarise(total_claims = sum(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(heatmap_summary, aes(x=production_load_bin, y=solar_system, fill=total_claims)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  labs(title="Heatmap: Total Claims by Production Load and Solar System", x="Production Load Bin", y="Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# -----------------------------
# 3 Severity Analysis
# -----------------------------
p1 <- ggplot(bi_sev_clean, aes(x=claim_amount)) +
  geom_histogram(binwidth=50000, fill="white", color=blue_pal[2]) +
  geom_density(aes(y=..count..), color=blue_pal[5], linewidth=1) +
  scale_x_continuous(labels=scales::comma) +
  labs(title="Distribution of BI Claim Amounts", x="Claim Amount", y="Count") +
  theme_minimal()

p1_log <- ggplot(bi_sev_clean, aes(x=log(claim_amount+1))) +
  geom_histogram(binwidth=0.3, fill=blue_pal[2], color="white") +
  geom_density(aes(y=..count..), color=blue_pal[5], linewidth=1) +
  labs(title="Log-Transformed Claim Amounts", x="Log(Claim Amount)", y="Count") +
  theme_minimal()

grid.arrange(p1, p1_log, ncol=2)

# -----------------------------
# 4 Portfolio Analysis
# -----------------------------
claim_summary <- bi_full %>%
  group_by(solar_system) %>%
  summarise(avg_total_claim = mean(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(claim_summary, aes(x=reorder(solar_system, avg_total_claim), y=avg_total_claim, fill=solar_system)) +
  geom_col() + coord_flip() +
  labs(title="Average Total Claim Amount per Policy", x="Solar System", y="Avg Total Claim Amount") +
  theme_minimal()

bi_full <- bi_full %>%
  mutate(exposure_bin = cut(exposure, breaks=seq(0,1,0.1), include.lowest=TRUE))

freq_by_exposure <- bi_full %>%
  group_by(exposure_bin, solar_system) %>%
  summarise(avg_claim_rate = mean(claim_count/exposure, na.rm=TRUE),
            total_policies = n(), .groups="drop")

ggplot(freq_by_exposure, aes(x=exposure_bin, y=avg_claim_rate, fill=solar_system)) +
  geom_col(position=position_dodge(width=0.8)) +
  labs(title="Average Claim Rate by Exposure Bin", x="Exposure Bin", y="Avg Claim Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

heatmap_summary2 <- bi_full %>%
  group_by(solar_system, maintenance_freq) %>%
  summarise(total_claims=sum(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(heatmap_summary2, aes(x=as.factor(maintenance_freq), y=solar_system, fill=total_claims)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  labs(title="Heatmap: Total Claims by Maintenance Frequency", x="Maintenance Frequency", y="Solar System") +
  theme_minimal()
