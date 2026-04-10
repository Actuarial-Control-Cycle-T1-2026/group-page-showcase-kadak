# =============================================================
# 8. Stress Testing
# =============================================================
# 8.1 Stress Levels
freq_levels <- c(1.25,1.5,1.75,2)
sev_levels  <- c(1.25,1.5,1.75,2)

production_levels  <- c(1.1,1.25,1.5,1.75,2)
energy_levels      <- c(1.1,1.25,1.5,1.75,2)
supply_levels      <- c(1.1,1.25,1.5,1.75,2)
maintenance_levels <- c(1.1,1.25,1.5,1.75,2)

crew_levels   <- c(0.6,0.8,1,1.2,1.4)
safety_levels <- c(0.8,1,1.2,1.4,1.6)

exposure_levels   <- c(1.25,1.5,1.75,2)
inflation_levels  <- c(1,1.05,1.1,1.15,1.2,1.3,1.4,1.5)

# 8.2 Stress Test Function
run_stress_test <- function(stress_levels, variable_name, category) {
  stress_levels <- c(1, stress_levels)
  results <- data.frame()
  
  for(s in stress_levels){
    freq_temp <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat * s)
    n_iter <- 5
    aggregate_losses <- numeric(n_iter)
    
    for(i in 1:n_iter){
      loss <- sum(sapply(freq_temp, function(n){
        if(n == 0) return(0)
        sum(rgamma(n, shape = sev_shape, scale = sev_scale))
      }))
      aggregate_losses[i] <- loss
    }
    
    aggregate_losses <- aggregate_losses * 0.3380428
    results <- rbind(results,
                     data.frame(`Risk Factor` = paste(category, "-", variable_name),
                                `Stress Factor` = s,
                                `Mean Loss ($)` = mean(aggregate_losses),
                                `VaR 99% ($)`  = quantile(aggregate_losses, 0.99)))
  }
  
  return(results)
}

# 8.3 Run Stress Tests by Category
core_freq_results <- run_stress_test(freq_levels, "Claim Frequency", "Core Insurance Risk")
core_sev_results  <- run_stress_test(sev_levels, "Claim Severity", "Core Insurance Risk")
oper_prod_results <- run_stress_test(production_levels, "Production Load", "Operational Risk")
oper_energy_results <- run_stress_test(energy_levels, "Energy Backup Score", "Operational Risk")
oper_supply_results <- run_stress_test(supply_levels, "Supply Chain Index", "Operational Risk")
oper_maint_results  <- run_stress_test(maintenance_levels, "Maintenance Frequency", "Operational Risk")
work_crew_results   <- run_stress_test(crew_levels, "Crew Experience", "Workforce Risk")
work_safety_results <- run_stress_test(safety_levels, "Safety Compliance", "Workforce Risk")
port_exposure_results <- run_stress_test(exposure_levels, "Exposure Growth", "Portfolio Risk")
fin_inflation_results <- run_stress_test(inflation_levels, "Claims Inflation", "Financial Risk")

# 8.4 Combine Stress Test Results
stress_results_final <- rbind(
  core_freq_results, core_sev_results,
  oper_prod_results, oper_energy_results, oper_supply_results, oper_maint_results,
  work_crew_results, work_safety_results,
  port_exposure_results,
  fin_inflation_results
)

colnames(stress_results_final) <- c("Risk_Factor", "Stress_Factor", "Mean_Loss", "VaR99")
print(stress_results_final)

# 8.5 Stress Test Plot
ggplot(stress_results_final,
       aes(x = Stress_Factor, y = VaR99, group = Risk_Factor, color = Risk_Factor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Risk_Factor, scales = "free_x") +
  theme_minimal() +
  labs(title = "Business Interruption Stress Testing by Risk Factor",
       x = "Stress Factor", y = "VaR 99% ($)") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none")
