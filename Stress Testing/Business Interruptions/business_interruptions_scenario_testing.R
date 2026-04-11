# =============================================================
# Scenario Testing (Portfolio-Level)
# =============================================================
scenario_results <- data.frame(
  `Risk Factor`   = character(),
  `Stress Factor` = character(),
  `Mean Loss ($)` = numeric(),
  `VaR 99% ($)`   = numeric(),
  stringsAsFactors = FALSE
)

scenarios <- data.frame(
  scenario = c("Best Case", "Moderate Case", "Worst Case"),
  freq     = c(0.75, 1, 2),
  sev      = c(0.9, 1, 2)
)

n_iter <- 50

for(i in 1:nrow(scenarios)){
  row <- scenarios[i,]
  freq_temp <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat * row$freq)
  sev_scale_stress <- sev_scale * row$sev
  aggregate_losses <- numeric(n_iter)
  
  for(j in 1:n_iter){
    loss <- sum(sapply(freq_temp, function(n){
      if(n == 0) return(0)
      sum(rgamma(n, shape = sev_shape, scale = sev_scale_stress))
    }))
    aggregate_losses[j] <- loss
  }
  
  aggregate_losses <- aggregate_losses * 0.3380428
  
  scenario_results <- rbind(scenario_results,
                            data.frame(`Risk Factor` = "Portfolio Scenario",
                                       `Stress Factor` = row$scenario,
                                       `Mean Loss ($)` = mean(aggregate_losses),
                                       `VaR 99% ($)`   = quantile(aggregate_losses,0.99)))
}

print(scenario_results)
