library(copula)

#######################################
# Stress Levels for Testing
#######################################

#frequency & severity 
freq_stress_levels <- c(1.25, 1.5, 1.75, 2.0)   
sev_stress_levels  <- c(1.25, 1.5, 1.75, 2.0)        
length_stress_levels <- c(1.25, 1.5, 1.75, 2.0)   

#operational risk 
exposure_levels <- c(1.25, 1.5, 1.75, 2.0)
hours_levels <- c(1.5, 2.0, 2.5)         
supervision_levels <- c(1.4, 1.2, 1, 0.8, 0.6, 0.4)
experience_levels <- c(1.4, 1.2, 1, 0.8, 0.6, 0.4)

#safety and training 
training_stress_levels <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)
gravity_stress_levels <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)

#psychological impact
psych_stress_levels <- c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0)
correlation_levels <- c(0.1, 0.3, 0.5, 0.7, 0.9, 1)

#occupation 
occ_levels <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)
worker_levels <- c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0)

#financial / economic
inflation_levels <- c(1, 1.05, 1.10, 1.15, 1.20, 1.30, 1.35, 1.4, 1.45, 1.5)
salary_levels <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)  

#######################################
#1. Frequency Stress Test
#######################################
freq_results <- data.frame()

for(f in freq_stress_levels){
  
  freq_temp <- round(freq_sim * f)
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  freq_results <- rbind(freq_results,
                        data.frame(
                          stress_factor = f,
                          mean_loss = mean(loss_temp),
                          VaR_99 = VaR,
                          TVaR_99 = mean(loss_temp[loss_temp > VaR])
                        ))
}

freq_results

#######################################
#2. Severity Stress Test
#######################################
sev_results <- data.frame()

for(s in sev_stress_levels){
  
  sev_scale_stress <- sev_scale * s
  
  loss_temp <- sapply(freq_sim, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  sev_results <- rbind(sev_results,
                       data.frame(
                         stress_factor = s,
                         mean_loss = mean(loss_temp),
                         VaR_99 = VaR,
                         TVaR_99 = mean(loss_temp[loss_temp > VaR])
                       ))
}

sev_results

#######################################
#3. Claim Length Stress Test
#######################################

claim_length <- na.omit(worker_data_sev$claim_length)

daily_wage <- mean(worker_data_sev$claim_amount) / mean(claim_length)

length_results <- data.frame()

for(l in length_stress_levels){
  
  length_stress <- claim_length * l
  severity_temp <- length_stress * daily_wage
  
  loss_temp <- sapply(freq_sim, function(n){
    
    if(n == 0) return(0)
    
    sum(sample(severity_temp, n, replace = TRUE))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  length_results <- rbind(length_results,
                          data.frame(
                            stress_factor = l,
                            mean_loss = mean(loss_temp),
                            VaR_99 = VaR,
                            TVaR_99 = mean(loss_temp[loss_temp > VaR])
                          ))
}

length_results

#######################################
# Combine Stress Results for first 3 
#######################################
#table format 
freq_results$test_type   <- "Frequency"
sev_results$test_type    <- "Severity"
length_results$test_type <- "Claim Length"

stress_results <- rbind(freq_results,
                        sev_results,
                        length_results)

stress_results

#######################################
#4. Exposure Stress Test
#######################################

exposure_results <- data.frame()

for(e in exposure_levels){
  
  freq_temp <- round(freq_sim * e)
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  exposure_results <- rbind(exposure_results,
                            data.frame(
                              stress_factor = e,
                              mean_loss = mean(loss_temp),
                              VaR_99 = VaR,
                              TVaR_99 = mean(loss_temp[loss_temp > VaR])
                            ))
}
exposure_results
#######################################
#5. Hours Worked Stress Test
#######################################

hours_results <- data.frame()

for(h in hours_levels){
  
  freq_temp <- round(freq_sim * h)
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  hours_results <- rbind(hours_results,
                         data.frame(
                           stress_factor = h,
                           mean_loss = mean(loss_temp),
                           VaR_99 = VaR,
                           TVaR_99 = mean(loss_temp[loss_temp > VaR])
                         ))
}
hours_results
#######################################
#6. Supervision Stress Test
#######################################

supervision_results <- data.frame()

for(s in supervision_levels){
  
  freq_temp <- round(freq_sim * (1 + (1 - s)))
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  supervision_results <- rbind(supervision_results,
                               data.frame(
                                 stress_factor = s,
                                 mean_loss = mean(loss_temp),
                                 VaR_99 = VaR,
                                 TVaR_99 = mean(loss_temp[loss_temp > VaR])
                               ))
}
supervision_results
#######################################
#7. Experience Stress Test
#######################################

experience_results <- data.frame()

for(x in experience_levels){
  
  freq_temp <- round(freq_sim * (1 + (1 - x)))
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  experience_results <- rbind(experience_results,
                              data.frame(
                                stress_factor = x,
                                mean_loss = mean(loss_temp),
                                VaR_99 = VaR,
                                TVaR_99 = mean(loss_temp[loss_temp > VaR])
                              ))
}
experience_results
#######################################
#8. Training / PPE Stress Test
#######################################

training_results <- data.frame()

for(t in training_stress_levels){
  
  sev_scale_stress <- sev_scale * t
  
  loss_temp <- sapply(freq_sim, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  training_results <- rbind(training_results,
                            data.frame(
                              stress_factor = t,
                              mean_loss = mean(loss_temp),
                              VaR_99 = VaR,
                              TVaR_99 = mean(loss_temp[loss_temp > VaR])
                            ))
}
training_results
#######################################
#9. Gravity / Hazard Stress Test
#######################################

gravity_results <- data.frame()

for(g in gravity_stress_levels){
  
  freq_temp <- round(freq_sim * g)
  
  sev_scale_stress <- sev_scale * g
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  gravity_results <- rbind(gravity_results,
                           data.frame(
                             stress_factor = g,
                             mean_loss = mean(loss_temp),
                             VaR_99 = VaR,
                             TVaR_99 = mean(loss_temp[loss_temp > VaR])
                           ))
}
gravity_results
#######################################
#10. Psychological Stress Test
#######################################

psych_results <- data.frame()

for(s in psych_stress_levels){
  
  freq_temp <- round(freq_sim * s)
  
  sev_scale_stress <- sev_scale * (1 + (s - 1)*0.5)
  
  loss_temp <- sapply(freq_temp, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  psych_results <- rbind(psych_results,
                         data.frame(
                           stress_factor = s,
                           mean_loss = mean(loss_temp),
                           VaR_99 = VaR,
                           TVaR_99 = mean(loss_temp[loss_temp > VaR])
                         ))
}
psych_results
#######################################
#11. Claims Inflation Stress Test
#######################################

inflation_results <- data.frame()

for(i in inflation_levels){
  
  sev_scale_stress <- sev_scale * i
  
  loss_temp <- sapply(freq_sim, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  inflation_results <- rbind(inflation_results,
                             data.frame(
                               stress_factor = i,
                               mean_loss = mean(loss_temp),
                               VaR_99 = VaR,
                               TVaR_99 = mean(loss_temp[loss_temp > VaR])
                             ))
}
inflation_results
#######################################
#12. Salary Linked Claims Stress Test
#######################################

salary_results <- data.frame()

avg_salary <- mean(worker_data_sev$base_salary, na.rm = TRUE)

for(s in salary_levels){
  
  salary_multiplier <- s
  
  sev_scale_stress <- sev_scale * salary_multiplier
  
  loss_temp <- sapply(freq_sim, function(n){
    
    if(n == 0) return(0)
    
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
    
  })
  
  VaR <- quantile(loss_temp,0.99)
  
  salary_results <- rbind(salary_results,
                          data.frame(
                            stress_factor = s,
                            mean_loss = mean(loss_temp),
                            VaR_99 = VaR,
                            TVaR_99 = mean(loss_temp[loss_temp > VaR])
                          ))
}
salary_results

#######################################
# Combined Results for external factors (4-12)
#######################################

stress_results_final <- rbind(
  transform(exposure_results, test="Exposure"),
  transform(hours_results, test="Hours"),
  transform(supervision_results, test="Supervision"),
  transform(experience_results, test="Experience"),
  transform(training_results, test="Training/PPE"),
  transform(gravity_results, test="Gravity Hazard"),
  transform(psych_results, test="Psych Stress"),
  transform(inflation_results, test="Claims Inflation"),
  transform(salary_results, test="Salary Linked")
)

ggplot(stress_results_final,
       aes(x = stress_factor,
           y = VaR_99,
           color = test)) +
  geom_line() +
  geom_point() +
  labs(title = "Workers Compensation Stress Testing",
       x = "Stress Multiplier",
       y = "VaR 99%") +
  theme_minimal()


#######################################
# Tail Dependence
#######################################
#extract claim freq and sev 
freq <- worker_data_freq$claim_count
freq_pos <- freq[freq > 0]
sev <- worker_data_sev$claim_amount
n <- min(length(freq_pos), length(sev))

freq_dep <- freq_pos[1:n]
sev_dep  <- sev[1:n]
dep_data <- data.frame(freq_dep, sev_dep)

# Convert data to pseudo-observations (uniform [0,1]) using ranks for copula fitting
u <- rank(dep_data$freq_dep) / (n + 1)
v <- rank(dep_data$sev_dep)  / (n + 1)

uv_data <- cbind(u, v)
t_cop <- tCopula(dim = 2)

fit_cop <- fitCopula(t_cop, uv_data, method = "ml")

summary(fit_cop)

#######################################
# Copula Simulation
#######################################
set.seed(123)
n_sim <- 500000

cop_sim <- rCopula(n_sim, fit_cop@copula)

u_sim <- cop_sim[,1]
v_sim <- cop_sim[,2]

lambda_hat <- predict(nb_freq, type = "response")
lambda_mean <- mean(lambda_hat)

freq_sim <- qnbinom(u_sim,
                    size = nb_freq$theta,
                    mu = sample(lambda_hat, n_sim, replace = TRUE))

# Convert lognormal GLM dispersion to gamma shape parameter
sev_shape <- 1 / summary(lognormal_sev)$dispersion
sev_scale <- mean(worker_data_sev$claim_amount) / sev_shape

sev_sim <- qgamma(v_sim,
                  shape = sev_shape,
                  scale = sev_scale)

aggregate_loss <- numeric(n_sim)

for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    aggregate_loss[i] <- sum(rgamma(freq_sim[i],
                                    shape = sev_shape,
                                    scale = sev_scale))
  }
}

#######################################
# Risk Measures 
#######################################
WC_mean_loss <- mean(aggregate_loss)
WC_sd_loss   <- sd(aggregate_loss)

WC_VaR_99  <- quantile(aggregate_loss, 0.99)
WC_TVaR_99 <- mean(aggregate_loss[aggregate_loss > WC_VaR_99])
lambda(fit_cop@copula)


#######################################
# Testing Various Other Copula for Comparison
#######################################

#intitial parameter guesses 
norm_cop <- normalCopula(param = 0.5, dim = 2)     # Gaussian
clay_cop <- claytonCopula(param = 2, dim = 2)      # Lower tail dependence
gumb_cop <- gumbelCopula(param = 2, dim = 2)       # Upper tail dependence
t_cop    <- tCopula(param = 0.5, df = 4, dim = 2)  # t-copula, symmetric tails

fit_norm <- fitCopula(norm_cop, uv_data, method = "ml")
fit_clay <- fitCopula(clay_cop, uv_data, method = "ml")
fit_gumb <- fitCopula(gumb_cop, uv_data, method = "ml")
fit_t    <- fitCopula(t_cop, uv_data, method = "ml")

n_sim <- 5000  # smaller for plotting

sim_norm <- rCopula(n_sim, fit_norm@copula)
sim_clay <- rCopula(n_sim, fit_clay@copula)
sim_gumb <- rCopula(n_sim, fit_gumb@copula)
sim_t    <- rCopula(n_sim, fit_t@copula)

#plots of copula 
par(mfrow = c(2,2)) 

plot(sim_norm, main = "Gaussian Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_clay, main = "Clayton Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_gumb, main = "Gumbel Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_t, main = "t-Copula", xlab = "Frequency", ylab = "Severity")


#extreme scenario risk 
quantile(aggregate_loss, 0.995) ##solvency risk 
quantile(aggregate_loss, 0.999) ##catastrophic risk 

