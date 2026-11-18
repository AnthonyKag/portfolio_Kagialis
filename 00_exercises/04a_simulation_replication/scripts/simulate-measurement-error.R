# Adapted from the simulation loop in the course's original_script.R.
simulate_measurement_error <- function(data, scenarios, n_sim) {
  n_people <- nrow(data)
  n_scenarios <- nrow(scenarios)
  
  # p is the fraction of the noisy measurement's variance due to error.
  # Convert p to an error SD using sqrt(original variance * p / (1 - p)).
  p_hba1c <- scenarios$exposure_error
  p_bmi <- scenarios$confounder_error
  hba1c_sd <- sqrt(var(data$HbA1C) * p_hba1c / (1 - p_hba1c))
  bmi_sd <- sqrt(var(data$bmi) * p_bmi / (1 - p_bmi))
  
  # Store one HbA1C coefficient per repetition (row) and scenario (column).
  estimates <- matrix(NA_real_, nrow = n_sim, ncol = n_scenarios)
  
  for (k in seq_len(n_sim)) {
    set.seed(k)  # Make this repetition reproducible.
    
    for (i in seq_len(n_scenarios)) {
      # Add one random error per person to each original measurement.
      data$HbA1C.me <- data$HbA1C + rnorm(n_people, sd = hba1c_sd[i])
      data$bmi.me <- data$bmi + rnorm(n_people, sd = bmi_sd[i])
      
      # Fit the model and save the coefficient for noisy HbA1C.
      fit <- lm(bp ~ HbA1C.me + bmi.me + age + as.factor(sex), data = data)
      estimates[k, i] <- coef(fit)[["HbA1C.me"]]
    }
  }
  
  estimates
}