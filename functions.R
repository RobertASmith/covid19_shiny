# Simulation Function from David B Sparkes

sim_function <- function(
  n_population = 10000,
  n_days = 100,
  time_to_quarantine = 14,
  n_encounters = 50,
  transmission_probability = 0.05,
  n_initial = 1
){
  condition_matrix<- matrix(nrow = n_population,
                            ncol = n_days,
                            0)
  condition_matrix[1:n_initial, 1] <- 1
  condition_counts <- matrix(nrow = 3, ncol = n_days, NA)
  current_count <- c(table(condition_matrix[, 1])[c("-1", "0", "1")])
  current_count <- replace_na(current_count, 0)
  condition_counts[, 1] <- current_count
  colnames(condition_counts) <- 1:n_days
  rownames(condition_counts) <- c(-1, 0, 1)
  
  for(jj in 2:ncol(condition_matrix)){
    prop_infected <- mean(condition_matrix[, jj-1] == 1)
    
    # Number of encounters with infected
    expected_encounters_with_infected <- n_encounters * prop_infected
    n_encounters_with_infected <- rbinom(n_population, n_encounters, prop_infected)
    
    # Whether individual contracted virus
    prop_did_not_contract <- (1 - transmission_probability) ^ n_encounters_with_infected
    contracted <- rbinom(n_population, 1, 1-prop_did_not_contract)
    condition_matrix[, jj] <- contracted
    
    # Plus anyone who already had it
    condition_matrix[, jj][condition_matrix[, jj-1] == 1] <- 1
    condition_matrix[, jj][condition_matrix[, jj-1] == -1] <- -1
    
    # Go under quarantine
    condition_recognized <- rowSums(condition_matrix[, 1:jj] == 1) >= time_to_quarantine
    condition_matrix[condition_recognized, jj] <- -1
    
    current_count <- c(table(condition_matrix[, jj])[c("-1", "0", "1")])
    current_count <- replace_na(current_count, 0)
    condition_counts[, jj] <- current_count
  }
  
  n_infected_by_day <- colSums(condition_counts[-2, ])
  n_newly_infected <- c(0, colSums(condition_matrix[, -1] == 1 & condition_matrix[, -ncol(condition_matrix)] == 0))
  
  tibble(day = 1:n_days,
         n_infected_by_day,
         n_newly_infected) %>%
    return()
}
