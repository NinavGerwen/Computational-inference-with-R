## Practical 11 

## The data:

V1 <- c(10, 11, 11, 10, 12)
V2 <- c(12, 10, 14, 13, 12)

MyPermutationTest <- function(x, y) {
  ## initial t-value
  initial_t <- t.test(x, y)$statistic
  
  t_values <- rep(0, 250)
  ## creating 250 permutation samples and calculating t-statistic for each sample
  for(i in 1:250) {
    temp_sample <- sample(x = c(x, y), replace = FALSE)
    ## First XX number of elements go to the first group...
    S1 <- temp_sample[1:length(x)]
    ## Then the rest goes to the second group
    S2 <- temp_sample[(length(x) + 1):(length(x)+length(y))]
    
    t_values[i] <- t.test(S1, S2)$statistic
  }
  ## histogram of the t-statistic distribution with a vertical line at the initial t-value
  hist(t_values, breaks = 25)
  abline(v = initial_t, col = "red")
  
  ## proportion of times the t-statistic is larger than the t-statistic value in the original sample
  empirical_p_value <- sum(initial_t > t_values)/250
  
  return(empirical_p_value)
}

MyBootstrapTest <- function(x, y) {
  ## initial t-value
  initial_t <- t.test(x, y)$statistic
  
  t_values <- rep(0, 250)
  ## The bootstrapping
  for(i in 1:250) {
    temp_sample <- sample(x = c(x, y), replace = TRUE)
    ## First XX number of elements go to the first group...
    S1 <- temp_sample[1:length(x)]
    ## Then the rest goes to the second group
    S2 <- temp_sample[(length(x) + 1):(length(x)+length(y))]
    t_values[i] <- t.test(S1, S2)$statistic
  }
  ## histogram of the t-statistic distribution with a vertical line at the initial t-value
  hist(t_values, breaks = 25)
  abline(v = initial_t, col = "red")
  
  ## proportion of times the t-statistic is larger than th et-statistic value in the original sample
  empirical_p_value <- sum(initial_t > t_values)/250
  
  return(empirical_p_value)
}

MyPermutationTest(V1, V2)
MyBootstrapTest(V1, V2)
