## Nina van Gerwen, Statistical Inference with R
## Practical 14: Generating data from probability distributions

## Monte Carlo Integration

library(tidyverse)

## Create a function that requires three inputs:
## a lower and upper bound of the integral, and the amount of numbers to be drawn
## pseudo-randomly (by default these numbers are 0, 1 and 10000)
MonteCarloIntegration <- function(LB = 0, UB = 1, n = 10000) {
  ## The package then loads tidyverse, as it uses the pipeline function
  library(tidyverse)
  ## Then it generates n random observations from a standard normal distribution
  ## (a normal distribution with mean = 0 and standard deviation = 1)
  generated_numbers <- rnorm(n = n, mean = 0, sd = 1)
  ## From these observations, the ones within the integral limits are then subsetted
  generated_numbers <- generated_numbers %>% subset(generated_numbers >= LB & generated_numbers <= UB)
  ## And with these numbers, the integral is approximated through the following function:
  solved_integral <- sum(exp(-((generated_numbers^2))/2))/((sqrt(2*pi))*length(generated_numbers))
  ## Finally, the function should return the value of the approximated integral
  return(solved_integral)
}

MonteCarloIntegration(LB = 0, UB = 1, n = 10000)
## It seems to work for lowerbound of 0 and upperbound of 1. As it gives a value
## of 34. This value coincides with the standard normal distribution where
## around 34% of values are between the the mean and a standard deviation of 1.