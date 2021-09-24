## Nina van Gerwen, 1860852

## Exercise A: Using a 'for' loop

setwd("C:/Users/nina-/Documents/Master/Computational inference with R/")

# Sampling distribution of the mean of a normal distribution.
# Learning to use a for loop.

# numsim = the number of samples taken
# mu, sigma = values of the normal distribution parameters, to
# be provided by the user


SamplingMeanNorm <- function(mu, sigma, numsim){
  # create memory for the sampled values for the three different sample sizes
  mean5 <- rep(0,numsim)
  mean100 <- rep(0,numsim)
  mean10000 <- rep(0,numsim)
    # fill in the missing part here 
  # use rnorm (for n, with mean and sd, give n values)
  # and for loop
  for(i in 1:numsim) {
    X <- rnorm(5, mu, sigma)
    Y <- rnorm(100, mu, sigma)
    Z <- rnorm(10000, mu, sigma)
    # now calculate the mean of these 3 things
    # add that mean to respectively mean5/100/100000
    mean5[i] <- mean(X)
    mean100[i] <- mean(Y)
    mean10000[i] <- mean(Z)
  }
  # store the output in a list (separate elements of the list
  # can be called with $ sign.)
  list(mean5=mean5, mean100=mean100, mean10000=mean10000)
  # Makes a boxplot of the three different sample sizes: 5, 100, 1000.
  boxplot(mean5,mean100,mean10000,
          names=c("n=5","n=100","n=10000"))
  title("Distribution of Means from Normal Distribution")
}

SamplingMeanNorm(2, 1.2, 1000)

## Exercise B

## if multiple of 2 (AKA if even number) --> get the median value bij getting the 2 middle values and dividing by 2
## else odd number,= just get the median value