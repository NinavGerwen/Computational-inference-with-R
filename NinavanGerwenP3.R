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
  # create a for loop in which a variable i loops a total of 'numsim' times (which the user can specify)
  for(i in 1:numsim) {
    X <- rnorm(5, mu, sigma)        # create three new vectors that take respectively 5/100/100000 random values
    Y <- rnorm(100, mu, sigma)      # from a normal probability with the mean and standard deviation 
    Z <- rnorm(10000, mu, sigma)    # specified by the user through rnorm()
    
    # The next step is to calculate the mean of those three variables
    # and add them to the correct memory base on the ith element
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

# To test, run the following example

SamplingMeanNorm(2, 1.2, 1000)

# I believe it works!

## Exercise B

## if multiple of 2 (AKA if even number) --> get the median value bij getting the 2 middle values and dividing by 2
## else odd number,= just get the median value

# First set-up median function

# 5.1: Commenting the function 

example.data <- matrix(c(2,5,14,23,18,9,11), nrow=7, ncol=1)

ComputeMedian <- function(data){          # creates a function_name that once specified will perform the function
  n <- length(data)                       # create within the function a variable n which is the length of the dataset
  sorted.data <- sort(data)               # sort the dataset (locally) from low to high
  median.value <- sorted.data[(n+1)/2]    # create the variable median.value which is the middle element of the sorted
  list(median.value = median.value)       # dataset. Which element to extract is specified through adding 1 to 
                                          # the length n and dividing by 2, which should be the middle element
}                                 

# 5.2: Running the function

ComputeMedian(example.data)

# Running the function on the example.dataset gives the value 11, which is in fact the median of the dataset

# 5.3: The problem...

# The problem with the current function is that on a dataset with an even amount of elements, the median value
# won't be correct. Take the following example:

data.problem <- matrix(c(1:8), 8, 1)

ComputeMedian(data.problem)

# Here it gives the value 4, however the median of 1:8 is actually (4 + 5) / 2 = 4.5
# The reason the current function doesn't work is because of the specification on which element to extract
# In an oddnumbered dataset, this calculation would work, but not on an evennumbered dataset

# 5.4: A more complete function


ComputeAllMedians <- function(data){
  n <- length(data)
  sorted.data <- sort(data)
  # now I create an if statement, stating that if the length of the data set modulo 2 is not 0 (i.e.,
  # if the data set is an odd numbered vector), use the previous method for calculating the median
  if(n %% 2 != 0) {
    median.value <- sorted.data[(n+1)/2]    
    list(median.value = median.value)
    } else { 
    # otherwise, (i.e., if the data set is an even numbered vector) use the following function:
    sum.two.middle.elements <- sum(sorted.data[c((n)/2, ((n)/2)+1)])
    # take the sum of the two middle elements in the data set
    median.value <- sum.two.middle.elements/2
    # calculate the median value by dividing the sum of the two middle elements by two
    list(median.value = median.value)
    # list the correct median value
  }
}

# Now the function works for both odd and even numbered vectors!

ComputeAllMedians(data.problem) == median(data.problem)
ComputeAllMedians(example.data) == median(example.data)
