## Nina van Gerwen, 1860852

## Practical 4

## Exercise A: Speed comparisons

set.seed(9)

## A.1: 

## Create a test matrix that consists of 1 million random numbers over 1000 rows and columns
testmatrix <- matrix(data = runif(n = 1000000, min = 1, max = 100), nrow = 1000, ncol = 1000)

column_sums <- function(x) {
  ## test if the argument given is a matrix
  if(is.matrix(x) == TRUE) {
    ## if it is a matrix, create space for the sums
    column_space <- rep(0, ncol(x))
    for(i in 1:ncol(x)) {
      ## calculate the sum of the ith row
      ## put this calculated sum into the i'th element of the newly created space
      column_space[i] <- sum(x[, i])
    } 
  } else {
  ## if the argument is not a matrix, state so and ask for a matrix
  print(c(x, "is not a matrix."))
  }
  ## return the sums of all rows from the created space variable
  return(column_space)
  }

## Try out with our test matrix
column_sums(testmatrix)

## Test the time the function takes: 0.01 / 0.00 / 0.01
system.time(column_sums(testmatrix))

## A.2:

matrixmultiplication <- function(x) {
  if(is.matrix(x) == TRUE) {
    ## create a vector of same row-length as matrix consisting of only 1s
    Dummy_Vector <- rep(1, nrow(x)) 
    ## Multiply the matrix with the new vector
    new_matrix <- x %*% Dummy_Vector
  } else {
    ## if not a matrix, state so
    print(c(x, "is not a matrix."))
  }
  ## return the newly multiplied matrix
  return(new_matrix)
}

## Try out with our test matrix
matrixmultiplication(testmatrix)

## the time it takes is 0.01 / 0.00 / 0.02
system.time(matrixmultiplication(testmatrix))

## A.3:

## create an apply function on the test matrix, asking for the sums of the columns
apply(testmatrix, 2, sum)

## the time the apply function takes is: 0.02, 0.00, 0.03
system.time(apply(testmatrix, 2, sum))

## Exercise B: Matrix algebra

library(MASS)

## B.1: Column means

## with the data set Animals, divide the sums of the columns by the number of rows to get the column means
Animal_colmeans <- with(Animals, column_sums(Animals) / nrow(Animals))
Animal_colmeans
## check to see if it matches: it does!
with(Animals, colMeans(Animals))

## B.2: Centering the matrix

## with data set Animals, subtract from all elements in the matrix the column mean 
## done by multiplying the column means by a matrix consisting of ones of same row length as the initial matrix
cA <- with(Animals, Animals - matrix(1, nrow = nrow(Animals), ncol = 1) %*% (column_sums(Animals) / nrow(Animals)))

## with these options, the scale() function gives the same results!
with(Animals, scale(Animals, center = TRUE, scale = FALSE))

## B.3: Getting the SSCP and Covariance Matrix
## make a matrix out of the centered Animals vector
cA <- as.matrix(cA)
## calculate the SSCP by multiplying the transpose of cA with cA
SSCP <- t(cA)%*% cA
## calculate the covariance atrix by dividing the SSCP by the number of rows of the Animal dataset minus one
CovarianceMatrix <- SSCP/(nrow(Animals) -1)
CovarianceMatrix

## Check whether it is correct, yes it is!
with(Animals, cov(Animals))

## B.4: Getting the correlation matrix

Inv_SD_D <- sqrt(diag(1/CovarianceMatrix))

Diag_Matrix <- diag(Inv_SD_D, nrow(CovarianceMatrix), ncol(CovarianceMatrix))

CorrelationMatrix <- Diag_Matrix %*% CovarianceMatrix %*% Diag_Matrix
CorrelationMatrix

## Create 
with(Animals, cor(Animals))
