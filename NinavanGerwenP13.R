## Practical 13: Numerical optimisation using R

## Part A: Logistic regression with the Newton-Raphson method

## Question 1:

## Create a function in which you have to provide:
    ## a matrix as the first argument - X
    ## a binary dependent variable (can be a vector) - y 
    ## the maximum amount of iterations (default = 30)
    ## a tolerance value (default = 1E-6)
logregNewtonRaphson <- function(X, y, max.iter=30, tol=1E-6){
  ## First, through cbind, add a column at the beginning of X
  ## in order to get the intercept coefficient
  X <- cbind(1, X)
  ## Create starting beta values of 0 as the begin value for the algorithm
  ## the length is equal to the number of columns of X because this is
  ## how many coefficients there will be in the final model
  beta <- beta.last <- rep(0, ncol(X))
  ## start at the first iteration
  it <- 1
  ## while the number of iterations is smaller or equal to the maximum
  ## around of iterations, do the following lines of code:
  while (it <= max.iter){
    ## First, calculate the predicted probability of a 1 given the beta coefficients
    p <- as.vector(1/(1 + exp(-X %*% beta)))
    ## Then make a diagonal matrix out of the calculated p values times 1 minus the p-values
    W <- diag(p * (1 - p))
    ## Through matrix multiplication, gain the inverse of a Hessian matrix 
    var.beta <- solve(t(X) %*% W %*% X)
    ## Calculate the new 
    beta <- beta + var.beta %*% t(X) %*% (y - p)
    ## Question for Rene: why is the function "max" used here?
    if (max(abs(beta - beta.last)/(abs(beta.last)
                                   + 0.01*tol)) < tol)
      break
    beta.last <- beta
    it <- it + 1
  }
  if (it > max.iter)
    warning("maximum iterations exceeded")
  list(coefficients=as.vector(beta), var=var.beta,
       iterations=it)
}

