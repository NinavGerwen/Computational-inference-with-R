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
    ## Calculate the new beta values through the formula of the Newton raphson algorithm
    ## through matrix multiplication
    beta <- beta + var.beta %*% t(X) %*% (y - p)
    ## Question for Rene: why is the function "max" used here?
    ## Why not simply the difference between beta and beta.last
    if (max(abs(beta - beta.last)/(abs(beta.last)
                                   + 0.01*tol)) < tol)
    ## if statement:
    ## if the absolute difference between the new beta and the last beta 
    ## divided by the absolute last beta is below
    ## 0.01 times the specified tolerance value, then stop the iterating
      break
    ## if the absolute difference is not below:
    ## the new beta becomes the last beta
    beta.last <- beta
    ## add + 1 to the iteration count
    it <- it + 1
    ## repeat the while function
  }
  ## If the maximum iterations has been reached, state so
  ## by giving a warning
  if (it > max.iter)
    warning("maximum iterations exceeded")
  ## List the final beta coefficients as a vector, the hessian matrix and the number of iterations
  list(coefficients=as.vector(beta), var=var.beta,
       iterations=it)
}

## Question 2: Comments added to the code

## Question 3: Load the data

load("heartdata.Rdata")

## Question 4: Logistic regression through the glm() function 

logreg <- glm(chd ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity + alcohol + age, data = heartdata, family = "binomial")
summary(logreg)

## Question 5:

str(heartdata)
heartdata$famhist <- as.numeric(heartdata$famhist)

Y <- heartdata$chd
X <- as.matrix(heartdata[, -c(1, 11)])

logreg$coefficients
logregNewtonRaphson(X, Y)$coefficients

## Conclusion: the coefficients match! 