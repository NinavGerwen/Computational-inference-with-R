## Nina van Gerwen, 1860852
## Assignment 2, Statistical Inference with R

## Part 1: Programming your own t-test function

## Question 1:

## For a t-test, someone should have two vectors as input, so the function should also ask for two vectors
myTtest <- function(x, y) {
  ## First, ask and define a few values necessary to calculate the t-test
  n_x <- length(x)
  n_y <- length(y)
  ## now, we  calculate the pooled sd through the following formula, using the built-in function var()
  pooled_s <- sqrt(((n_x - 1)*(var(x)) + (n_y - 1)*(var(y)))/(n_x + n_y - 2))
  ## knowing the pooled_s, we can calculate the t-statistic through the final formula using the built-in function mean()
  t_test <- (mean(x) - mean(y))/(pooled_s * sqrt((1/n_x)+(1/n_y)))
  ## Question 3:
  
  ## To get the p-value, we use the built-in function pt(), in this function, we need to define the t-statistic (gained above) and the degrees of freedom
  ## Furthermore, to get the correct p-value, we should make an if-statement, because:
  if(t_test < 0) {
    ## if the t-value is below 0, it means we need to gain the p-value for the lower tail
    p_value <- 2 * pt(t_test, df = n_x + n_y - 2, lower.tail = TRUE)
  } else {
    ## if the t-value is above 0, it means we need to gain the p-value for the upper tail
    p_value <- 2 * pt(t_test, df = n_x + n_y - 2, lower.tail = FALSE)
    ## Finally, in both cases the p-value should be multiplied by 2, because we are doing a two-tailed test
  }
  ## create a list of values that we wish to have as output
  list_of_values <- list(t_test, p_value)
  ## return the list of values
  return(list_of_values)
}

## Question 2: 

## To test whether the results match, create the two factors and run it with both t-tests
CSFI <- c(2,5,5,6,6,7,8,9)
TFI <- c(1,1,2,3,3,4,5,7,7,8)

t.test(CSFI, TFI, var.equal = TRUE)
myTtest(CSFI, TFI)

t.test(TFI, CSFI, var.equal = TRUE)
myTtest(TFI, CSFI)

## Under the assumption that variances are equal (which is what is assumed in the formula from Question 1), the results match!