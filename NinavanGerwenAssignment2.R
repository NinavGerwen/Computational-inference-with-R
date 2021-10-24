## Nina van Gerwen, 1860852
## Assignment 2, Statistical Inference with R

## Part 1: Programming your own t-test function -----------------------------------------

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
  ## Question 4: 
  
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

## Part 2: Multiple regression analysis and matrix multiplication ------------------------

## Question 1:

setwd("C:/Users/nina-/Documents/GitHub/Computational-inference-with-R/")

## First load the correct file with the right separator specified 
gala_data <- read.csv(file = "gala.txt", sep = " ")

## Perform the multiple regression through the lm() function specifying the correct variables through the formula 
## Important to note is that we want to remove the intercept (by using + 0), because otherwise we won't be able to compare the coefficients
regression_model <- lm(Species ~ Area + Elevation + Endemics + 0, data = gala_data)
## Gain the correct statistics/estimates using the summary() function
summary(regression_model)

## Create a simple scatterplot of the predicted (also known as fitted) values against the residuals
plot(regression_model$fitted.values, regression_model$residuals)

## Interpretation of the results:
## 

## Question 2:

## The regression coefficients:
DV <- matrix(gala_data$Species, ncol = 1, nrow = 30)
IVs <- matrix(c(gala_data$Area, gala_data$Elevation, gala_data$Endemics), ncol = 3, nrow = 30)

b_coefficients <- (solve(t(IVs) %*% IVs)) %*% (t(IVs) %*% DV)
b_coefficients

## The predicted values:
predicted_values <- IVs %*% b_coefficients
predicted_values

## The residuals:
error_residuals <- DV - predicted_values
error_residuals

## Question 3:

## First to specify the arguments:
## Most importantly, a dataset should be given as input
## Then from this dataset, the dependent variable (DV) should be specified (column x from the dataset)
## Then finally  independent variable(s) (IV) should be specified, we use ... because it can be any number of variables/columns

Regression_Function <- function(data, DV, IVs) { ## question: how to allow IV to be any number of things
  ## Descriptive statistics of data:
  
  ## Regression coefficients, predicted values and residuals using matrix algebra:
  for(i in 1:ncol(IVs)) { 
    
  
  }
  
  ## Simple plot of predicted values against the residuals:

  return(list_of_information)
}

Regression_Function()
