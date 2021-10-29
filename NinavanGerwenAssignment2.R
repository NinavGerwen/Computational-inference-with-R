## Nina van Gerwen, 1860852
## Assignment 2, Statistical Inference with R

## Part 1: Programming your own t-test function -----------------------------------------

## Question 1:

## For a t-test, someone should have two vectors as input, so the function should also ask for two vectors
myTtest <- function(x, y) {
  ## First, ask and define a few values (such as length) necessary to calculate the t-test
  n_x <- length(x)
  n_y <- length(y)
  ## now, we  calculate the pooled sd through the following formula, using the built-in function var()
  pooled_s <- sqrt(((n_x - 1)*(var(x)) + (n_y - 1)*(var(y)))/(n_x + n_y - 2))
  ## knowing the pooled_s, we can calculate the t-statistic through the final formula using the built-in function mean()
  t_test <- (mean(x) - mean(y))/(pooled_s * sqrt((1/n_x)+(1/n_y)))
  ## Question 3:
  
  ## To get the p-value, we use the built-in function pt(), in this function, we need to 
  ## define the t-statistic (gained above) and the degrees of freedom
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
  
  ## Create a list of values that we wish to have as output and name the lists
  list_of_values <- list(t_test, n_x + n_y - 2, p_value, c(mean(x), mean(y)))
  names(list_of_values) <- c("t-statistic", "degrees of freedom", "p-value", "Group means of x and y respectively")
  ## return the list of values
  return(list_of_values)
}

## Question 2: 

## To test whether the results match, create the two datasets and run it with both t-tests
CSFI <- c(2,5,5,6,6,7,8,9)
TFI <- c(1,1,2,3,3,4,5,7,7,8)

t.test(CSFI, TFI, var.equal = TRUE)
myTtest(CSFI, TFI)

t.test(TFI, CSFI, var.equal = TRUE)
myTtest(TFI, CSFI)

## Under the assumption that variances are equal (which is what is assumed in the formula from Question 1), 
## the results match!

## Part 2: Multiple regression analysis and matrix multiplication ------------------------

## Question 1:

setwd("C:/Users/nina-/Documents/GitHub/Computational-inference-with-R/")

## First load the correct file with the right separator specified 
gala_data <- read.csv(file = "gala.txt", sep = " ")

## Perform the multiple regression through the lm() function specifying the correct variables through the formula 
regression_model <- lm(Species ~ Area + Elevation + Endemics, data = gala_data)
## Gain the correct statistics/estimates using the summary() function
summary(regression_model)

## Create a simple scatterplot of the predicted (also known as fitted) values against the residuals
plot(regression_model$fitted.values, regression_model$residuals)

## Interpretation of the results:
## A multiple linear regression with the number of species of tortoises (Species) as the dependent variable 
## and Area, Elevation and Endemics as the independent variables shows that together 
## the independent variables significantly predict Species (F(3,26) = 161.8, p < .001).
## The model with the three independent variables explains 94.9% of the variance in Species in the sample.
## However, when looking at the contribution of the individual predictors, only the variable Endemics seems to
## be a significant predictor of Species (b = 4.33, t(26) = 14.22, p < .001). 
## Furthermore, a plot of the predicted values against the residuals (commonly known as a plot that shows
## the homo- or heteroscedasticity of the model) indicates that there is heteroscedasticity.
## This means that the assumption of homoscedasticity has been violated and in turn our p-values might
## be too optimistic. In conclusion, although significant, we should not trust the regression model and
## future research should look into perhaps transforming certain variables or gaining a larger sample size
## in order to gain more trustworthy results.

## Question 2:

## The regression coefficients:

## First, create matrices out of the IVs and DVs with the correct number of columns and rows
DVEx <- matrix(gala_data$Species, ncol = 1, nrow = 30)
## For the IVs, we also add a column that consits of only 1's in order to gain the intercept
IVEx <- matrix(c(gala_data$Area, gala_data$Elevation, gala_data$Endemics, rep(1,30)), ncol = 4, nrow = 30)

## Now, correctly multiply the matrices, using the t() function to gain the transpose of matrices
## and using solve() to gain the inverse of a matrix
b_coefficients <- (solve(t(IVEx) %*% IVEx)) %*% (t(IVEx) %*% DVEx)
b_coefficients

## The predicted values:

## To get the predicted values, we matrix multiply the IVs with the b_coefficients
predicted_y <- IVEx %*% b_coefficients
predicted_y

## The residuals:

## To get the error residuals, we subtract from the true score (the DV), the predicted_y
err_residuals <- DVEx - predicted_y
err_residuals

## The coefficients, predicted values and residuals seem to match the ones from the
## multiple linear regression.

## Question 3:

## First to specify the arguments:
## Most importantly, a dataset should be given as input
## Then from this dataset, the dependent variable (DV) should be specified (by default, the first column of the dataset)
## Then finally the independent variable(s) (IV) should be specified (by default, all columns past the first column)

Regression_Function <- function(dataset, DV = dataset[, 1], IV = dataset[, 2:ncol(dataset)]) {
  ## First, we should change both the DV and IVs into matrices, otherwise we won't be able to do matrix multiplication
  DV <- as.matrix(DV)
  ## For the IV, it is important to also add a column that exists of only 1's so we can get the correct intercept
  IV <- cbind(rep(1,nrow(dataset)), as.matrix(IV))
  ## Descriptive statistics of data: 
    ## We want the sample size, which is equal to the number of rows of the dataset
    Sample_Size <- nrow(dataset)
    ## We want the means of the DV and all IVs
    DV_mean <- mean(DV)
    ## For the IVs, we make use of the colMeans() function
    IV_means <- as.matrix(colMeans(IV[, -1, drop = FALSE]))
    ## We want the standard deviations of all IVs
    ## We gain these through using the apply function, applying the sd() function to all columns in the IV matrix
    IV_sds <- as.matrix(apply(IV[, -1], 2, FUN = sd))
    ## Finally, we column bind the means and standard deviations
    IV_info <- cbind(IV_means, IV_sds)
  ## Put all the descriptive statistics in a list and give them aptly names
  descriptive_statistics <- list(Sample_Size, DV_mean, IV_info)
  names(descriptive_statistics) <- c("Sample Size", "Mean of the Dependent Variable", 
                                     "Mean(s) and Standard Deviation(s) - respectively - of the Independent Variable(s)")
  
  ## Regression coefficients, predicted values and residuals using matrix algebra:
  ## Copy the above used code (with changed variable-names) for matrix multiplication to gain the correct values
    model_coefficients  <- (solve(t(IV) %*% IV)) %*% (t(IV) %*% DV)
    predicted_values <- IV %*% model_coefficients
    error_residuals <- DV - predicted_values
  
  ## Simple plot of predicted values against the residuals: simply use the plot() function with our newly calculated
  ## predicted_values and error_residuals
    simple_plot <- plot(predicted_values, error_residuals)
  
  ## Now before we return all this info, we put in a list all the information we want to give when the function is called
  ## This includes the list of descriptive statistics, the model coefficients and the simple plot
  list_of_information <- list(descriptive_statistics, model_coefficients, simple_plot)
  ## Furthermore, we give the different parts of the list apt names
  names(list_of_information) <- c("Descriptive statistics", "Regression coefficients", 
                                  "Simple plot of predicted values against the residuals")
  ## Finally, the function should return the created list that holds all information
  return(list_of_information)
}

## Question 4:

## Running the function on the tortoise dataset with the correct specifications, the coefficients and plot match!

## The function can be specified in two ways for this dataset:
Regression_Function(data = gala_data, DV = gala_data[, 1], IV = gala_data[, 2:4])
Regression_Function(data = gala_data[1:4])
