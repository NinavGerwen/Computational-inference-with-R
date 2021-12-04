####################
# Practical 12 RECAP
# 28 03 2016
####################


#Cross Validation
#################

require(faraway)
data(gala, package="faraway")
summary(gala)
names(gala)

#dependent variable is Species
y <- gala$Species       

#predictors that we'll use, binded by columns, giving them names:
X <- cbind(Endemics=gala$Endemics,Elevation=gala$Elevation,Adjacent=gala$Adjacent)

#model specification:
Model1 <- lm(y ~ X)
summary(Model1)

  # the model significantly predicts the number of species (F(3,26)=151.1, p<.001). The model
  # explains at least 93.95% of the variation (R2_{adjusted}=.9395, R2=.9458), which is a lot.

#MSE, SIGMA2_{e}= E(y - y.hat)^2:
MSE <- mean((y - Model1$fitted.values)^2)       #y.hat is given by the fitted.values
MSE


###############################################################################################
#Make a function that performs a 3-fold cross-validation on the data and report the mean
#squared error of prediction (MSEP = d2), averaged over the 3 folds. What do you
#conclude if you compare this value with the value for MSE in the previous question?
###############################################################################################

# Definition of a function that performs CV K times
CrossValidation <- function(X, y, K){
  #calculate the MSE for the sample:
  y.hat <- lm(y ~ X)$fitted.values
  MSE <- mean((y - y.hat)^2)
  #create an empty vector to later store the prediction error (MSEP) for each fold:
  MSEP <- matrix(NA, 1, K)
  
  # create k cross-validation folds (k test sets with n/k observations,
  # k training sets with n-(n/k) observations)
  # Many approaches will work to do this. Here is one of them:
  # First create a random vector of length = the number of observations (Nrow(X))
  # that contains for each observation the number of the 'fold' that it belongs to  
  # I start by randomly permuting the indices of the subjects, by using sample(nrow(X)).
  # Then the random fold number is obtained by using the modulus operator ('%%'), 
  # i.e. the remainder after integer division with K (=the number of folds). 
  # because the remainder will range from 0 to K-1, we have to add 1 to the result.
  foldindices <- sample(nrow(X)) %% K+1
  
  for (k in 1:K){
    # using these indices we create a training set and a test set for each value of K:
    y.train <- y[k!=foldindices]      #the training set gets all values for which the foldindex
    X.train <- X[k!=foldindices, ]    #does not equal k
    
    y.test  <- y[k==foldindices]      #the test sets consists of the values for which the 
    X.test  <- X[k==foldindices, , drop=FALSE]    #foldindex equals k.
        #Note: , drop=FALSE ensures that X remains a matrix, even with LOOCV where a single row is selected here. Otherwise, R would convert it into a vector
    
    #2a. fit the model to the other N-K observations (training set)
    #we then compose a model by fitting a model on the i'th training set and save the
    #coefficients in a vector with a row for each value, so that we can use it later for matrix
    #multiplication:
    model.coeff   <- lm(y.train ~ X.train)$coefficients
    train.coeff   <- matrix(model.coeff, nrow=ncol(X)+1, ncol=1) #ncol(X)+1 because of intercept
    
    #we then get our predicted values for the test set by multiplying it by the coefficients of 
    #the training set. We first add a column of 1's to the beginning of the X test set, so we have
    #space for the intercept, these values are 1 so that we in the end product just get the 
    #coefficients for the intercept found in the training set.
    X.test2 <- cbind(Intercept=rep(1,times=nrow(X.test)), X.test)
    
    #then we find the predicted values (y.hat.test):
    y.hat.test <- X.test2 %*% train.coeff
    
    #2b. calculate the prediction error when predicting the k-th part of the data (test set)
    MSEP[, k]<-apply((y.test-y.hat.test)^2,MARGIN=2,FUN=mean)  #2nd margin is columns 
  }
  
  #compute the cross-validated prediction error:
  CV.predicted.error <- apply(MSEP, MARGIN=1, FUN=mean) #1st margin is rows
  
  #report output:
  
  (output <- list(MSE=MSE, CV.predicted.error=CV.predicted.error, MSEP.per.k.fold=MSEP))
}


cv3 <- CrossValidation(X, y, 3)
cv3

#The MSE based on the entire sample is a lot smaller than the cross-validated predicted error.
#Since the sample size is only 30, this seems quite logical (with small sample sizes the 
#population will be represented less thoroughly because of capitalization on chance).
#When performing the crossvalidation multiple times, it can be seen that the cross-validated
#predicted error is very dependent on how the observations are divided into folds:
#its value varies a lot (but is most of the time quite a bit larger than the MSE of the whole sample).

# do LOOCV:
LOOCV <- CrossValidation(X, y, nrow(X))
LOOCV

