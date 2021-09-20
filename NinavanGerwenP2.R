## Nina van Gerwen, 1860852

## Exercises 1: Importing data

# 1.1: To import data from spss:

library(foreign)      # First load the foreign package (install first if necessary)

setwd("C:/Users/nina-/Documents/Master")  # set WD to the place that contains the spss file

SPSS <- read.spss(file = "SPSSData.sav", to.data.frame = TRUE)

# create a dataframe from the file SPSSDATA.sav and make sure it is imported as data frame with to.data.frame

is.data.frame(SPSS)

# Check whether the file is actually a data frame

# 1.2: 

importExceldata <- read.csv(file = "ExcelData.csv",
                            header = TRUE, sep = ";", row.names = NULL)

#To import a file from excel, first change the excel file in excel to a csv file in Export.
# Afterwards, specify the separation as a semi-colon as I have a dutch computer and specify that row names
# should be NULL to work around the error of rows having the same value

## Exercise 2: Logical Operators

# 3.1: I believe the results are not equal because there is a slight difference between the rounded value
# of sqrt(2) * sqrt(2) (which is a floating point number) and the integer 2
# which is enough for a logical operator to give the answer FALSE


x <- sqrt(2)

# R says the sqrt(2) is 1.4142135623731, which to the power of 2 gives 2,000000000000014.
# So an indication of the difference between sqrt(2) * sqrt(2) and 2 might be 0.000000000000014.


# 3.2: To fix the issue where sqrt(2) * sqrt(2) is not equal to (==) 2 in R, you can use round
z <- sqrt(2) * sqrt(2)      # create a variable z, which is twice the square root of 2

round(z, digits = 0) == 2   # test whether the variable z, rounded up to 0 digits (AKA an integer) is equal to 2

# 3.3: It's simply an important issue to be aware of, especially for example 
#in if statements that rely on logical operators
# because if not properly rounded upwards/downwards, the if statement might always be false and the program
# at hand wouldn't operate properly

## Exercise 3: Graphics

#3.1:
data("USArrests")

#3.2:
?USArrests

# The USArrests dataset has a total of 4 variables, which are given for every state in the USA
# The four variables are respectively: the amount of murder, assault and rape  arrests per 100.000 residents
# Finally the dataset contains the percentage of urban population

#3.3:
summary(USArrests)

# The summary() function provides information about the range (min-max), first and third quartile, median and mean
# value of each variable

#3.4: 

pairs(USArrests)

# Although I can't conclude anything about the relations between the variables, a visual inspection of the
# pairs of variables does show that all the different arrest variables (murder, assault, rape)
# seem to have a positive linear relationship. None of the variables seem to have a relation
# with the percentage of urban population however.

#3.5: Copy-pasted

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
  
pairs(USArrests, diag.panel = panel.hist, upper.panel = panel.smooth, lower.panel = panel.cor)

#3.6: Copy-pasted and run

library(lattice)

splom(~USArrests)

#3.7: Copy-pasted and run

splom(~USArrests | state.region)

## Exercise 4: More graphics


