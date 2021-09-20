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

#4.1: 
# I loaded the Rdata file into my global workspace and then wrote this code, meaning I don't have to use attach() or with()
library(ggplot2)

load("ChildIQ.RData")

summary(ChildData)      # gives numerical information (mean, median, range, etc.) on all variables 
dim(ChildData)          # shows the number of observations and variables
str(ChildData)          # displays the internal structure of the data frame (whether the variables are numerical, integers, etc.)
head(ChildData)         # returns the first part of the data frame, the heading if you will

ChildData$mom.hs <- as.factor(ChildData$mom.hs)   # recode the variable mom.hs as a factor with 2 levels (0 for no, 1 for yes)

#4.2:

ggplot(data = ChildData, aes(x = mom.iq, y = kid.score)) + geom_point()

# call on ggplot to make a plot, with ChildData as the dataset, mother's IQ on x-axis, and kid's IQ on y-axis, state geom_point()
# as we are looking for a scatterplot

#4.3:

plot_a <- ggplot(data = ChildData, aes(x = mom.iq, y = kid.score)) + geom_point() + 
  labs(x = "Mother's IQ Score", y = "Child's IQ Score")

plot_a

# I create a plot with ggplot with ChildData as its dataframe, the right axes and also label both axes

#4.4:
  
plot_b <- ggplot(data = ChildData, aes(x = mom.iq, y = kid.score)) + 
  geom_point(shape = 21, colour = "black", fill = "grey", size = 1, stroke = 1) +
  labs(x = "Mother's IQ Score", y = "Child's IQ Score")

plot_b

# I created the same plot as before, however, now I add to the geom_point argument the shape, colour, fill colour, and size it should use

#4.5:
coef(lm(kid.score ~ mom.iq, data = ChildData)) 

# look up the intercept and slope coefficients of child's IQ dependent on mom's IQ

plot_c <- ggplot(ChildData, aes(mom.iq, kid.score)) + 
  geom_point(shape = 21, colour = "black", fill = "grey", size = 1, stroke = 1) +
  labs(x = "Mother's IQ Score", y = "Child's IQ Score") +
  geom_abline(intercept = 25.7997778, slope = 0.6099746)

plot_c

# I created the same plot as in question 4.4, however, now I added a geom_abline into the function with the intercept 
# and slope gained from the coef() function

#4.6:

plot_final <- ggplot(ChildData, aes(mom.iq, kid.score)) + 
  geom_point(shape = 21, colour = "black", fill = "grey", size = 1, stroke = 1) +
  labs(x = "Mother's IQ Score", y = "Child's IQ Score") +
  geom_abline(intercept = 25.7997778, slope = 0.6099746, colour = "blue", size = 2)

plot_final

# I copied the plot from question 4.5, but I added to the geom_abline argument the new colour and size