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

                            