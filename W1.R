## Nina van Gerwen, 1860852

## Exercises A: Basic Exercises with R

## Question 1: The right command for getting v1 on the x-axis and v2 on y-axis is A (plot(v1, v2)), however for
## the sake of clarity, you might consider coding plot(x = v1, y = v2)

## Question 2
x <- 1:10   # this code creates a new value x, which is a list (or vector) of integers ranging from 1 to 10
x[-11]      # this gives x, which is a list ranging from 1 to 10 without the 11th element, however because x doesn't have 11 elements
            # it simply gives x
x[0]        # this produces integer(0), which means that it returns an empty vector
x[14]       # this produces an NA, it should give the 14th element of x, however because it doesn't have one, it returns NA

## Question 3
A <- rep(seq(1,5), 5)           # create a sequence from 1 to 5 and repeat this five times to get A
B <- rep(seq(1, 5), each = 5)   # create a sequence from 1 to 5, and repeat each element 5 times

## Question 4
a <- matrix(data = c(2, 5, 0), nrow = 3, ncol = 1)      # create a matrix with the data 2,5,0, 3 rows and 1 column
b <- matrix(data = c(-1, 0, 8), nrow = 3, ncol = 1)     # create a matrix with the data -1,0,8, 3 rows and 1 column
d <- matrix(data = c(12, 15, 10), nrow = 3, ncol = 1)   # I think we get the gist!

## Question 5
M <- matrix(data = cbind(a, b, d), nrow = 3, ncol = 3)  # create a matrix with the data from matrix a,b,d, 3 rows and 3 columns

## Question 6
Array_A <- array(1:18, dim = c(3,2,3))    # create an array with numbers ranging from 1 to 18 with 3 rows, 2 columns and 3 slides
print(Array_A)

# To select the element in the third row, first column and first slide:
Array_A[dim = 3, 1, 1]
# To select the elements in the second column of the second slide:
Array_A[dim = c(1:3), 2, 2]

## Exercise B: Manipulating and displaying data

## Question 1
require(MASS)
attach(Animals)

## Question 2
dim(Animals)      # the dataset has 28 rows and 2 columns, the rows represent different animals and 
                  # the columns represent body (kg) and brain (g) weight
print(Animals)

## Question 3
dataset <- as.data.frame(Animals)     # create a data.frame of the Animals dataset
dataset[3, 1]                         # extract the first variable (= first column) on the third row
dataset[3,]                           # display everything on the third row, disregarding columns
dataset[,2]                           # display everything on the second column, disregarding rows

# What I notice when displaying elements is that when there are only numerical values, R displays them in a vector, but when there are strings 
# (see dataset[3,] ) R displays the row with the format of the dataframe attached

## Question 4
dotchart(log(body), row.names(Animals), xlab = "log(body)")                     # following the code in the assignment

## Question 5
sortbody <- sort.list(body)                                                     # following the code in the assignment
dotchart(log(body[sortbody]), row.names(Animals[sortbody,]), xlab = "log(body")

# The graph displays the natural logarithm of the body weight on the x-axis and the animal type on the y-axis
# and compared to the graph in Question 4, I believe this graph is better as the y-axis is ordered on bodyweight, showing which animals are heavier/lighter

## Question 6
sortbrain <- sort.list(brain)                                                         # creating a new sorted vector of the variable brain
dotchart(log(brain[sortbrain]), row.names(Animals[sortbrain,]), xlab = "log(brain)")  # creating a dotchart of the natural logarithm of sorted brainweight

# Comparing this graph to the graph in Question 5, I notice that the order is similar and pointdistribution also is similar, indicating that
# brainweight and bodyweight might very well be correlated

## Question 7

# To create ordered graphs, first a variable was created that was an ordered/sorted version of the values in the column brainweight.
# Afterwards, a dotchart was created of the variable brainweight, whilst indexing based on the sortbrain, the rownames were then based on the
# animal type (also indexed based on the sortbrain) and the x-axis is named 'log(brain)'

# The difference between sort() and sort.list() is that sort() simply orders the vector from low to high, so if you have values 10 - 4 - 7,
# sort will give 4 - 7 - 10
# sort.list() instead will give the values in the vector a number (starting at 1) based on their value so sort.list on 10 - 4 - 7, would give:
# 3 - 1 - 2, as 4 is the lowest value, 7 is the second lowest value and 10 is the third lowest value

# The reason we used sort.list here is because we simply wanted to sort the animals from lowest to highest whilst accounting for bodyweight on the x-axis.

## Question 8

attach(mammals)

all_animals <- rbind(Animals, mammals)                        # create a new dataset which is the combination of Animals and mammals
duplicated(all_animals)                                       # check which values in the combined dataset are duplicated

final_dataset <- all_animals[!duplicated(all_animals), ]      # create the final dataset, which is the combined dataset which are NOT (!) duplicated

final_dataset <- 
