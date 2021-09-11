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
dim(Animals)      ## the dataset has 28 rows and 2 columns, the rows represent different animals and 
                  ## the columns represent body (kg) and brain (g) weight
print(Animals)

## Question 3
