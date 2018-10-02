###############################################################################
#
# INTRODUCTION TO R
#
###############################################################################

# calculator
(50 + 1.45)/12.5


# assignment operators
x = 945 					# discouraged
y <- sin(0.47)^2 * sqrt(5) 	# encouraged
y^2 -> z


# to inspect the value of a variable simply type its name (similar to MATLAB or Python)
x
y
z


# listing and deleting objects
ls()
rm(y)
rm(x,z)

# remove (almost) everything in the working environment
rm(list=ls())


#
# Vectors (the most basic data objects in R)
#

# creating vectors
v <- c(14,7,23.5,76.2) 						# c is a function that combines objects into a vector
v

# generating a regular sequence of numbers
v <- 1:10 									# similar to MATLAB
v

v <- seq(from=5, to=10, by=2) 				# Function that returns a sequece
v

w <- rep(v, times = 2) 						# Repeat vector v 2 times
w


# scalars are vectors with a single element
w <- 45.0

# vectors can be created using other vectors (similar to MATLAB)
z <- c(v, 2.5, w)
z


#
# Useful functions
#

v <- c(8, 4, 2, 3, 6, 9, 1)

length(v)
max(v)
min(v)

which.min(v) 	# At which index is the minimum element?
which.max(v)	# At which index is the maximum element?

sum(v)
mean(v)
sd(v)
rev(v)
sort(v)

sort(v, decreasing=T)

order(v)		# Get indices of elements in sorted vector.


# types of vectors
mode(v)

# logical vector - has logical constants as elements 
b <- c(TRUE, FALSE, F, T) 				# F and T are aliases for TRUE and FALSE

b
mode(b)

x <- 5 > 3
x
mode(x)


# string vector - has strings as elements
s <- c("character", "logical", "numeric", "complex")
mode(s) # type of file stored in vector


# type coercion (all elements must be of the same type)
x <- c(F, T, 34.56, 'aaa')
x


#
# Vectorization
#

# vector arithmetic (operations are performed element-wise)
v1 <- c(10,20,30,40)
v2 <- 1:4
v1 + v2
v1 * v2 	# Hadamard/Shur product


# functions operate directly on each element of a vector
v1^2
sqrt(v1)
exp(v1)
log2(v1)

# the recycling rule (if lengths are different the elements of the shorter vector are repeated)
v1 * 10
v1 + 1
v1 + c(100, 200) # Add 100 to first element in v1, 200 to second element in v1, 100 to third element in v1 and so on.



#
# Indexing
#

x <- c(-10,20,-30,40,-50,60,-70,80)
x


# individual elements can be addressed using an integer index vector
# (indexing starts with 1)
x[3]
x[c(1,4,5)]
x[1:3]
x[] 			# Whole vector


# negative integer indices address all elements but those stated
x[-1]
x[-c(4,6)]
x[-(1:3)]


# vector elements can be addressed using logical vectors
# (elements corresponding to constants TRUE are selected)

# logical vector
x > 0

# logical vector indexing
x[x>0]						# Positive elements
x[x <= -20 | x > 50]		# Get vector of elements that satisfy predicate.
x[x > 40 & x < 100]

# equality operator is ==
# inequality operator is !=

# the which() function returns indices corresponding to constants TRUE
which(x > 0)	# Returns indices of elements that are greater than 0.

# character string index vector
point <- c(4.7, 3.6, 2.5) 			# "Records/dictionaries"
names(point) <- c('x', 'y', 'z')
point

point['x'] 							# Accessing element by name/key
point[c('x','z')]

# empty indices
point[] <- 0 						# Set all indices to 0.
point

# not the same as
point <- 0
point


#
# Vector editing
#

x <- c("a", "b", "c", "d")

# replacing an element
x[2] <- "BBBBB"
x

# Replacing a subvector
x[c(1,3)] <- c("AAAAA", "CCCCC")
x

# adding new element (append)
x[length(x)+1] = "EEEEE"
x

# what happens if we do not define all elements in the vector?
x[10] <- "FFFFF" 		# Intermediate values are filled with NA
x

# which elements are not defined
is.na(x) 	# Returns a vector of boolean values which is TRUE where there are NAs in the original vector.


# removing elements
x <- x[-c(1,3)]			# All elements except the first and third.
x

x <- c(x[2],x[3]) 		# Only the second and third element.
x




#
# Factors
#

gender <- c("f","m","m","m","f","m","f")
gender

# factors are useful when modelling nominal variables
gender <- factor(gender)
gender

# argument "levels" defines all possible elements' values
dir <- factor(c('left','left','up'), levels = c('left','right','up','down')) 	# Useful when all possible values not guaranteed to be present.
dir

# all possible elements' values
levels(dir) 

# if no match is found
dir[1] <- "diagonal" # Adds NA
dir

# valid assignment
dir[1] <- "down" 	# adds "down" to vector
dir

# frequency tables for factors 
table(gender)	# Count how many times each value appears.
table(dir)



#
# Lists (an ordered collection of objects - components)
#

# creating a list
student <- list(id=12345,name="Marko",marks=c(10,9,10,9,8,10)) 	# A "record"
student

# extracting elements of a list (using named components)
student$id 				# projection
student$name
student$marks

# extracting elements of a list (using indexing)
student[[1]]
student[[2]]
student[[3]]

# extending lists
student$parents <- c("Ana", "Tomaz")  # Add another field.
student


#
# Data frames
#

# R: dataframe is a quasi-builtin data type in R.
# Technically, it is not a primitive; the language definition mentions "Data frame objects" only in passing:
# "A data frame is a list of vectors, factors, and/or matrices all having the same length (number of rows in the case of matrices).
# In addition, a data frame generally has a names attribute labeling the variables and a row.names attribute for labeling the cases."
# Vectors, lists, and "factors" are primitive; matrices and dataframes are not. But in practice the dataframe is central to R.
# A web search will produce many tutorials on working with dataframes, e.g.

 
# creating a data frame
height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)
gender <- factor(c("f","m","m","m","f","m","f","f","m","f"))
student <- c(T, T, F, F, T, T, F, F, F, T)

df <- data.frame(gender, height, weight, student)
df

# some important functions
summary(df) 					# Get summary of data.
names(df) 						# Get names of vectors in data frame.
nrow(df) 						# Get number of rows in data frame.
ncol(df)						# Get number of columns in data frame.

# accessing elements of data frames
df[5,]			# 5th row
df[1:5,] 		# first 5 rows
df[,1] 			# first vector in data frame
df[,c(1,3,4)] 	# first, third and fourth vector in data frame
df[1,3] 		# first row second column
df[1,-3] 		# first row and all except the third column

df$height 		# select vectors by name

df[df$height < 180,] 	# Select every column of rows where height is less than 180
df[df$gender == "m",]	# Select every column of rows where value of gender vector is "m"

# adding columns to a data frame
df <- cbind(df, age = c(20, 21, 30, 25, 27, 19, 24, 27, 28, 24))
df

# another way to add columns to a data frame
df$name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")
df

summary(df)