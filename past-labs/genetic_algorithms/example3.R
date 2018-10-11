#
#
# EXAMPLE 3: The Knapsack problem
#
#

library(GA)

# The Knapsack problem is defined as follows: given a set of items, each with a mass and a value, determine the subset 
# of items to be included in a collection so that the total weight is less than or equal to a given limit and the total value 
# is as large as possible.

# a vector of the items' values
values <- c(5, 8, 3, 4, 6, 5, 4, 3, 2)

# a vector of the item's weights
weights <- c(1, 3, 2, 4, 2, 1, 3, 4, 5)

# the knapsack capacity
Capacity <- 10

# A binary GA can be used to solve the knapsack problem. The solution to this problem is a binary string equal to the number 
# of items where the ith bit is 1 if the ith item is in the subset and 0 otherwise. The fitness function should penalize 
# unfeasible solutions.
knapsack <- function(x) 
{
	f <- sum(x * values)
	w <- sum(x * weights)

	if (w > Capacity)
		f <- Capacity - w

	f	
}

GA3 <- ga(type = "binary", fitness = knapsack, nBits = length(weights), maxiter = 1000, run = 200, popSize = 100)

summary(GA3)