#
#
# EXAMPLE 5: Traveling salesman problem
#
#

library(GA)

# Given a list of cities and the distances between each pair of cities, what is the shortest possible route that visits 
# each city exactly once and returns to the origin city?

data("eurodist", package = "datasets")
D <- as.matrix(eurodist)

# Rows can be accessed as D['Stockholm', ]

# An individual round tour is represented as a permutation of a default numbering of the cities defining the current order 
# in which the cities are to be visited

# Calculation of the tour length
tour_length <- function(tour) 
{
	N <- length(tour)
	print(tour)
	dist <- 0
	for (i in 2:N) 
		dist <- dist + D[tour[i-1],tour[i]] # Add distance from previous destination to next destination to accumulator.
	
	dist <- dist + D[tour[N],tour[1]] # Add distance back to home city.
	dist
}

# The fitness function to be maximized is defined as the reciprocal of the tour length.
tspFitness <- function(tour) 
{
	1/tour_length(tour)
}

# Run genetic algorithm.
GA5 <- ga(type = "permutation", fitness = tspFitness, lower = 1, upper = ncol(D), popSize = 50, maxiter = 5000, run = 500, pmutation = 0.2)

summary(GA5)

# Reconstruct the solution found 
tour <- GA5@solution[1, ]
tour <- c(tour, tour[1])

# Get length of tour.
tour_length(tour)

# get names of cities in tour.
colnames(D)[tour]
