# We are going to use the GA package
# Make sure that the package is installed.

library(GA)


#
#
# EXAMPLE 2: Model fitting
#
#

# We consider a data on the growth of trees

# The age at which the tree was measured
Age <- c(2.44, 12.44, 22.44, 32.44, 42.44, 52.44, 62.44, 72.44, 82.44, 92.44, 102.44, 112.44)

# The bole volume of the tree
Vol <- c(2.2, 20.0, 93.0, 262.0, 476.0, 705.0, 967.0, 1203.0, 1409.0, 1659.0, 1898.0, 2106.0)

# Plot age vs vol.
plot(Age, Vol)

# An ecological model for the plant size (measured by volume) as a function of age is the Richards curve:
# f(x) = a*(1-exp(-b*x))^c, where a, b, in c are the model parameters

# Let's fit the Richards curve using genetic algorithms - find best parameters a, b and c.

# We first define our model function (argument params represents a vector of the parameters a, b, and c) 
model <- function(params)
{
	params[1] * (1 - exp(-params[2] * Age))^params[3]
}

# We define the fitness function as the sum of squares of the differences between estimated and observed data
myFitness2 <- function(params) 
{
	-sum((Vol - model(params))^2)
}

# The fitness function needs to be maximized with respect to the model's parameters, given the observed data in x and y.
# A blend crossover is used for improving the search over the parameter space: for two parents x1 and x2 (assume x1 < x2) 
# it randomly picks a solution in the range [x1 - k*(x2-x1), x2 + k*(x2-x1)], where k represents a constant between 0 and 1.


# We restrict the search interval for a,b, and c to [1000.0, 5000.0], [0.0, 5.0], and [0.0, 5.0], respectively.

# real valued gene vector, fitness function, lower bound for parameters, upper bound for parameters, population size, crossover mutation, maximum number of iterations,
# run ~ the number of consecutive generations without any improvement in the best fitness value before the GA is stopped, names of parameters.
# (a vector of character strings providing the names of decision variables).
GA2 <- ga(type = "real-valued", fitness = myFitness2, lower = c(1000, 0, 0), upper = c(5000, 5, 5),
 popSize = 500, crossover = gareal_blxCrossover, maxiter = 5000, run = 200, names = c("a", "b", "c"))

#summary(GA2)

# Let's plot our solution.
plot(Age, Vol) 					# Plot measured data.
lines(Age, model(GA2@solution)) # Plot model with optained parameters.

Sys.sleep(5)


# we can use a monitor function to plot the current solution -> visualize the algorithm
myMonitor2 <- function(obj) 
{
	i <- which.max(obj@fitness)
	plot(Age, Vol)
	lines(Age, model(obj@population[i,]), col="red")
	title(paste("iteration =", obj@iter), font.main = 1)
	Sys.sleep(1)
}

GA2 <- ga(type = "real-valued", fitness = myFitness2, lower = c(1000, 0, 0), upper = c(5000, 5, 5),
 popSize = 500, crossover = gareal_blxCrossover, maxiter = 5000, run = 200, names = c("a", "b", "c"), monitor=myMonitor2)
