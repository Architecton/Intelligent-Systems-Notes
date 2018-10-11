# We are going to use the GA package
# Make sure that the package is installed.

library(GA)

#
#
# EXAMPLE 1: One-dimensional function optimization
#
#

# The asymmetric double claw is difficult to maximize because there are many local solutions.
# Standard derivative-based optimizers would simply climb up the hill closest to the starting value.

f <- function(x) 
{
	y <- (0.46 * (dnorm(x, -1, 2/3) + dnorm(x, 1, 2/3)) +
	(1/300) * (dnorm(x, -0.5, 0.01) + dnorm(x, -1, 0.01) +
	dnorm(x, -1.5, 0.01)) +
	(7/300) * (dnorm(x, 0.5, 0.07) + dnorm(x, 1, 0.07) +
	dnorm(x, 1.5, 0.07)))
	
	y
}

# Plot the double claw
curve(f, from = -3, to = 3, n = 1000)

# For the maximization of this function we may use f directly as the fitness function
GA <- ga(type = "real-valued", fitness = f, lower = -3, upper = 3) # Type of genevector, fitness function, lower bound, upper bound.
# We get an object as a result.


# The object returned can be plotted
plot(GA)
summary(GA)
Sys.sleep(10)

# plot the solution - GA@solution contains the x value.
curve(f, from = -3, to = 3, n = 1000)
points(GA@solution, f(GA@solution), col="red") # Mark maximum on plot.
Sys.sleep(10)


# The evolution of the population units and the corresponding functions values at each 
# generation can be obtained by defining a new monitor function and then passing this 
# function as an optional argument to ga
myMonitor <- function(obj) 
{
	curve(f, obj@lower, obj@upper, n = 1000, main = paste("iteration =", obj@iter)) 	# Plot curve.
	points(obj@population, obj@fitness, pch = 20, col = 2) 								# pch refers to the symbol used to mark the points.
	rug(obj@population, col = 2) 														# Plot agent values on x axis.
	Sys.sleep(1) 																		# Wait before starting next iteration.
}

# GA <- ga(type = "real-valued", fitness = f, lower = -3, upper = 3, monitor = myMonitor)
