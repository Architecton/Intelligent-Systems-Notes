##########################################################################################################################
#
# - Use GA search to find the minimum of the real-valued two-dimensional function 
#   f(x1, x2) = 20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2)), where x1 and x2 are from the interval [-5.12, 5.12].
#	
##########################################################################################################################
library(GA)
library(plotly)


# Function that we are trying to minimize.
f <- function(x1, x2) {
	return (20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2)))
}

# Try to find minimm using genetic algorithm.
GA <- ga(type = "real-valued", fitness = function(params){-f(params[1], params[2])}, lower = c(-5.12, -5.12), 
	upper = c(5.12, 5.12), popSize = 200, run = 50, maxiter = 10000, names = c("x1", "x2"))

# Store found minimum in new variable.
minimum = as.vector(GA@solution)

# Plot results as a countor plot with marked found minimum.
x1 <- seq(-5.12, 5.12, length.out=100)
x2 <- seq(-5.12, 5.12, length.out=100)
z <- outer(x1, x2, f)
filled.contour(x1, x2, z, plot.axes={points(minimum[1], minimum[2])})