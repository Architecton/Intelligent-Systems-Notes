## Problem 1 ##

##########################################################################################################################
#
# - Use GA search (using the ga() function in the GA package) to find the minimum of the real-valued function 
#   f(x) = abs(x) + cos(x). Restrict the search interval to [-20, 20]. Carefully define the fitness function, 
#   since the ga() can only maximize it! 
#
##########################################################################################################################
library(GA)

# Function we are trying to minimize.
f <- function(x) {
	return (abs(x) + cos(x))
}

# Minimize using genetic algorithm. Since we can only find the maximum, the fitness function is the negated function we are trying to minimize.
GA <- ga(type = "real-valued", fitness = function(x){-f(x)}, lower = -10, 
	upper = 10, popSize = 200, run = 50, maxiter = 5000, names = c("x"))

# Plot curve and solution.
curve(f, from = -10, to = 10, n = 1000, main = "Minimization of f(x) = |x| + cos(x)")
points(GA@solution, f(GA@solution), col = "red")