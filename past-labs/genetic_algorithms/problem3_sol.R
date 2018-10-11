##########################################################################################################################
#
# - We are given the following data:
#
#   Substrate <- c(1.73, 2.06, 2.20, 4.28, 4.44, 5.53, 6.32, 6.68, 7.28, 7.90, 8.80, 9.14, 9.18, 9.40, 9.88)
#   Velocity <- c(12.48, 13.97, 14.59, 21.25, 21.66, 21.97, 25.36, 22.93, 24.81, 25.63, 24.68, 29.04, 28.08, 27.32, 27.77)
#
#   Use GA search to fit the data to the model:
#   Velocity = (M * Substrate) / (K + Substrate), where M and K are the model parameters. Restrict the search interval 
#   for M to [40.0, 50.0] and for K to [3.0, 5.0].
#
##########################################################################################################################
library(GA)

# Data
Substrate <- c(1.73, 2.06, 2.20, 4.28, 4.44, 5.53, 6.32, 6.68, 7.28, 7.90, 8.80, 9.14, 9.18, 9.40, 9.88)
Velocity <- c(12.48, 13.97, 14.59, 21.25, 21.66, 21.97, 25.36, 22.93, 24.81, 25.63, 24.68, 29.04, 28.08, 27.32, 27.77)

# model for the data - velocity is a vunction of M, K and Substrate.
model <- function(params) {
	(params[1] * Substrate) / (params[2] + Substrate)
}

# fitness function - sum of squared errors of our model.
fitness_func <- function(params) {
	-sum((Velocity - model(params))^2)
}

# Use genetic algorithm to try to find parameters that minimize sum of squared errors.
GA <- ga(type = "real-valued", fitness = fitness_func, lower = c(40.0, 3.0), upper = c(50.0, 5.0),
 popSize = 500, crossover = gareal_blxCrossover, maxiter = 5000, run = 200, names = c("M", "K"))

# Plot solution.
plot(x = Substrate, y = Velocity, main = "Computed model")
lines(x = Substrate, y = model(GA@solution), col = 'red')