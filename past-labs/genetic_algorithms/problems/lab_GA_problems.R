##########################################################################################################################
#
# PROBLEMS
#
##########################################################################################################################
#
# - Use GA search (using the ga() function in the GA package) to find the minimum of the real-valued function 
#   f(x) = abs(x) + cos(x). Restrict the search interval to [-20, 20]. Carefully define the fitness function, 
#   since the ga() can only maximize it! 
#
##########################################################################################################################
#
# - Use GA search to find the minimum of the real-valued two-dimensional function 
#   f(x1, x2) = 20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2)), where x1 and x2 are from the interval [-5.12, 5.12].
#	
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
#
# - Use a binary GA to select (sub)optimal attribute subset for a linear model:
#
#   train.data <- read.table("AlgaeLearn.txt", header = T)
#   test.data <- read.table("AlgaeTest.txt", header = T)
#   lm.model <- lm(a1 ~., train.data)
#
##########################################################################################################################
