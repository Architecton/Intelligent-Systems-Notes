##########################################################################################################################
#
# - Use a binary GA to select (sub)optimal attribute subset for a linear model:
#
#   train.data <- read.table("AlgaeLearn.txt", header = T)
#   test.data <- read.table("AlgaeTest.txt", header = T)
#   lm.model <- lm(a1 ~., train.data)
#
##########################################################################################################################

train.data <- read.table("AlgaeLearn.txt", header = T)
test.data <- read.table("AlgaeTest.txt", header = T)
lm.model <- lm(a1 ~., train.data)