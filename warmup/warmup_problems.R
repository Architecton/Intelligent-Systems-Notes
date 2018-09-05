################################################################################
#
# PROBLEMS:
#
# 1.  Construct a vector that contains elements: 1,2,3,...,19,20.
#
#
# 2.  Construct a vector that contains elements: 1,2,3,...,19,20,19,...,3,2,1.
#
#
# 3.  Construct a vector that contains elements: 1,3,5,1,3,5,...,1,3,5 
#     where there are 10 occurrences of element 5.
#
#
# 4.  Calculate the values of sin(x) at 0, 0.1, 0.2, 0.3, ..., 1.0
#
#
# 5.  Suppose we have measured the heights and weights of ten individuals:
#
      # the vector of heights in 'cm'
      height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)

      # the vector of weights in 'kg'
      weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)

#     Calculate the body mass index (bmi) for each individual using the formula:
#     bmi = weight_in_kg / (height_in_m)^2
#
#     HINT: first convert heights from 'cm' to 'm', then use the formula above.  
#
#
# 6.  Consider a vector:
# 
      x <- c(1, -2, 3, -4, 5, -6, 7, -8)
   
#     Edit the vector x as follows. Replace all elements with a negative value 
#     with 0. Multiply the elements with a positive value by 10.
#
#
# 7.  Without using R, determine the result of the following computation:
#
      x <- c(1,2,3)
      x[1]/x[2]^2-1+2*x[3]-x[1+1]

#
#
# 8.  Consider a vector:
#
      x <- 1:200

#     Determine how many elements in the vector are exactly divisible by 11.
#
#     HINT: the integer division operator is %/%
#           the modulus operator is %%              
#
#
# 9.  Consider a data frame:
#
      height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
      weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)
      gender <- factor(c("f","m","m","m","f","m","f","f","m","f"))
      student <- c(T, T, F, F, T, T, F, F, F, T)
      age = c(20, 21, 30, 25, 27, 19, 24, 27, 28, 24)
      name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")
 
      df <- data.frame(name, gender, age, height, weight, student)
 
#     
#     - calculate the average age of persons in our dataset. 
#       (HINT: use the mean() function)
#
#
#     - calculate the average age of students in our dataset.
#
#
#     - how many males and females are in our dataset? 
#       (HINT: use the table() function)
#
#
#     - print persons that are students. 
#
#
#     - print persons who are between 1.8m and 1.9m tall (inclusive). 
#
#
#     - print students who are above average height 
#       (considering all persons in the dataset).
#
#
#     - arrange persons by their age. 
#       (HINT: use the order function)             
#
###############################################################################

