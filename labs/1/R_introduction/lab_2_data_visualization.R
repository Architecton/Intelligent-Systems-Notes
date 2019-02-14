##############################################################################
#
# DATA VISUALIZATION
#
##############################################################################

# Please download data files "movies.txt" and "players.txt" into a local directory
# then set that directory as the current working directory of R.
# You can achive this using the "setwd" command or by selecting "File -> Change dir..."

# for example:
setwd("/home/jernej/Desktop/Repositories/classes/third-year/intelligent-systems-class/labs/1/R_introduction")


# To read data from a text file, use the "read.table" command.
# The parameter header=TRUE indicates that the file to be read includes a first line with the column names
md <- read.table(file="movies.txt", sep=",", header=TRUE)

# To get more information on any specific named function, type "?" followed by the function name
?read.table

# Useful functions
head(md) 			# Return the first part of the table.
tail(md)			# Return the last part of the table.
summary(md) 		# 'summary' is a generic function used to producte result summaries of the results of various model fitting functions.
str(md) 			# Compactly display the internal structure of an R object.
names(md) 			# Get column names.


# We will transform binary attributes into nominal variables with a fixed number of possible values (factors)
md$Action <- as.factor(md$Action) 					# Possible values are 0 and 1 (levels: 0 1)
md$Animation <- as.factor(md$Animation)

# The remaining columns will be transformed using the for loop (Alternative way of doing the same thing)
for (i in 20:24)
	md[,i] <- as.factor(md[,i])

#
# Type conversion functions:
#
# as.numeric
# as.integer
# as.character
# as.logical
# as.factor
# as.ordered
#
# values that cannot be converted to the specified type will be converted to a NA value
#

# Binary attributes are now represented as factors
summary(md)

# Accessing data frame elements...
md[30,]
md[30,3]
md[30,"length"]
md[,3]
md$length

# Useful data visualization functions
plot(md$length) 			
hist(md$length) 			# md$length returns a vector containing all the movie lengths.
plot(density(md$length)) 	# Plot estimated PDF.
boxplot(md$length) 			# Make box plot.
barplot(table(md$Drama)) 	# Plot number of dramas against other genres.
pie(table(md$mpaa)) 		# make a pie graph of the Motion Picture Association of America's ratings


###############################################################################
#
# EXAMPLE 1: What is the proportion of comedies to other genres in our data set?
#
###############################################################################

# the table() command gives the frequency of values in the vector
table(md$Comedy)

# the proportion of comedies can be plotted
barplot(table(md$Comedy))
pie(table(md$Comedy))


#  it is important to always label graphs ...
tab <- table(md$Comedy)
names(tab) <- c("Other genres", "Comedies")
tab

sum(tab)

barplot(tab, ylab="Number of titles", main="Proportion of comedies to other genres")
barplot(tab / sum(tab) * 100, ylab="Percentage of titles", main="The proportion of comedies to other genres")

pie(tab, main = "Proportion of comedies to other genres")


###############################################################################
#
# EXAMPLE 2: How are ratings distributed for comedies?
#
###############################################################################

# Plot the rating distribution for comedies
# Select rows representing comedies and select the "rating" column
hist(md[md$Comedy == "1", "rating"], xlab="Rating", ylab="Frequency", main="Histogram of ratings for comedies")

# Box plots provide a visual display of the range and potential skewness of the data
boxplot(md[md$Comedy == "1", "rating"], ylab="Rating", main="Boxplot of ratings for comedies")
# note: [row, column]


# Find quantiles of the comedies' ratings
quantile(md$rating[md$Comedy == 1])
# note: select rating column from rows where Comedy == 1

###############################################################################
# 
# EXAMPLE 3: Are comedies on average better rated than non-comedies?
# MARK
###############################################################################

# Select comedies
comedy <- md$Comedy == "1"

# Calculate the mean rating value for comedies and non-comedies
mean(md[comedy,"rating"]) 		# Compute mean of ratings of comedies.
mean(md[!comedy,"rating"]) 		# Compute mean of ratings of non-comedies.

# Comedies have, on average, higher ratings than non-comedies

# Side-by-side boxplots of ratings grouped by values of the attribute "Comedy" 
boxplot(rating ~ Comedy, data=md)
boxplot(rating ~ Comedy, data=md, names=c("Other genres", "Comedies"), ylab="Rating", main="Comparison of ratings between comedies and non-comedies")


###############################################################################
#
# EXAMPLE 4: What is the proportion of comedies (per year) from 1990 onwards?
# MARK
###############################################################################


# Selection mask (year greater or equal to 1990)
sel <- md$year >= 1990

# the table() command can be used to get a two-way contigency table 
table(md$Comedy[sel], md$year[sel]) # MARK

# Make a vector of years column where year is greater or equal to 1990.
table(md$year[sel])

tabcomedy <- table(md$Comedy[sel], md$year[sel])
tabyear <- table(md$year[sel]) # Mark
tabcomedy[2,]/tabyear

ratio <- tabcomedy[2,]/tabyear
barplot(ratio, xlab="Year", ylab="Relative frequency", main="Proportion of comedies")

plot(x=names(ratio), y=as.vector(ratio), type="l", xlab="Year", ylab="Relative frequency", main="Proportion of comedies, 1990-2005")

###############################################################################
# 
# EXAMPLE 5: Are there more movies above or below the average rating?
#
###############################################################################

# the average rating
mean(md$rating)

# how many movies are above the average rating?
tab <- table(md$rating > mean(md$rating))
tab

names(tab) <- c("below", "above")
barplot(tab, ylab="Number of titles", main="Proportion of movies above and below the average rating")
pie(tab, main="Proportion of movies above and below the average rating")


# Box plots provide a summarization of the variable distribution
boxplot(md$rating, ylab="Rating", main="Boxplot of movie ratings")

# The horizontal line inside the box represents the median rating value

# Let's plot the mean value...
abline(h=mean(md$rating))

# The mean differs from the median so the distribution is skewed.
# We can conclude that there are more cases above the mean value. 


###############################################################################
#
# EXAMPLE 6: Do movies with bigger budgets get higher ratings?
# MARK
###############################################################################

# there are missing values in the budget attribute
summary(md$budget)

is.na(md$budget)
table(is.na(md$budget))
which(is.na(md$budget))

# select complete observations only
sel <- is.na(md$budget)
mdsub <- md[!sel,]

nrow(mdsub) 			# Number of movies with specified budgets (number of rows in data frame)
summary(mdsub$budget)


plot(mdsub$budget, mdsub$rating, xlab="Budget in $", ylab="Rating", main="Movie rating vs budget")

# Plotted points are mostly located in the upper left part of the diagram, 
# which means that a higher budget usually leads to a higher rating
 
# Utilization of the budget in terms of rating  
ratio <- mdsub$budget/mdsub$rating
hist(ratio)

# Which movie has the worst budget utilization?
mdsub[which.max(ratio),]

 
# Let's discretize these budgets to:
# low (less than 1M), mid (between 1M and 50M) and big (more than 50M)
# Return a vector of labels for each movie.
disbudget <- cut(mdsub$budget, c(0, 1000000, 50000000, 500000000), labels=c("low", "mid", "big"))
barplot(table(disbudget)/length(disbudget), xlab="Budget", ylab="Relative frequency", main="Proportion of movies vs budget")

# Side-by-side boxplots of ratings grouped by budget values
boxplot(mdsub$rating ~ disbudget, xlab="Budget", ylab="Rating", main="Boxplot of movie rating vs budget")


###############################################################################
#
# EXAMPLE 7: 
# What is the cumulative movie budget for each year from 1990 to 2000?
# What is the average movie budget for each year from 1990 to 2000?
# (consider only those movies for which information on the budget is available!)
#
# MARK
###############################################################################

# Select the movies that contain information on their budgets
sel <- !is.na(md$budget) & md$year >= 1990 & md$year <= 2000

# We can calculate cumulative budget for each year using the "aggregate" function

# Data overflow problem!
aggregate(budget ~ year, data = md[sel,], sum)

# The budget values are represented as integers
typeof(md$budget)

# In order to avoid the overflow problem we have to convert 
# the budget values into a double-precision representation (using the as.double() command)
aggregate(as.double(budget) ~ year, data = md[sel,], sum)

sum.budget <- aggregate(as.double(budget) ~ year, data = md[sel,], sum) 
plot(sum.budget, type="l", xlab="Year", ylab="Cumulative budget in $", main="Cumulative movie budget per year")

avg.budget <- aggregate(as.double(budget) ~ year, data = md[sel,], mean)
plot(avg.budget, type="l", xlab="Year", ylab="Average budget in $", main="Average movie budget per year")


##############################################################################
#
# EXAMPLE 8: (players dataset)
# What is the average height for each season in the period from 1970 to 2000?
#
##############################################################################

# Load the Players dataset
players <- read.table("players.txt", sep=",", header = T)
summary(players)

# Create an empty vector
h <- vector()

# Use a for loop to go through each year in the period from 1970 to 2000
for (y in 1970:2000)
{
	# Select active players in that year
	sel <- players$firstseason <= y & players$lastseason >= y
	
	# Append the resulting vector with the mean height for the current year
	h <- c(h, mean(players$height[sel]))
}

# plot the resulting vector (use type="l" for lines)
plot(1970:2000, h, type="l", xlab="Year", ylab="Height in cm", main="Average height in NBA")