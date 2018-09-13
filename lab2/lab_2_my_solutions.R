#######################################################################################################################
#
# PROBLEMS 
#
#######################################################################################################################
#
# Load the Movies dataset using the command:
#
#	md <- read.table("movies.txt", sep=",", header=TRUE)
#
# Answer the following questions:
#
# 1. Are there more movies shorter than 100 min or longer than (or equal to) 100 minutes?
#   (show your answer numerically and graphically) 
#
# 2. Are there more action comedies or romantic comedies?
#
# 3. Plot a histogram of the ratings for drama movies.
#
# 4. Is the average rating of dramas higher than the average rating of non-dramas?
#   (show your answer numerically and graphically)
#
# 5. Plot the number of animated movies being produced every year for the period 1995-2005.
#
# 6. Is there a clear boundary between short and feature movies (according to their length)?
#
#
#######################################################################################################################
#
# Load the Players dataset using the command:
#
#	players <- read.table("players.txt", sep=",", header = T)
#
# 7. Plot the proportion of players according to playing positions.
#
# 8. Compare career rebounds (the "reb" attribute) with respect to playing position.
#
# 9. Show the distribution of free throw percentages.
#   The percentage is determined by dividing the number of shots made ("ftm") by the total number of shots attempted ("fta").
#
# 10. Compare career 3-pointers made for the players active between 1990 and 2007, with respect to playing position. 
#
# 11. How does the average career length of retired players vary from year to year?
#
#######################################################################################################################

md <- read.table("movies.txt", sep=",", header=TRUE)

######
# 1. #
######

# Getting the answer numerically:
sel_shorter <- md$length < 100 # vector of boolean values.
tab <- table(sel_shorter) # We can see that there are more longer movies than shorter movies.

# Plotting the proportions using a bar plot.
par(mfrow=c(1,2))
barplot(tab, ylab = "Number of titles", main = "Proportion of movies shorter than 100 minutes.")
pie(tab, main = "Proportion of movies shorter than 100 minutes.")

######
# 2. #
######

# Select romantic comedies
sel_romantic_comedy <- md$Comedy == "1" & md$Romance == "1"
# Select action comedies
sel_action_comedy <- md$Comedy == "1" & md$Action == "1"

# Count romantic comedies and action comedies.
num_romantic_comedy <- length(sel_romantic_comedy[sel_romantic_comedy == TRUE])
num_action_comedy <- length(sel_action_comedy[sel_action_comedy == TRUE])

# Compare counts.
if (num_romantic_comedy > num_action_comedy) {
	print("There are more romantic comedies than action comedies.")
} else if (num_romantic_comedy == num_action_comedy) {
	print("There are an equal number of romantic comedies and action comedies.")
} else {
	print("There are more action comedies than romantic comedies.")
}

# Another way to visualize the answer is to select all comedies and then compute the proportions
# of romantic comedies and action comedies.
proportion_romantic <- nrow(md[md$Romance == "1" & md$Comedy == "1",])/nrow(md[md$Comedy == 1, ]) # Get proportions.
proportion_action <- nrow(md[md$Action == "1" & md$Comedy == "1",])/nrow(md[md$Comedy == 1, ])

# Store computed proportions in proportions vector.
proportions <- c(proportion_romantic, proportion_action)
# Give names to vector fields.
names(proportions) <- c("proportion of romantic comedies", "proportion of action comedies")

# Plot proportions.
par(mfrow=c(1,1))
barplot(proportions, ylab = "Proportion", main = "Proportion of Romantic Comedies and Action Comedies")

######
# 3. #
######

drama_ratings <- md$rating[md$Drama == "1"] 														# Get vector of ratings of dramas.
hist(drama_ratings, xlab = "rating", ylab = "frequency", main = "Histogram of Ratings of Dramas")	# Plot histogram of frequencies of ratings.

######
# 4. #
######

# Showing the answer numerically

# Compute average rating of dramas
avg_rating_dramas <- mean(md$rating[md$Drama == "1"])
tab_compare <- table(md$rating[md$Drama == "1"] > avg_rating_dramas) # We can see that there are more dramas rated higher than average drama rating.
names(tab_compare) <- c("lower", "higher")

# We can now plot the table and show the answer graphically.
pie(tab_compare / sum(tab_compare), main = "Proportions of Dramas rated Higher or Lower than Mean Rating of Dramas")

######
# 5. #
######

# Construct a contingency table.

# Get years.
animation_years <- md$year[md$Animation == "1" & md$year >= 1995 & md$year <= 2005]
# Get 

num_animated <- table()