#
# Example 4: ESTABLISHING A TIMETABLE
#

# A small football club has a youth team and a senior team. The player 
# training program has seven components: stamina training, strength training, 
# technique, tactics, psychological preparation, teamwork, and regeneration. 
# Due to lack of funds, for each component, a single staff member is responsible 
# for both the youth and the senior team, with the exceptions of tactics and 
# stamina training, where two staff members are assigned, one to each team.
# 
# The weekly training regime is summarized in the following table:
# 
#+----------+---------------------+-----------------+-----------------+
#| Coach    | Component           | Senior team     | Youth team      |
#+----------+---------------------+-----------------+-----------------+
#| Anze     | Strength training   | 1 time a week   | 1 time a week   |
#| Bojan    | Technique           | 3 times a week  | 3 times a week  |
#| Ciril    | Regeneration        | 2 times a week  | 2 times a week  |
#| Dusan    | Stamina training    | doesn't conduct | 4 times a week  |
#| Erik     | Stamina training    | 4 times a week  | doesn't conduct |
#| Filip    | Teamwork            | 3 times a week  | 3 times a week  |
#| Gasper   | Psychological prep. | 1 time a week   | 1 time a week   |
#| Hugo     | Tactics             | 1 time a week   | doesn't conduct |
#| Iztok    | Tactics             | doesn't conduct | 1 time a week   |
#+----------+---------------------+-----------------+-----------------+
#
# Training is performed from Monday to Friday in four different time slots: 
#    8:00 - 10:00, 10:15 - 12:15, 14:00 - 16:00, and 16:15 - 18:15.
# 
# Constraints:
#
# - each time slot can hold only one component for the youth team and one component 
#   for the senior team (the youth and senior teams train separately, so a single
#   staff member can only train one of the two teams in a single time slot). 
# 
# - a team is not allowed to train the same component 2 or more times within one day.
# 
# - the main purpose of the Tactics training component is to prepare the team for 
#   the upcoming match. Matches are usually played during the weekend, so Tactics 
#   training should be scheduled for Thursday in the 16:15 - 18:15 time slot.
# 
# - after a match, the players need to rest. Therefore, there is no training in 
#   the Monday 8:00 - 10:00 time slot.
#
# - the stamina training coach Dusan is not available on Monday mornings 
#   (8:00 - 10:00 in 10:15 - 12:15 time slots)
#
# - there can be no Technique training on Wednesdays, because coach Bojan is 
#   not available.
#
#
# Produce a training schedule that takes into account these two and all of 
# the above restrictions!
#
#

# VARIABLES
#
# senior     - number of sessions per component for the senior team
# youth      - number of sessions per component for the youth team
# staff      - coaching staff
# slots      - possible slots
#

library(GA)

senior     = c(1, 3, 2, 0, 4, 3, 1, 1, 0)
youth      = c(1, 3, 2, 4, 0, 3, 1, 0, 1)
staff      = c(1, 2, 3, 4, 4, 5, 6, 7, 7)
slots      = 4*5

valueBin <- function(timetable)
{
	# organize data into a multi-dimensional array
	# days, time slots, staff, teams

	t <- array(as.integer(timetable), c(5,4,9,2))
	
	violations <- 0

	# check all the conditions
	
	# check the number of sessions per component
	for (i in 1:9)
	{
		violations <- violations + abs(sum(t[,,i,1]) - senior[i])
		violations <- violations + abs(sum(t[,,i,2]) - youth[i])
	}	

	# it is not allowed to train the same component 2 or more times within one day
	for (i in 1:9)
	{
		violations <- violations + sum(apply(t[,,i,1], 1, sum) > 1)
		violations <- violations + sum(apply(t[,,i,2], 1, sum) > 1)
	}

	# a single staff member can only train one of the two teams in a single time slot
	violations <- violations + sum(t[,,,1] == t[,,,2] & t[,,,1] != 0)

	# each time slot can hold only one component for the youth team and one component
	# for the senior team
	for (i in 1:5)
		for (j in 1:4)
		{
			violations <- violations + max(0, sum(t[i,j,,1]) - 1)
			violations <- violations + max(0, sum(t[i,j,,2]) - 1)
		}


	# Tactics training should be scheduled for Thursday in the 16:15 - 18:15 time slot
	violations <- violations + (t[4,3,8,1] != 1)
	violations <- violations + (t[4,3,9,2] != 1)

	# there is no training in the Monday 8:00 - 10:00 time slot
	violations <- violations + sum(t[1,1,,])

	# the stamina training coach Dusan is not available on Monday mornings
	violations <- violations + sum(t[1,1:2,4,] == 1)
	
	# there can be no Technique training on Wednesdays
	violations <- violations + sum(t[3,,2,] == 1)

	
	-violations	
}

myInitPopulation <- function(object)
{
	p <- gabin_Population(object)

	for (i in 1:nrow(p))
	{
		t <- array(p[i,], c(5,4,9,2))
		
		# Tactics training on Thursdays in the 16:15 - 18:15 time slot
		t[4,3,8,1]=1
 		t[4,3,9,2]=1
		
		# there is no training in the Monday 8:00 - 10:00 time slot
		t[1,1,,] = 0

		# there is no Stamina training on Monday mornings
		t[1,1:2,4,] = 0
		
		# there is no Technique training on Wednesdays
		t[3,,2,] = 0

		
		p[i,] <- as.vector(t)
	}

	p
}

GA4 <- ga(type = "binary", fitness = valueBin, nBits = 4*5*9*2,
 popSize = 500, maxiter = 1000, run = 200, population = myInitPopulation)


timetable <- function(solution)
{
	t <- array(solution, c(5,4,9,2))

	result <- array(0, c(5, 4, 2))

	for (i in 1:5)
		for (j in 1:4)
		{
			n <- which(t[i,j,,1] == 1)
			if (length(n) > 0)
				result[i,j,1] = n

			n <- which(t[i,j,,2] == 1)
			if (length(n) > 0)
				result[i,j,2] = n
		}

	result
}

t <- timetable(GA4@solution[1,])
t
