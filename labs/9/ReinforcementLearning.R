###########################################################################
#
# Value iteration method
#
###########################################################################

#
# T[s,a] - The transition from a state "s" for a given action "a".
#          This encodes a problem in a form that is suitable for 
#          reinforcement learning
# R[s,a] - The immediate reward after transition to state "s" using action "a".
#          We use this matrix to specify the actions that we know are good

# The parameters for the function are the transition matrix and the reward
# matrix
valueIteration <- function(T, R, gamma = 0.9, iterations = 100)
{	
  # initialize starting state values to 0
  # V - The vector of scores for each state
  V <- rep(0, nrow(R))
  # a set of actions to iterate through
  A <- 1:ncol(R)
  # a set of states to iterate through
  S <- 1:nrow(R)
  
  # Q - The vector of scores for each [state, action] pair
  # initialize the matrix Q (states X actions)
  # everything should start at 0
  Q <- matrix(0, nrow(R), ncol(R))
  
  # Iterate trough every state and action multiple times
  for(i in 1:iterations)
  {
    # for each state "s"
    for(s in S)
    {
      # for each action "a"
      for(a in A)
      {
        # Update the score for the action "a" from the state "s".
        # It should be equal to the score in the reward matrix R + 
        # some factor gamma times the score of the state we reached
        # with that action. Gamma is the discount factor and should 
        # be set to a number lower than 1. In a maze-solving problem,
        # this ensures that the further away a room is from the exit,
        # the smaller its score will be.
        Q[s,a] <- R[s,a] + gamma*1*V[T[s,a]]
        # Update the scores for the state. The score is equal to the
        # score of the best possible move we can make from that state.
        V[s] <- max(Q[s,], na.rm=TRUE)
      }
      
    }
  }
  # Return the scores of each state
  V
}

  
###########################################################################
#
# Q-learning
#
###########################################################################

#
# T[s,a] - the transition from a state "s" for a given action "a"
# R[s,a] - the immediate reward after transition out of state "s" for a given action "a"
# F - the set of final states. This is needed to stop the agent.
#

qlearning <- function(T, R, F, gamma = 0.9)
{
  # the number of states
  nstates <- nrow(T)
  nactions <- ncol(T)
  
  # initialize the matrix Q
  Q <- matrix(0, nrow = nstates, ncol = nactions)
  
  alpha <- 1
  
  while (alpha > 0.1)
  {
    # We want an agent that randomly explores the environment
    # randomly select a starting state
    cur.state <- sample(1:nstates, 1)
    
    # move around until we reach one of the final states
    while (!(cur.state %in% F))
    {
      # see which actions are possible in the current state
      possible.actions <- which(!is.na(T[cur.state,]))
      
      # randomly select the next action
      action <- possible.actions[sample(length(possible.actions), 1)]
      
      # the selected action determines the next state
      next.state <- T[cur.state, action]
      
      # update Q for the current state and the selected action
      # This is similar to value iteration, with some changes:
      #     - We no longer use the V vector. Instead this is incorporated
      #       directly into the equation.
      #     - We use alpha to ensure later iterations don't chage the Q
      #       matrix as much as earlier iterations
      #     - We subtract Q[cur.state, action] at the end to ensure moving
      #       from a better state to a worse state is penalized even if 
      #       the worse state still has a good score.
      Q[cur.state, action] <- Q[cur.state, action] + alpha * (R[cur.state, action] + gamma * max(Q[next.state,], na.rm = T) - Q[cur.state, action])
      
      # Execute the move
      cur.state <- next.state
    }
    
    # Lower alpha during every iteration
    alpha <- alpha * 0.999
  }
  
  # Return the Q matrix, which is normalized so that the scores look nicer.
  Q / max(Q)
}



################################################################################################
#
# Solution for Problem 1 (see "Reinforcement learning problems.pdf" on the course web page)
# using q-learning
#
################################################################################################

# the transition matrix
#
# rows represent rooms 
# columns represent actions
# value T[2,3] = 1 means that when in Room 2, the door on the left leads to Room 1 
#

mT <- matrix(NA, nrow = 12, ncol = 4)
mT[1, 1] = 2
mT[1, 2] = 12
mT[2, 3] = 1
mT[2, 4] = 5
mT[3, 1] = 4
mT[3, 4] = 6
mT[4, 1] = 5
mT[4, 3] = 3
mT[4, 4] = 9
mT[5, 2] = 2
mT[5, 3] = 4
mT[5, 4] = 11
mT[6, 2] = 3
mT[6, 4] = 7
mT[7, 1] = 8
mT[7, 2] = 6
mT[8, 1] = 9
mT[8, 3] = 7
mT[9, 2] = 4
mT[9, 3] = 8
mT[10, 1] = 11
mT[11, 2] = 5
mT[11, 3] = 10
mT[11, 4] = 12
mT[12, 1] = 11
mT[12, 4] = 1

mT


# the reward matrix
mR <- matrix(0, nrow = 12, ncol = 4)
mR[1, 2] = 100
mR[11, 4] = 100

mR

# Solving with Q-learning
mQ <- qlearning(mT, mR, 12)

mQ

# write out the optimal strategy
# we get the best move by selecting the column (action) with the best score
# in a row (state)
apply(mQ, 1, function(x){which(x == max(x))})


################################################################################################
#
# Solution for the same problem using the value iteration method
#
################################################################################################


# the reward matrix
# KEEP IN MIND: a reward will be granted after the agent leaves state "s" using action "a"

# The reward matrix needs to be changed slightly so that state 12 is correctly
# identified as the best one
mR <- matrix(0, nrow = 12, ncol = 4)
mR[12, 4] = 100
mR[12, 2] = 100

mR

v <- valueIteration(mT, mR, 0.9, 100)
v

