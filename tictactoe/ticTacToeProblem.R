# PROBLEM INSTRUCTIONS
#
# Board p x p, p = 3
# Two players, agent vs robot
# Each player can set its mark on a cell
# Marks: "X" for player 1, "O" for player 2
# Either player can start
# When one player aligns p marks, (s)he wins and game ends.

# imports
library(tidyverse)
source('tictactoe/ticTacToe_foo.R')
source('tictactoe/ticTacToe_foo_to_fill.R')

# run bunch of simulations
gameSimulations = list()
N <- 10000
pb <- txtProgressBar(min = 0, max = N)
for(i in 1:N){
  setTxtProgressBar(pb, value = i)
  o <- simulateTicTacToeGame(randomPolicy, randomPolicy, whoStarts = "agent") %>% 
    as_tibble() %>% 
    mutate(simu = i) %>%
    mutate(reward_of_simu = tail(reward, n=1))
  gameSimulations[[i]] <- o
}
gameSimulations = bind_rows(gameSimulations)

# visualize your tictactoe
ex = gameSimulations$state[2]
print(ex)
print(unflattenGameboard(ex))

# run qlearning
ql <- run_qlearning(df = gameSimulations)

# check out that we find correct behavior
# => check that on a board like this we recommend to fill the blank !!
# X X _
# ? ? ?
# ? ? ?
#
# %>% 
#  filter(substr(state, 1, 1) == "X", 
#         substr(state, 4, 4) == "X") 


# run montecarlo
mc <- run_montecarlo()

