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
source('tictactoe/tictactoe_foo_filled.R')
# source('tictactoe/ticTacToe_foo_to_fill.R')

# run bunch of simulations
gameSimulations = list()
N <- 5000
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

# check that we have learned a proper action
test_states <- tibble(state=ql$stateEnums[1]) %>%
  mutate(index=row_number()) 
test_states <- tibble(state=ql$stateEnums) %>%
  mutate(index=row_number()) %>% 
  filter(substr(state, 3, 3) == "X") %>%
  filter(str_count(string=state, pattern="X")==1) 
test_states <- tibble(state=ql$stateEnums) %>%
  mutate(index=row_number()) %>% 
  filter(substr(state, 1, 1) == "X", 
         substr(state, 4, 4) == "X",
         substr(state, 7, 7) == "_") 
test_states <- tibble(state=ql$stateEnums) %>%
  mutate(index=row_number()) %>% 
  filter(substr(state, 1, 1) == "X", 
         substr(state, 5, 5) == "X",
         substr(state, 9, 9) == "O") 

# 
index <- 1

# print table
unflattenGameboard(test_states$state[index])

# check best actions
qsa_est <- ql$qsa[test_states$index[index],]
reco <- ql$actionEnums[our_which_max_(qsa_est)]
reco <- as.integer(str_split(reco, "_")[[1]])

# plot gameboard and recommendation
plotGameboard(test_states$state[index]) +
  geom_text(data = tibble(row=reco[1], col=reco[2], value="X"), cex=10, color="red")+
  geom_text(data = 
              tibble(expand.grid(col=(1:3), row=(1:3)+0.35)) %>%
              mutate(value=round(qsa_est,2)) %>%
              filter(value>-20) %>%
              mutate(value = ifelse(value<0, as.character(value), paste0("+", value))), 
            color="blue")

### run montecarlo
mc <- run_montecarlo()

