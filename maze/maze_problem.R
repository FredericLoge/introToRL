# imports
library(tidyverse)
library(emojifont) # install.packages("emojifont")
library(gganimate)
source('maze/maze_foo.R')

# simulate maze, containing :
# - normal cell
# - goal 
# - start position
# - obstacle
# - intermediate reward 
maze1 <- simulate_maze(nbrow = 12, nbcol = 12, complexity = 5,
                       goal_reward = +100, obstacle_reward = -1000,
                       immediate_reward = +500, usual_reward = -1)

maze1 %>% count(category)

# plot maze
plot_maze_environment(maze1)

# run value iteration algorithm
vf <- run_value_function_iteration(maze1, pars = list(gamma = 0.9, delta_threshold = 0.1, max_iter = 100))

# plot maze
plot_maze_policy(vf)
