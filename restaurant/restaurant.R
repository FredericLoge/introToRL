# PREPARATION ======================================================

# tidyverse for data manip and viz
library(tidyverse)

# load functions
source('restaurant/restaurant_foo.R')
source('restaurant/restaurant_foo_strategy_to_fill.R')

# PROBLEM SETUP ======================================================

# define number of actions
nb_restaurants = 5

# generate model parameter (unknown in the bandit game)
set.seed(530)
p = sort(x=runif(n = nb_restaurants, min = 0, max = 1), decreasing=TRUE)

# define number of timesteps for simulation
nb_timesteps = 1000

# define strategy
strategy = list('name'='RANDOM', 'pars'=list(nb_cold_start_rounds=5))
# strategy = list('name'='ETC', 'pars'=list(m=30, nb_cold_start_rounds=5))
# strategy = list('name'='EXP3', 'pars'=list(nb_cold_start_rounds=0, eta=0.5, weights=rep(1,nb_restaurants)))
# strategy = list('name'='EPS_GREEDY', 'pars'=list(nb_cold_start_rounds=5, eps=0.1))
# strategy = list('name'='UCB', 'pars'=list(nb_cold_start_rounds=5, alpha=0.05))
# strategy = list('name'='TS', 'pars'=list(nb_cold_start_rounds=5))

# SIMULATION ======================================================

# simulate bandit game
his = tibble(time_index = 1:nb_timesteps, action = NA_real_, reward = NA_real_)
pb = txtProgressBar(min=0, max=nb_timesteps, style=3)
for(i in 1:nb_timesteps){
  setTxtProgressBar(pb, value=i)
  # choose action
  if(i <= strategy$pars$nb_cold_start_rounds*nb_restaurants){
    # iterate through each possible action
    a = i %% nb_restaurants
    if(a == 0) a = nb_restaurants
  }else{
    a = pick_action(historic = his[1:(i-1),], round_index = i, strategy = strategy, nb_actions = nb_restaurants)
  }
  
  # collect reward
  r = rbinom(n = 1, size = 1, prob = p[a])
  
  # update strategy parameter (EXP3)
  if(strategy$name == 'EXP3'){
    ...
  }
  
  # store in history
  his$time_index[i] <- i
  his$action[i] <- a
  his$reward[i] <- r

}

# STATISTICS AND GRAPHICS ======================================================

# compute statistics
his <- his %>%
  mutate(action = factor(action)) %>%
  mutate(cum_reward = cumsum(reward)) %>%
  mutate(regret = max(p) - p[action]) %>%
  mutate(cum_regret = cumsum(regret)) %>%
  mutate(cum_regret_normalized = cum_regret/(1:n())) %>%
  mutate(action_is_optimal = (action == which.max(p))) %>%
  mutate(cum_action_is_optimal_normalized = cumsum(action_is_optimal)/(1:n()))

# 
size_ratio <- 0.5
some_theme <-   theme(legend.position = 'top',
                      plot.title = element_text(size=25*size_ratio, margin=margin(t=20, b=20)),
                      axis.text = element_text(size=20*size_ratio),
                      axis.title.x = element_text(size=20*size_ratio, margin=margin(t=20, b=20)),
                      axis.title.y = element_text(size=20*size_ratio, margin=margin(l=20, r=20)))

# action sequence
o <- purrr::map(1:nb_restaurants, function(a){
  cumsum(his$action == a)/1:nrow(his)
}) %>%
  setNames(nm = paste0('action', 1:nb_restaurants)) %>%
  as_tibble() %>%
  mutate(time_index = 1:n(), .before='action1') %>%
  pivot_longer(cols=-time_index, names_to='action', values_to='prop') %>%
  mutate(action = sub('action', '', action))
g0 <- ggplot(o) +
  aes(x = time_index, y = prop, fill = action) +
  geom_area() +
  geom_label(data=o %>% filter(time_index == nb_timesteps), aes(x = time_index, y = prop, label = prop*nb_timesteps), position = position_stack(), show.legend = FALSE) +
  labs(x = 'Time', y = 'Cumulative selection proportion', fill = 'Action chosen',
       title = 'Which action was chosen over time ?') +
  geom_vline(xintercept = strategy$pars$nb_cold_start_rounds * nb_restaurants, lty = 2) +
  some_theme

# cumulative reward
g1 <- ggplot(his) +
  aes(x = time_index, y = cum_reward, col = action) + 
  geom_point() +
  labs(x = 'Time', y = 'Cumulative reward', col = 'Restaurant chosen',
     title = 'Cumulative reward collected over time') +
  geom_vline(xintercept = strategy$pars$nb_cold_start_rounds * nb_restaurants, lty = 2) +
  some_theme

# cumulative regret
g2 <- ggplot(his) +
  aes(x = time_index, y = cum_regret, col = action) + 
  geom_point() +  
  labs(x = 'Time', y = 'Cumulative regret', col = 'Restaurant chosen',
     title = 'Cumulative regret over time') +
  geom_vline(xintercept = strategy$pars$nb_cold_start_rounds * nb_restaurants, lty = 2) +
  some_theme

# cumulative regret normalized
g3 <- ggplot(his) +
  aes(x = time_index, y = cum_regret_normalized) + 
  geom_point(aes(col = action)) +
  labs(x = 'Time', y = 'Cumulative regret/Time', col = 'Restaurant chosen',
     title = 'Cumulative regret normalized over time') +
  geom_vline(xintercept = strategy$pars$nb_cold_start_rounds * nb_restaurants, lty = 2) +
  some_theme


# pct of times optimal action was taken
g4 <- ggplot(his) +
  aes(x = time_index, y = cum_action_is_optimal_normalized) + 
  geom_point(aes(col = action)) +
  geom_vline(xintercept = strategy$pars$nb_cold_start_rounds * nb_restaurants, lty = 2) +
  geom_hline(yintercept = 1/nb_restaurants, lty = 2) +
  ylim(0,1) +
  labs(x = 'Time', y = '% of times optimal action was taken', col = 'Restaurant chosen',
       title = 'Percentage of times optimal action was taken') +
  some_theme

# estimates of different actions
g5 <- his %>% 
  group_by(action) %>%
  summarise(m = mean(reward), 
            n = n(), 
            n0 = sum(reward),
            lb_m = get_CI(n = n, s = n0, alpha = 0.05)$auto[1],
            hb_m = get_CI(n = n, s = n0, alpha = 0.05)$auto[2]
          ) %>%
ggplot() +
  aes(x = action, y = m, col = action) + 
  geom_point(cex = 5) +
  geom_errorbar(aes(ymin = lb_m, ymax=hb_m)) +
  geom_point(aes(x = factor(1:nb_restaurants), y = p), cex = 10, pch="x", col = 'black') +
  theme(legend.position = 'bottom') +
  labs(x = 'Action', y = 'Estimate', col = 'Restaurant chosen',
       title = 'Parameter estimate and confidence interval') +
  ylim(0,1) +
  some_theme

library(gridExtra)
grid.arrange(g0,g1,g2,g3,g4,g5,ncol=3,nrow=2)
