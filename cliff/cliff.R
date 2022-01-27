# SETUP ========================================================
#
# m x n grid
# grid indexes are as follows : 
# |         1 |   2 | ... |   m |
# |       m+1 | m+2 | ... | 2*m |
# | ...                     |
# | (n-1)*m+1 | ... | ... | n*m |
#
# last row is specially defined : 
# - start point is on index m*n+1
# - end point is on index n*m
# - points n*m+2 up to n*m-1 included are traps (cliff fall ensured)
# - wind blows to the south, such that 
#   if a == 'South':
#     P(S_{t+1} = s + m | S_t = s, A_t = a) = 1
#   else if a == 'North':
#     P(S_{t+1} = s | S_t = s, A_t = a) = p   # sur place
#     P(S_{t+1} = s-m | S_t = s, A_t = a) = 1-p # va au Nord
#   else:
#     P(S_{t+1} = s + m | S_t = s, A_t = a) = p  # va au Sud
#     P(S_{t+1} = s + 1*1{a = 'East'} - 1*1{a = 'West'} | S_t = s, A_t = a) = 1-p # va a l'Ouest ou l'Est
#
# Conditions on boundaries of course ....

# MAIN ========================================================

# imports
library(tidyverse)
library(emojifont) # install.packages("emojifont")
library(gganimate)
source('cliff/cliff_foo.R')

# generate state space
states = generate_states(n=7, m=7, goal_reward=20, abyss_reward=-20, default_reward=-1)

# generate transition matrix
tmat = generate_transition_matrix(s=states, pr=0)

# check transition matrix integrity
check_integrity_transition_matrix(tmat)

# plot environment
plot_cliff_environment(states)

# check out transition probability
tmat %>% 
  filter(state0==23) %>%
  filter(prob>0) %>%
  arrange(state1)

# run value iteration algorithm
VI = run_value_iteration(states, tmat, 
                         pars_print=list(n_chars=15),
                         pars_VI=list(gamma=0.9, 
                                      delta_threshold=1e-3, max_iter=200))

# present policy
plot_cliff_policy(VI)

# simulate runs under policy
list_sim = list()
for(i in 1:9){
  list_sim[[i]] = simulate_run(VI) %>% mutate(simu = i)
}
positions = bind_rows(list_sim)

# visualize result
gg = plot_cliff_environment(VI$states)
an = gg + 
  labs(title = 'Cliff environment simulation') + 
  geom_text(data=positions %>% filter(simu == 1), 
            aes(y=row, x=col), 
            label=emoji(aliases='runner'), 
            family='EmojiOne', cex=25, vjust=0.25)
animate(an + transition_states(time_index), 
        nframes = 24, 
        device = "png",
        renderer = file_renderer("~/Documents/cliff", prefix = "cliff_plot", overwrite = TRUE))
## anim_save('anim.gif', an + transition_states(time_index))

