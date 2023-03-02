# # # Shortest Path Problem on a grid --------------------

# # # Start code

# # # Libraries & functions ------------------------------

# load tidyverse library
# useful here mostly for dplyr/ggplot pkgs
library(tidyverse)

# generate state environment for SPP
# only one terminal state
# potentially some states don't allow you to move
spp_grid <- function(nx, ny, p=0, seed=1234){
  set.seed(seed) # note that if p=0, this changes nothing
  expand.grid(x=1:nx, y=1:ny) %>%
    mutate(p_stuck = case_when(p<1e-3 ~ 0, TRUE ~ rbeta(n=n(), shape1=1, shape2=1/p-1))) %>%
    mutate(is_terminal = (x==nx & y==ny)) %>%
    mutate(reward = -1*(!is_terminal))
}

# get indices over grid (1:nx)x(1:ny) which are adjacent
# directly to position (ix, iy)
# note: a small utilities function which could be helpful for the 
#       implementation but not required
get_adjacent_indices <- function(nx, ny, ix, iy){
  l = list()
  if(ix>1){  l[["left"]] = c(ix-1, iy) }
  if(ix<nx){ l[["right"]] = c(ix+1, iy) }
  if(iy>1){  l[["top"]] = c(ix, iy-1) }
  if(iy<ny){ l[["down"]] = c(ix, iy+1) }
  return(l)
}

# # # Minimal example ----------------------------------------

# generate 5x5 grid
# > note, if you change arg p to 0.1, you will have a grid 
#   with in average 10% risk of getting stuck on a cell
df <- spp_grid(nx=5, ny=5, p=0)

# check out the generated grid
df %>% glimpse()
df %>% head()

# nice plot of the grid 
ggplot(df) + 
  geom_tile(aes(x=x, y=y, fill=p_stuck, col=ifelse(is_terminal, "Yes", "No")), lwd=1) + 
  coord_equal() +
  scale_fill_viridis_c() +
  theme_minimal(base_line_size = 0) +
  theme(legend.position = 'right',
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 10),
        axis.text = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(r=10, t=10, b=0, l=0), angle=0, vjust=1/2), 
        axis.title.x = element_text(size=15, margin=margin(r=10, t=10, b=0, l=0), angle=0, vjust=1/2)) +
  labs(title = "Shortest path problem, on a 5x5 grid",
       col = "End ?",
       fill = "P(stuck)")

# note: when you will have found which best action is to take on each
# cell, just display it directly on the graph, "Top", "Down", "Left", "Right"
