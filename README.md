# introToRL 🤖

## Description 

Quick introduction to Reinforcement Learning

Currently, all code is written in R

## Setup

1. Clone the repository on your local env. 
2. Using RStudio, you may click on introToRL.Rproj to launch project and start running experiments in cliff/, maze/, restaurant/, etc. 

## Tabular Q-Learning

Consider the Tic-Tac-Toe game, where you start on the board. Can you find an optimal way to play ?

The game is suited to present how tabular Q-Learning can be used and set up, check directory tictactoe/.

## Q-Learning with function approximation

Consider now that we are trying to learn how to navigate in a [0,1]x[0,1] grid, where the exit is in a square of the grid [0.75,0.85]x[0.75,0.85] and there is a wall at x=0.5 and from y=0.5 to 1.0. The constraint we set ourselves: no discretization of the state space. In directory continuousGrid/ we are using linear function approximation to solve this problem. Please check it out.

