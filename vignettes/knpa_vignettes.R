## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----initialize---------------------------------------------------------------
library(knpa)

RNGversion(min(as.character(getRversion()),"3.5.3"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
  )

## ----brute_force_knapsack-----------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ----brute_force_knapsack_parallel--------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = TRUE)

## ----dynamic_knapsack---------------------------------------------------------
dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)

## ----greedy_knapsack----------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)

