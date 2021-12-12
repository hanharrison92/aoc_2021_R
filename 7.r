##File name: 7.r
##Author: Hannah Harrison
##Last Edit: 10/12/2021
##Description: day 7, advent of code 2021

##load libraries
library(data.table)
library(tidyverse)

fuel_check1 <- function(crab_pos){
    max_pos = max(crab_pos)
    fuel_use <- replicate(max_pos + 1, NA)

    for (pos in 0:max_pos) {
        move_temp <- abs(crab_pos - pos)
        fuel_use[pos + 1] <- sum(move_temp)
    }

    pos_min <- which.min(fuel_use) - 1
    fuel_min <- min(fuel_use)

    return(c(pos_min, fuel_min))
}

fuel_check2 <- function(crab_pos){
    max_pos = max(crab_pos)
    fuel_use <- replicate(max_pos + 1, NA)

    for (pos in 0:max_pos) {
        move_temp <- abs(crab_pos - pos)
        fuel_by_crab <- ((move_temp*(1 + move_temp)) / 2)
        fuel_use[pos + 1] <- sum(fuel_by_crab)
    }

    pos_min <- which.min(fuel_use) - 1
    fuel_min <- min(fuel_use)

    return(c(pos_min, fuel_min))
}

input <- readLines("7_input.csv")
input <- as.numeric(unlist(strsplit(input, ",")))
input_test <- c(16,1,2,0,4,2,7,1,2,14)

res1_test <- fuel_check1(input_test)
res1_real <- fuel_check1(input)

res2_test <- fuel_check2(input_test)
res2_real <- fuel_check2(input)

