##File name: 2.r
##Author: Hannah Harrison
##Last Edit: 03/12/2021
##Description: day 2, advent of code 2021

##load libraries
#library(tidyverse) 
#library(dplyr)
library(data.table)

#define functions
coord_cal <- function(my_instr){
    n <- length(my_instr$type)
    my_pos <- c(0, 0)
    for(i in 1:n) {
        if(my_instr$type[i] == "forward") {
            my_pos[1] = my_pos[1] + my_instr$amount[i]
        }
        else if(my_instr$type[i] == "down") {
            my_pos[2] = my_pos[2] + my_instr$amount[i]
        }
        else if(my_instr$type[i] == "up") {
            my_pos[2] = my_pos[2] - my_instr$amount[i]
        }

    }
    return(my_pos)
}

coord_cal_2 <- function(my_instr){
    n <- length(my_instr$type)
    my_pos <- c(0, 0, 0) #horizontal position, depth and aim
    for(i in 1:n) {
        if(my_instr$type[i] == "forward") {
            my_pos[1] = my_pos[1] + my_instr$amount[i]
            my_pos[2] = my_pos[2] + (my_pos[3]*my_instr$amount[i]) 
        }
        else if(my_instr$type[i] == "down") {
            my_pos[3] = my_pos[3] + my_instr$amount[i]
        }
        else if(my_instr$type[i] == "up") {
            my_pos[3] = my_pos[3] - my_instr$amount[i]
        }

    }
    return(my_pos)
}


input <- fread("2_input.csv")
test_input <- data.frame(c("forward", "down", "forward", "up", "down", "forward"))
test_input$amount <- c(5, 5, 8, 3, 8, 2)
colnames(test_input) <- c("type", "amount")
colnames(input) <- c("type", "amount")

test_pos <- coord_cal(test_input)
real_pos <- coord_cal(input)
ans1 <- real_pos[1]*real_pos[2]

test_pos_2 <- coord_cal_2(test_input)
real_pos_2 <- coord_cal_2(input)
ans2 <- real_pos_2[1]*real_pos_2[2]