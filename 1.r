##File name: 1.r
##Author: Hannah Harrison
##Last Edit: 03/12/2021
##Description: day 1, advent of code 2021

##load libraries
#library(tidyverse) 
#library(dplyr)
library(data.table)

#define functions
depth_increase <- function(my_depths){
    n <- length(my_depths) - 1
    counter <- 0
    for(i in 1:n) {
        diff <- my_depths[i+1] - my_depths[i]
        if(diff > 0) {#increment counter if next measurement is deeeper
            counter <- counter + 1
        }
    }
    return(counter)
}

depth_window <- function(my_depths){
    n <- length(my_depths) 
    counter <- 0
    old_window <- my_depths[1:3]
    for(i in 1:(n-3)) {
        new_window <- my_depths[(i+1):(i+3)]
        diff <- sum(new_window) - sum(old_window)
        if(diff > 0) {#increment counter if next measurement is deeper
            counter <- counter + 1
        }
        old_window <- new_window
    }
    return(counter)
}


input <- fread("1_input.csv")
test_input <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

counter_test <- depth_increase(test_input)
counter_real <- depth_increase(input$V1)
g
window_test <- depth_window(test_input)
window_real <- depth_window(input$V1)