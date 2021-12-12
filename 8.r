##File name: 7.r
##Author: Hannah Harrison
##Last Edit: 10/12/2021
##Description: day 7, advent of code 2021

##load libraries
library(data.table)
library(tidyverse)

digit_checker <- function(input_file){
    con <- file(input_file, "r")
    tot_sum <- NULL
    while (TRUE) {
        my_line <- readLines(con, n = 1)
        if (length(my_line) == 0) {
            close(con)
            break
        }
        
        two_parts <- unlist(strsplit(my_line, "[|]"))
        output <- unlist(strsplit(trimws(two_parts[2]), " "))
        word_length <- nchar(output)
        sum_output <- sum(word_length == 2) + sum(word_length == 3) + sum(word_length == 4) + sum(word_length == 7)
        tot_sum <- c(tot_sum, sum_output)
    }
    return(sum(tot_sum))
}

res_test <- digit_checker("8_input_test.csv")
res_real <- digit_checker("8_input.csv")

