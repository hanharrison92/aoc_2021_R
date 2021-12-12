##File name: 7.r
##Author: Hannah Harrison
##Last Edit: 10/12/2021
##Description: day 7, advent of code 2021

##load libraries
library(data.table)
library(tidyverse)

syntax_checker <- function(input_file){
    con <- file(input_file, "r")
    line_err <- NULL
    tot_score <- NULL
    while (TRUE) {
        my_line <- readLines(con, n = 1)
        if (length(my_line) == 0) {
            close(con)
            break
        }
        vec_temp <- NULL
        my_length <- nchar(my_line)
        for (i in 1:my_length) {
            if (any(c("(", "{", "[", "<") == substr(my_line, i, i))) {
                vec_temp <- c(vec_temp, substr(my_line, i, i))
            }
            else if (substr(my_line, i, i) == ")") {
                if (tail(vec_temp, n=1) == "(") {
                    vec_temp <- head(vec_temp, -1)
                }
                else {
                    line_err <- c(line_err, 3)
                    vec_temp <- NULL
                    break
                }
            }
            else if (substr(my_line, i, i) == "]") {
                if (tail(vec_temp, n=1) == "[") {
                    vec_temp <- head(vec_temp, -1)
                }
                else {
                    line_err <- c(line_err, 57)
                    vec_temp <- NULL
                    break
                }
            }
            else if (substr(my_line, i, i) == "}") {
                if (tail(vec_temp, n=1) == "{") {
                    vec_temp <- head(vec_temp, -1)
                }
                else {
                    line_err <- c(line_err, 1197)
                    vec_temp <- NULL
                    break
                }
            }
            else if (substr(my_line, i, i) == ">") {
                if (tail(vec_temp, n=1) == "<") {
                    vec_temp <- head(vec_temp, -1)
                }
                else {
                    line_err <- c(line_err, 25137)
                    vec_temp <- NULL
                    break
                }
            }
        }
      
        if(length(vec_temp) > 0) {
            vec_temp <- rev(vec_temp)
            line_score <- 0
            for (j in 1:length(vec_temp)) {
                if (vec_temp[j] == "(") {
                    line_score <- (line_score *5) + 1
                }
                else if (vec_temp[j] == "[") {
                    line_score <- (line_score * 5) + 2
                }
                else if (vec_temp[j] == "{") {
                    line_score <- (line_score * 5) + 3
                }
                else if (vec_temp[j] == "<") {
                    line_score <- (line_score * 5) + 4
                }
            }
            tot_score <- c(tot_score, line_score)
        }
    }
    return(c(sum(line_err), median(tot_score)))
}


res1_test <- syntax_checker("10_input_test.csv")
res1_real <- syntax_checker("10_input.csv")