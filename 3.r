##File name: 3.r
##Author: Hannah Harrison
##Last Edit: 03/12/2021
##Description: day 3, advent of code 2021

##load libraries
#library(tidyverse) 
#library(dplyr)
library(data.table)
library(compositions) #includes function unbinary

#define functions
get_power <- function(my_binary){
    df_split <- my_binary[, tstrsplit(V1, "")]
    df_split <- data.frame(lapply(df_split, as.numeric))
    vec_meds <- apply(df_split, 2, median)
    vec_not_meds <- bitwNot(vec_meds) + 2
    gamma_rate <- compositions::unbinary(paste(vec_meds, collapse = ""))
    epsilon_rate <- compositions::unbinary(paste(vec_not_meds, collapse = ""))
    return(gamma_rate * epsilon_rate)
}

get_oxygen <- function(my_binary){
    df_split <- data.frame(my_binary[, tstrsplit(V1, "")])
    num_rows <- nrow(df_split)
    col_idx <- 1
    while (num_rows > 1) {
        sum_temp <- sum(as.numeric(df_split[, col_idx]))
        if (sum_temp >= num_rows/2) {#if 1 is most common or they are the same
            df_split <- df_split[df_split[ , col_idx] == "1", ]
        }
        else {#if 0 is most common 
            df_split <- df_split[df_split[ , col_idx] == "0", ]
        }
        col_idx <- col_idx + 1
        num_rows <- nrow(df_split)         
    }
    oxygen_rate <- compositions::unbinary(paste(df_split, collapse = ""))
    return(oxygen_rate)
}

get_co2 <- function(my_binary){
    df_split <- data.frame(my_binary[, tstrsplit(V1, "")])
    num_rows <- nrow(df_split)
    col_idx <- 1
    while (num_rows > 1) {
        sum_temp <- sum(as.numeric(df_split[, col_idx]))
        if (sum_temp >= num_rows/2) {#if 1 is the most common or they are same
            df_split <- df_split[df_split[ , col_idx] == "0", ]
        }
        else {#if 0 is the most common
            df_split <- df_split[df_split[ , col_idx] == "1", ]
        }
        col_idx <- col_idx + 1
        num_rows <- nrow(df_split)
    }
    co2_rate <- compositions::unbinary(paste(df_split, collapse = ""))
    return(co2_rate)
}


input <- fread("3_input.csv", colClasses = "character")
V1 <- c("00100", "11110", "10110", "10111", "10101", "01111", 
        "00111", "11100", "10000", "11001", "00010", "01010")
test_input <- data.table(V1)

res_test <- get_power(test_input)
res_real <- get_power(input)

#res2_test_oxy <- get_oxygen(test_input)
#res2_test_co2 <- get_co2(test_input)

res2_oxy <- get_oxygen(input)
res2_co2 <- get_co2(input)

ans <- res2_oxy * res2_co2
