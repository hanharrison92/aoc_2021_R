##File name: 11.r
##Author: Hannah Harrison
##Last Edit: 16/12/2021
##Description: day 16, advent of code 2021

##load libraries
library(data.table)
library(tidyverse)

flash_counter <- function(my_mat, num_steps) {
    counter <- 0
    for (i in 1:num_steps) {
        my_mat <- my_mat + 1
        pos <- which(my_mat > 9, arr.ind = TRUE)
        while (dim(pos)[1] > 0) {
            for (j in 1:dim(pos)[1]) {
                x_range <- c((pos[j, 1] - 1):(pos[j, 1] + 1))
                x_range <- x_range[x_range > 0 & x_range < 11]
                y_range <- c((pos[j, 2] - 1):(pos[j, 2] + 1))
                y_range <- y_range[y_range > 0 & y_range < 11]
                
                my_mat[min(x_range):max(x_range), min(y_range):max(y_range)] <- 
                    my_mat[min(x_range):max(x_range), min(y_range):max(y_range)] + 1
                my_mat[pos[j, 1], pos[j, 2]] <- 1000
                }
            pos <- which((my_mat > 9) & (my_mat < 1000), arr.ind = TRUE)
        }
    counter <- counter + length(which(my_mat > 9))
    my_mat[my_mat > 9] <- 0
    
    }
return(counter)
}
 

simultaneous_flash <- function(my_mat) {
    clock <- 1
    while (TRUE) {
        my_mat <- my_mat + 1
        pos <- which(my_mat > 9, arr.ind = TRUE)
        while (dim(pos)[1] > 0) {
            for (j in 1:dim(pos)[1]) {
                x_range <- c((pos[j, 1] - 1):(pos[j, 1] + 1))
                x_range <- x_range[x_range > 0 & x_range < 11]
                y_range <- c((pos[j, 2] - 1):(pos[j, 2] + 1))
                y_range <- y_range[y_range > 0 & y_range < 11]
                
                my_mat[min(x_range):max(x_range), min(y_range):max(y_range)] <- 
                    my_mat[min(x_range):max(x_range), min(y_range):max(y_range)] + 1
                my_mat[pos[j, 1], pos[j, 2]] <- 1000
                }
            pos <- which((my_mat > 9) & (my_mat < 1000), arr.ind = TRUE)
        }
    flashes <- length(which(my_mat > 9))
    my_mat[my_mat > 9] <- 0
    if (flashes == 100) {
        break
    }
    clock <- clock + 1
    }
return(clock)
}

df_input <- fread("11_input.csv", colClasses = "character")

temp_col_names <- c("space", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10")
df_input <- df_input %>% separate(col = V1, into =  temp_col_names, sep = "") %>% select(-space)
mat_input <- as.matrix(df_input)
mat_input <- matrix(as.numeric(mat_input), ncol = ncol(mat_input))

res1 <- flash_counter(mat_input, 100)
res2 <- simultaneous_flash(mat_input)

