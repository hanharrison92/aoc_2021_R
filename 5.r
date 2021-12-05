##File name: 5.r
##Author: Hannah Harrison
##Last Edit: 05/12/2021
##Description: day 5, advent of code 2021

##load libraries
library(data.table)
library(tidyverse)

check_vents <- function(input) {
    #format corrdinates
    colnames(input) <- c("x1", "middle", "y2")
    df_coords <- input %>%  separate(middle, into = c("y1", "x2"), sep =  "->")
    x1 <- as.numeric(df_coords$x1) + 1
    x2 <- as.numeric(df_coords$x2) + 1
    y1 <- as.numeric(df_coords$y1) + 1
    y2 <- as.numeric(df_coords$y2) + 1
    #define map
    max_x <- max(c(x1, x2))
    max_y <- max(c(y1, y2))
    dim <- max(max_x, max_y)
    map_vents <- matrix(0, ncol = dim, nrow = dim)
    #iterate through lines of vents
    for (i in 1:length(x1)) {
        if (x1[i] == x2[i]) {##x is the same
            map_vents[x1[i], y1[i]:y2[i]] <- map_vents[x1[i], y1[i]:y2[i]] + 1
        }
        else if (y1[i] == y2[i]) {##y is the same
            map_vents[x1[i]:x2[i], y1[i]] <- map_vents[x1[i]:x2[i], y1[i]] + 1
        }
        else {#diagonals
            range_x <- 0:(x2[i] - x1[i])
            range_y <- 0:(y2[i] - y1[i])
            for (j in 1:length(range_x)) {
                map_vents[x1[i] + range_x[j], y1[i] + range_y[j]] <- 
                    map_vents[x1[i] + range_x[j], y1[i] + range_y[j]] + 1
            }
        }
    }
    #check for hotspots
    cross_vents <- length(which(map_vents > 1))
    return(cross_vents)
}

input <- fread("5_input.csv")
input_test <- fread("5_input_test.csv")

res_test <- check_vents(input_test)
res_real <- check_vents(input)
