##File name: 4.r
##Author: Hannah Harrison
##Last Edit: 04/12/2021
##Description: day 4, advent of code 2021

##load libraries
library(data.table)

#define functions
get_bingo <- function(input_file, num) {
    bingo_calls <- as.vector(read.csv(input_file, nrows = 1, header = F))
    curr_best <- c(length(bingo_calls), 0, 0)
    curr_worst <- c(0, 0, 0)
    for (i in 1:num) {
        mat_bingo <- as.matrix(read.table(input_file, skip = 2+(i-1)*6 , nrows = 5,  header = F))
        bingo_res <- play_bingo(bingo_calls, mat_bingo)
        if (bingo_res[1] < curr_best[1]) {
            curr_best <- bingo_res
        }
        if (bingo_res[1] > curr_worst[1]) {
            curr_worst <- bingo_res
        }
    }
    return(c(curr_best, curr_worst))
}

play_bingo <- function(calls, mat_board) {
    check_sheet <- matrix(0, ncol = ncol(mat_board), nrow = nrow(mat_board))
    BINGO <- 0
    i <- 0
    while (!BINGO) {
        i <- i + 1
        if (calls[,i] %in% mat_board) {
            idx <- which(mat_board == calls[,i])
            check_sheet[idx] <- 1
            if (max(c(rowSums(check_sheet), colSums(check_sheet))) == 5) {
                BINGO <- 1
            }
        }   
    }
    idx_final <- which(check_sheet == 0)
    return(c(i, calls[,i], sum(mat_board[idx_final]))) 
}

input_file <- file.path(getwd(), "4_input.csv")
input_test <- file.path(getwd(), "4_input_test.csv")

res <- get_bingo(input_file, 100)
res_test <- get_bingo(input_test, 3)

ans1 <- res[2]*res[3]
ans2 <- res[5]*res[6]