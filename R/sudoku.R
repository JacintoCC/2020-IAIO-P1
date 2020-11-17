#' Get Sudoku
#' @return Sudoku board
#' @export
generateSudoku <- function() {
   sudoku <- jsonlite::fromJSON("https://sugoku.herokuapp.com/board?difficulty=easy")
   return(sudoku$board)
}

#' Write Sudoku
#' @param board Board to be written in .sud format
#' @param file File name to write sudoku. Default is sudoku.sud
#' @importFrom utils write.table
#' @export
writeSudoku <- function(board, file="sudoku.sud") {
   board[board==0] = " "
   board <- cbind(rep(" ", 9), board, rep(".", 9))
   write.table(board, file, sep = "|", row.names = F, col.names = F, quote = F)
}
