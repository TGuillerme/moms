#' @title Results fable
#'
#' @description Plots the results f(igure and t)able.
#'
#' @param results the list of all the results
#' @param test.results the list of the test results (named)
#' @param col two plotting colours
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

results.fable <- function(results, test.results, col = c("orange", "blue")) {

    ## Making the general plot window
    make.plot.window <- function(rows, cols) {
        ## Variables
        n_rows <- length(rows[[2]])
        ## Cols
        n_cols <- length(cols[[1]][[1]])+1
        ## Making the plot window
        layout_matrix <- matrix(c(1:(n_rows * n_cols)), nrow = n_rows, ncol = n_cols)
        ## Cell parameters
        col_spaces_width <- 1
        col_result_width <- rep(1, (n_cols-1))
        first_row_height <- 1.3
        last_row_height <- 1.4
        middle_row_heights <- rep(1, (n_rows - 2))
        ## Making the layout
        layout <- layout(layout_matrix,
                        c(col_spaces_width, col_result_width),
                        c(first_row_height, middle_row_heights, last_row_height))
        return(invisible())
        #layout.show(layout)
    }


    ## The number of rows (metrics)
    n_rows <- length(results[[1]]) + 1

    ## The number of columns (3 times the transformations + the length of test results)
    n_cols <- 3 + length(test.results) + 1




    return()
}