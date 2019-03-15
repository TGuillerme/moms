#' @title Plot space
#' @description plotting space changes
#' @param space the space to plot (matrix)
#' @param remove logical, the elements to remove or add (using !)
#' @param main the title
#' @param defaults a list of parameters for plot() and points()

#' @example

#' @export

## Plot space function (utility shortcut)
plot.space <- function(space, remove, main, defaults) {
    ## Plot the first space
    plot(space, pch = defaults$pch, xlim = defaults$xlim, ylim = defaults$ylim, col = defaults$col1,
         main = main, xlab = defaults$xlab, ylab = defaults$ylab)
    
    ## Plot the second space
    points(space[remove,], pch = defaults$pch, col = defaults$col2)
}