#' @title Plot space
#' @description plotting the spaces changing
#' @param space the space to plot (matrix)
#' @param remove logical, the elements to remove or add (using !)
#' @param main the title
#' @param defaults a list of parameters for plot() and points()
#' @examples

## Plot space function (utility shortcut)
plot.space <- function(space, remove, main, defaults, ...) {
    ## Plot the first space
    plot(space, pch = defaults$pch, xlim = defaults$xlim, ylim = defaults$ylim, col = defaults$col1,
         main = main, xlab = defaults$xlab, ylab = defaults$ylab, cex = defaults$cex, ...)
    
    ## Plot the second space
    points(space[remove,], pch = defaults$pch, col = defaults$col2, cex = defaults$cex)
}


#' @title Plot the results fable for the metrics analysis
#' @description plotting the spaces changing
#' @param cent.tend the analysis central tendencies
#' @param CIs the analysis CIs
#' @param col the colours for the results (i.e. for the different metrics)
#' @param space the space to be plotted on the side (plot.space)
#' @param remove_list a list of elements to remove in the space (plot.space)
#' @param defaults a list of arguments for plot.space
#' @examples


plot.metrics <- function(cent.tend, CIs, col, space, remove_list, defaults) {

    empty.plot <- function(n_metrics, names_metrics, is.last = FALSE) {
        ## Plot margins
        par(bty = "n", mar = c(ifelse(is.last, 4, 0),10,0.5,0))
        ## Plot size
        if(!is.last) {
            plot(NULL, ylim = range(1:n_metrics), pch = 19, xlim = c(-1,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        } else {
            plot(NULL, ylim = range(1:n_metrics), pch = 19, xlim = c(-1,1), ylab = "", yaxt = "n", xlab = "Scaled disparity")
        }
        ## Adding lines
        abline(v = 0, lty = 2, col = "grey")
        ## Adding the y axis
        axis(2, at = 1:n_metrics, labels = names_metrics, las = 2)
        ## Adding the x axis
        if(!is.last) {
            axis(1, labels = FALSE, tick = TRUE, col.ticks = "white", col = "grey")
        }
    }

    add.points <- function(cent.tend, one_reduction, col) {
        ## Get the right values 
        points_to_add <- as.numeric(lapply(cent.tend, function(X, Y) return(X[Y]), Y = one_reduction))
        ## Plot the values
        points(x = points_to_add, y = 1:length(points_to_add), col = col, pch = 19)
    }

    add.lines <- function(CIs, one_reduction, col) {

        ## Number of quantiles
        quantiles_n <- nrow(CIs[[1]])
        ## Extract the values for y
        y_vals <- lapply(as.list(1:length(CIs)), function(X) rep(X, 2))
        ## Extract the values for x
        x_vals <- list()
        for(cis in 1:(quantiles_n/2)) {
            x_vals[[cis]] <- lapply(CIs, function(X, Y, cis, quantiles_n) return(X[(1:quantiles_n)[c(cis, quantiles_n-(cis-1))],Y]), Y = one_reduction, cis, quantiles_n)
        }
        ## Plotting all the lines
        for(one_metric in 1:length(CIs)) {
            for(cis in 1:(quantiles_n/2)) {
                lines(x_vals[[cis]][[one_metric]], y_vals[[one_metric]], col = col[one_metric], lty = (quantiles_n/2 - cis + 1), lwd = cis * 1.5)
            }
        }
    }

    ## Set some variables
    n_reductions <- length(cent.tend[[1]])-1
    n_metrics <- length(cent.tend)
    names_metrics <- names(cent.tend)

    ## Set up the layout matrix
    layout_matrix <- matrix(c(1:(n_reductions*2)), ncol = 2, nrow = n_reductions)
    layout <- layout(layout_matrix, c(4,1), c(rep(1, (n_reductions - 1)), 1.6))
    # layout.show(layout)

    ## Plot the results in each cell
    for(one_reduction in 2:n_reductions) {
        ## All plots minus last
        empty.plot(n_metrics, names_metrics)
        add.lines(CIs, one_reduction, col)
        add.points(cent.tend, one_reduction, col)
    }

    ## Last row
    empty.plot(n_metrics, names_metrics, is.last = TRUE)
    add.lines(CIs, one_reduction = n_reductions+1, col)
    add.points(cent.tend, one_reduction = n_reductions+1, col)

    ## Plotting the space transformations in the following cells
    defaults$xlab <- defaults$ylab <- ""
    defaults$cex <- 0.4
    for(removal in 1:length(remove_list)) {
        par(bty = "n", mar = c(ifelse(removal != length(remove_list), 1, 4),2,1,2))
        plot.space(space, remove_list[[removal]], main = names(remove_list)[removal], defaults, xaxt = "n", yaxt = "n")
    }
}