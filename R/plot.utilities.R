#' @title Plot space
#' @description plotting the spaces changing
#' @param space the space to plot (matrix)
#' @param remove logical, the elements to remove or add (using !)
#' @param main the title
#' @param defaults a list of parameters for plot() and points()
#' @param axis which axis to plot
#' @examples

## Plot space function (utility shortcut)
plot.space <- function(space, remove, main, defaults, axis = c(1,2), ...) {
    ## Plot the first space
    plot(space[, axis], pch = defaults$pch, xlim = defaults$xlim, ylim = defaults$ylim, col = defaults$col1,
         main = main, xlab = ifelse(defaults$xlab == "", "", paste(defaults$xlab, axis[1])), ylab = ifelse(defaults$ylab == "", "", paste(defaults$xlab, axis[1])), cex = defaults$cex, ...)
    
    ## Plot the second space
    points(space[remove, axis], pch = defaults$pch, col = defaults$col2, cex = defaults$cex)
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


plot.results <- function(cent.tend, CIs, col, space, remove_list, defaults) {

    empty.plot <- function(n_metrics, names_metrics, is.last = FALSE) {

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
        ## Adding the x axis
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

        ## Number of quantiles
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


#' @title Plot the results fable for the metrics analysis
#' @description plotting the spaces changing
#' @param space_results a named list of results from different simulated spaces
#' @param col the colours for the results (i.e. for the different spaces)
#' @param remove the proportion of elements removed
#' @param metrics_names optional, a list of metrics_names
#' @param defaults a list of arguments for plot.space
#' @param reduce.distribution the function for making the spaces for the row "names"
#' @param text.in.cell logical, whether to add text in the first cell (i.e. legend)
#' @examples

plot.metrics <- function(space_results, col, remove, metrics_names, defaults, reduce.distribution = rnorm, text.in.cell = FALSE) {

    ## Set the colours
    if(class(col) == "function") {
        col <- rev(col(length(space_results)))
    }
    
    if(missing(metrics_names)) {
        metrics_names <- names(space_results[[1]][[1]])
    }

    ## Randomly make some removal plots as column "names"
    make.reductions <- function(remove, reduce.distribution) {
        space <- space.maker(200, 2, reduce.distribution)
        ## Do the space reductions
        random <- reduce.space(space, type = "random", remove = remove)
        limit <- reduce.space(space, type = "limit", remove = remove)
        displacement <- reduce.space(space, type = "displacement", remove = remove)
        density <- reduce.space(space, type = "density", remove = remove)
        ## Return every "row"
        return(list(space = space,
                    reductions = list(
                                "random" = random,
                                "limit (inner)" = limit,
                                "limit (outer)" = !limit,
                                "displace (+)" = displacement,
                                "displace (-)" = !displacement,
                                "density (high)" = density,
                                "density (low)" = !density)))
    }

    ## Making the general plot window
    make.plot.window <- function(rows, cols) {
        ## Variables
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
        ## Making the layout
        layout <- layout(layout_matrix,
                        c(col_spaces_width, col_result_width),
                        c(first_row_height, middle_row_heights, last_row_height))
        return(invisible())
        #layout.show(layout)
    }

    ## Plotting the spaces (i.e. row names)
    plot.reduction <- function(removal, reductions, defaults) {
        ## Update the defaults
        defaults$ylab <- ""
        defaults$xlab <- ""
        defaults$cex <- 0.3
        
        ## Plot window
        par(bty = "n",
            mar = c(bottom = ifelse(removal != length(reductions$reductions), 1, 4),
                    left   = 1,
                    top    = ifelse(removal != 1, 1, 4),
                    right  = 1)
            )

        ## Plot space change

        plot.space(reductions$space, reductions$reductions[[removal]], main = names(reductions$reductions[removal]), defaults, xaxt = "n", yaxt = "n")
        return(invisible())
    }

    ## Plot one result
    empty.plot <- function(space_results, overal_range, is.last, is.first, main) {
        
        ## Set margins (auto)
        par(bty ="l",
            mar = c(bottom = ifelse(is.last, 4, 1),
                    left   = 1,
                    top    = ifelse(is.first, 4, 1),
                    right  = 1))

        ## Make plot (empty)
        plot(NULL, ylim = 1:length(space_results[[1]]), xlim = overal_range,
            ylab = "", xaxt = "n", yaxt = "n", 
            xlab = ifelse(is.last, "Difference", ""), 
            main = ifelse(is.first, main, ""))

        ## Adding lines
        abline(v = 0, lty = 2, col = "grey")

        ## If last axis ad values on x
        if(is.last) {
            axis(1)
        }
        return(invisible())
    }

    ## Add the CIs and median
    add.lines.points <- function(one_space, space_results, one_metric, reduce, col, y.val, text.in.cell) {
        ## Extract the results for one metric and one space
        CIs <- space_results[[one_space]]$CI[[one_metric]]
        point <- space_results[[one_space]]$cent.tend[[one_metric]]
        ## Number of quantiles
        quantiles_n <- nrow(CIs)
        ## Extract the values for x and y
        x_vals <- CIs[, reduce]
        y_vals <- rep(y.val, 2)
        ## Plot the lines
        for(cis in 1:(quantiles_n/2)) {
            lines(x = x_vals[c(cis, (quantiles_n:1)[cis])],
                  y = y_vals,
                  lty = (quantiles_n/2 - cis + 1),
                  lwd = cis * 1.5,
                  col = col[one_space])
        }
        points(x = point[reduce], y = y_vals[1], pch = 21, bg = col[one_space], col = "black")
        if(text.in.cell) {
            text(x = point[reduce], y = y_vals[1], pos = 4, labels = names(space_results)[one_space], cex = 0.1)
        }
        return(invisible())
    }

    ## Making results plots (for one column)
    make.results.plots <- function(one_metric, space_results, overal_range, col, main, text.in.cell) {
        ## Initial parameters
        is.last <- FALSE
        is.first <- FALSE

        ## Loop through the results of each 
        for(reduce in 2:length(space_results[[1]][[1]][[1]])) {
            ## Update if last or first
            is.last <- ifelse(reduce == length(space_results[[1]][[1]][[1]]), TRUE, FALSE)
            is.first <- ifelse(reduce == 2, TRUE, FALSE)

            ## Empty plot
            empty.plot(space_results, overal_range, is.last, is.first, main)

            ## Get the y_values
            y_vals <- seq(from = 1, to = 2, length.out = length(space_results))

            # Add lines
            for(one_space in 1:length(space_results)) {
                add.lines.points(one_space, space_results, one_metric, reduce, col, y.val = y_vals[one_space], text.in.cell = ifelse((text.in.cell && reduce == 2), TRUE, FALSE))
            }
        }
    }

    ## Get the reductions (rows)
    reductions <- make.reductions(remove = remove, reduce.distribution = reduce.distribution)

    ## Make the plot layout
    make.plot.window(rows = reductions, cols = space_results)

    ## Plotting the space transformations in the following cells
    silent <- lapply(as.list(1:length(reductions$reductions)), plot.reduction, reductions = reductions, defaults = defaults)

    ## Get the overall range
    all_values <- unlist(space_results)
    if(any(all_values[-which(is.na(all_values))] == Inf)) {
        all_values[-which(all_values == Inf)]
    }
    overal_range <- round(range(all_values, na.rm = TRUE), digits = 1)

    ## Plot all results
    for(one_metric in 1:length(space_results[[1]][[1]])) {
        make.results.plots(one_metric, space_results, overal_range, col, main = metrics_names[one_metric], text.in.cell = ifelse(text.in.cell, ifelse(one_metric == 1, TRUE, FALSE), FALSE))
    }
    return(invisible())
}
