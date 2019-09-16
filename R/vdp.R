#' @title Make vdp list
#' 
#' @description Make a volume, density, position combination list
#'
#' @param base.range The base range of the plots (this will be contracted and expanded; default = \code{c(-0.5, 0.5)}).
#' @param extra.points Wether to add any extra pair of points (default = 0).
#' 
#' @examples
#' ## Make a Volume/density/position list
#' vdp_list <- vdp.make()
#' 
#' ## Plotting the transformations
#' vdp.plot(vdp_list)
#' 
#' ## Calculate disparity
#' vdp_disp <- vdp.dispRity(vdp_list, volume = c(sum, variances),
#'                                    density = c(mean, neighbours),
#'                                    position = c(mean, displacements))
#' 
#' ## Plotting the results with disparity
#' vdp.plot(vdp_list, disparity = vdp_disp)
#'
#' @seealso \code{\link{vdp.plot}}, \code{\link{vdp.dispRity}}
#' 
#' @author Thomas Guillerme
#' @export
vdp.make <- function(base.range = c(-0.5, 0.5), extra.points = 0) {

    ## Make the origin space (vdp and fixed)
    outer_edge <- t(data.frame("x" = rep(base.range, 2), "y" = rep(base.range, each = 2)))

    ## Add extra points (if required)
    if(extra.points != 0) {
        ## Add extra points on the outer edge
        extra_points <- runif(extra.points, max = max(base.range), min = min(base.range))
        ## Randomly assign each as a x or y point
        edge.sampler <- function(point, base.range) {
            random_side <- sample(c(TRUE, FALSE), 1)
            if(random_side) {
                return(c(x = sample(base.range, 1), y = point))
            } else {
                return(c(x = point, y = sample(base.range, 1)))
            }
        }
        extra_points <- sapply(extra_points, edge.sampler, base.range = base.range)
        ## Add the points to the origin
        outer_edge <- cbind(outer_edge, extra_points)
    }

    ## Scaling down the outer edge to fit in the middle
    ## Get the quarter range side
    centre <- mean(base.range)
    ## Shrinking/expanding points
    move.points <- function(points, centre, factor) {
        move <- function(point, centre, factor) {
            return(factor * point + (1-factor) * centre)
        }
        return(apply(points, 2, move, factor = factor, centre = centre))
    }

    ## Make the series of spaces
    base <- cbind(outer_edge, move.points(outer_edge, centre, factor = 0.5))

    ## Differences
    vol <- cbind(move.points(outer_edge, factor = 2, centre = centre),
                 move.points(outer_edge, factor = 1.5, centre = centre))
    den <- cbind(outer_edge,
                 move.points(outer_edge, factor = 0.3535, centre = centre))
    pos <- max(base.range) + base
    vol_den <- cbind(move.points(outer_edge, factor = 2, centre = centre),
                     move.points(outer_edge, factor = 0.25, centre = centre))
    vol_pos <- max(base.range) +
               cbind(move.points(outer_edge, factor = 1.5, centre = centre),
                     move.points(outer_edge, factor = 1, centre = centre))
    den_pos <- max(base.range) +
               cbind(outer_edge,
                     move.points(outer_edge, factor = 0.3535, centre = centre)) 
    vol_pos_den <- max(base.range) +
                   cbind(move.points(outer_edge, factor = 0.1, centre = centre),
                         move.points(outer_edge, factor = 0.25, centre = centre))

    ## Return the list of elements
    return(list("base" = base,
                "diff.vol" = vol,
                "diff.den" = den,
                "diff.pos" = pos,
                "diff.vol_den" = vol_den,
                "diff.vol_pos" = vol_pos,
                "diff.den_pos" = den_pos,
                "diff.vol_pos_den" = vol_pos_den
                ))
}


#' @title Make vdp plot
#' 
#' @description Make a volume, density, position combination plot
#'
#' @param vdp A list output from \code{\link{vdp.make}}
#' @param plots Which output from \code{\link{vdp.make}} to plot (default is \code{1:8} for all plots).
#' @param limits Optional, a set of plot limits
#' @param pch The dots type to plot (default = 19 - full round dots)
#' @param xlab, ylab The x and y labels (default is none - \code{""}).
#' @param disparity optional, disparity values obtained from \code{\link{vdp.dispRity}} to be displayed as x labels
#' @param plot.names optional, the plot names (passed as \code{main})
#' @param mfrow optional, the display disposition to be passed to \code{par(mfrow)}. If left empty, the nearest square matrix is used.
#' @param ... any additional argument to be passed to \code{\link[base]{plot}}.
#' 
#' @examples
#' ## Make a Volume/density/position list
#' vdp_list <- vdp.make()
#' 
#' ## Plotting the transformations
#' vdp.plot(vdp_list)
#' 
#' ## Calculate disparity
#' vdp_disp <- vdp.dispRity(vdp_list, volume = c(sum, variances),
#'                                    density = c(mean, neighbours),
#'                                    position = c(mean, displacements))
#' 
#' ## Plotting the results with disparity
#' vdp.plot(vdp_list, disparity = vdp_disp)
#' 
#' @seealso \code{\link{vdp.make}}, \code{\link{vdp.dispRity}}
#' 
#' @author Thomas Guillerme
#' @export
vdp.plot <- function(vdp, plots = 1:8, limits, pch = 19, xlab = "", ylab = "", disparity = NULL, plot.names, mfrow, ...) {

    ## Handle the limits
    if(missing(limits)) {
        limits <- range(unlist(vdp))
    }

    ## Get the disparity values as x labels
    if(!is.null(disparity)) {
        ## Make the base name
        base_name <- c("Vol.=", "Den.=", "Pos.=")
        disparity_lab <- character()
        ## Extract the values for each vdp
        for(val in 1:length(vdp)) {
            values <- round(c(disparity$volume[[val]], disparity$density[[val]], disparity$position[[val]]), digits = 3)
            disparity_lab[[val]] <- paste(paste0(base_name, values), collapse = "; ")
        }
    }

    if(!missing(plot.names)) {
        names(vdp) <- plot.names
    }

    ## Plotting all the 
    if(missing(mfrow)) {
        ## Default (squared) mfrow
        mfrow <- c(ceiling(sqrt(length(plots))),ceiling(sqrt(length(plots))))
    }

    par(mfrow = mfrow, bty = "n", mar = c(4, 3, 3, 1))
    ## Loop through each plot
    plot_name <- 0
    for(one_plot in plots) {
        plot_name <- plot_name + 1
        if(!is.null(disparity)) {
            ## Get the disparity values
            plot(t(vdp[[one_plot]]), main = names(vdp)[[plot_name]],
                 xlim = limits, ylim = limits, pch = pch, xlab = disparity_lab[[one_plot]], ylab = ylab, ...)
        } else {
            plot(t(vdp[[one_plot]]), main = names(vdp)[[plot_name]],
                 xlim = limits, ylim = limits, pch = pch, xlab = xlab, ylab = ylab, ...)
        }
    }
    return(invisible())
}

#' @title disparity vdp
#' 
#' @description Measure disparity on a vdp object
#'
#' @param vdp A list output from \code{\link{vdp.make}}
#' @param volume a volume function as passed to \code{\link[dispRity]{dispRity}}
#' @param density a density function as passed to \code{\link[dispRity]{dispRity}}
#' @param position a position function as passed to \code{\link[dispRity]{dispRity}}
#' @param base.relative whether to make the results relative to the base (\code{TRUE} - default) or absolute (\code{FALSE})
#' 
#' @examples
#' ## Make a Volume/density/position list
#' vdp_list <- vdp.make()
#' 
#' ## Plotting the transformations
#' vdp.plot(vdp_list)
#' 
#' ## Calculate disparity
#' vdp_disp <- vdp.dispRity(vdp_list, volume = c(prod, ranges),
#'                                    density = c(mean, neighbours),
#'                                    position = c(mean, displacements))
#' 
#' ## Plotting the results with disparity
#' vdp.plot(vdp_list, disparity = vdp_disp)
#' 
#' @seealso \code{\link{vdp.make}}, \code{\link{vdp.plot}}
#' 
#' @author Thomas Guillerme
#' @export
vdp.dispRity <- function(vdp, volume, density, position, base.relative = TRUE) {

    ## Creating a list of spaces
    list_spaces <- lapply(vdp, t)

    ## Apply the metrics on each spaces
    list_volume <- lapply(list_spaces, dispRity::dispRity, metric = volume)
    list_densit <- lapply(list_spaces, dispRity::dispRity, metric = density)
    list_positi <- lapply(list_spaces, dispRity::dispRity, metric = position)

    ## Extracting the observed values
    disp_volume <- lapply(list_volume, function(x) summary(x)[,3])
    disp_densit <- lapply(list_densit, function(x) summary(x)[,3])
    disp_positi <- lapply(list_positi, function(x) summary(x)[,3])

    if(base.relative) {
        disp_volume <- lapply(disp_volume, function(x, base) return(x/base), base = disp_volume$base)
        disp_densit <- lapply(disp_densit, function(x, base) return(x/base), base = disp_densit$base)
        disp_positi <- lapply(disp_positi, function(x, base) return(x/base), base = disp_positi$base)
    }

    return(list("volume" = disp_volume,
                "density" = disp_densit,
                "position" = disp_positi))
}

#' @title checking vdp
#' 
#' @description Checking the vdp results in a table
#'
#' @param vdp_disp A list output from \code{\link{vdp.dispRity}}
#' @param vdp_space A list output from \code{\link{vdp.make}}
#' @param round The number of digits to round (default = \code{3})
#' 
#' @examples
#' ## Make a Volume/density/position list
#' vdp_list <- vdp.make()
#' 
#' ## Calculate disparity
#' vdp_disp <- vdp.dispRity(vdp_list, volume = c(prod, ranges),
#'                                    density = c(mean, neighbours),
#'                                    position = c(mean, displacements))
#' 
#' ## Plotting the results with disparity
#' vdp.check.table(vdp_disp, vdp_list)
#' 
#' @seealso \code{\link{vdp.make}}, \code{\link{vdp.dispRity}}
#' 
#' @author Thomas Guillerme
#' @export
vdp.check.table <- function(vdp_disp, vdp_space, round = 3) {
  return(matrix(round(unlist(vdp_disp), round), ncol = 8, byrow = TRUE, dimnames = list(names(vdp_disp), gsub("diff.", "", names(vdp_space)))))
}
