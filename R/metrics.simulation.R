#' @title Apply metrics
#'
#' @description Apply the metrics to the simulated spaces
#'
#' @param space a list of spaces
#' @param reduction a list of reductions
#' @param metrics a list of metrics
#' @param rare.dim a rarefaction value (optional)
#' @param verbose whether to be verbose or not
#' 
#' @examples
#' ## Generate a space
#' space <- space.simulation(distributions = list(distribution = rnorm),
#'                          dimensions = 2, elements = 20, replicates = 3)
#' 
#' ## Make the shifts
#' groups <- lapply(space, shift.group.simulation, remove = 0.5)
#' 
#' ## List of metrics
#' metrics <- list(c(mean, variances), diag)
#' 
#' ## Apply the metrics
#' metrics.simulations(space, groups, metrics)
#' 
#' @seealso space.simulation shift.group.simulation
#' 
#' @author Thomas Guillerme
#' @export

metrics.simulation <- function(space, reduction, metrics, rare.dim = NULL, verbose = TRUE) {

    if(is.null(rare.dim)) {
        rare.dim <- dim(space[[1]])[2]
    }

    ## Function for fast metric calculations
    fast.disparity <- function(group, space, metric, rare.dim) {
        ## Setting up the default args
        args <- list(matrix = space[group, 1:rare.dim])
        ## Simple level 1 metric
        if(length(metric) == 1) {
            return(do.call(metric, args))
        } 
        ## Simple level 2 + 1 metric
        if(is.null(names(metric))) {
            return(metric[[1]](do.call(metric[[2]], args)))
        }
        ## Handle the named arguments
        args <- c(args, metric[-1])

        ## Level 1 metric + args
        if(length(metric[[1]]) == 1) {
            return(do.call(metric[[1]], args))
        } 
        ## Level 2 + 1 metric + args
        return(metric[[1]][[1]](do.call(metric[[1]][[2]], args)))
    }


    ## Mapply wrapper function
    lapply.metric <- function(one_metric, space, reduction, rare.dim, verbose) {
        lapply.disparity <- function(one_group, one_space, one_metric, rare.dim, verbose) {
            if(verbose) cat(".")
            return(lapply(one_group, fast.disparity, one_space, one_metric, rare.dim))
        }
        return(mapply(lapply.disparity, reduction, space,
                  MoreArgs = list(one_metric = one_metric, rare.dim = rare.dim, verbose = verbose)))
    }

    if(verbose) cat("Applying metric:")
    results <- lapply(metrics, lapply.metric, space, reduction, rare.dim, verbose)
    if(verbose) cat("Done.\n")
    return(results)
}