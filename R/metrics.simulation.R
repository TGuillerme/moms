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
#' metrics <- list(c(sum, variances), c(sum, ranges))
#' 
#' ## Apply the metrics
#' metrics.simulation(space, groups, metrics)
#' 
#' @seealso space.simulation shift.group.simulation
#' 
#' @author Thomas Guillerme
#' @export

metrics.simulation <- function(space, reduction, metrics, rare.dim = NULL, verbose = TRUE) {

    if(is.null(rare.dim)) {
        rare.dim <- dim(space[[1]])[2]
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