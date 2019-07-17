#' @title Fast disparity
#'
#' @description Fast disparity calculations (but less safe than dispRity)
#'
#' @param group a logical vector for grouping
#' @param space a matrix
#' @param metric a metric dispRity style
#' @param rare.dim the number of dimensions to consider
#' 
#' @examples
#' ## A random space
#' space <- matrix(rnorm(25), 5, 5)
#' 
#' ## A metric
#' metric <- c(sum, variances)
#' 
#' ## A group of four observations
#' group <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
#' 
#' ## The disparity
#' fast.disparity(group, space, metric, rare.dim = 5)
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
#' 
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