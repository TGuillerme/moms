#' @title Simulate metrics
#'
#' @description Run the metrics simulations
#'
#' @param replicates number of simulations
#' @param elements number of elements
#' @param dimensions number of dimensions
#' @param distributions the distribution of the space
#' @param remove the percentage of elements to remove
#' @param metrics_list the list of metrics
#' @param verbose 
#' @param scree variance per axis (see dispRity::space.maker)
#' @param cor correlation between axis (see dispRity::space.maker)
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

simulate.metrics <- function(replicates, elements, dimensions, distributions, remove, metrics_list, verbose = FALSE, scree = NULL, cor.matrix = NULL) {

    if(verbose) cat(paste0("Running ", replicates, " replicates:"))

    ## Run one simulation
    one.simulation <- function(elements, dimensions, distributions, remove, metrics_list, verbose, scree, cor.matrix) {

        ## Simulate the space
        space <- space.maker(elements, dimensions, distribution = distributions, scree = scree, cor.matrix = cor.matrix)
        ## Adding rownames
        rownames(space) <- 1:elements

        ## Removing elements
        random <- reduce.space(space, type = "random", remove = remove)
        limits <- reduce.space(space, type = "limit", remove = remove)
        displa <- reduce.space(space, type = "displacement", remove = remove)
        densit <- reduce.space(space, type = "density", remove = remove)

        ## Making the custom groups list
        custom_groups <- list("all" = rownames(space),
                              "random" = rownames(space)[random],
                              "limits.min" = rownames(space)[limits],
                              "limits.max" = rownames(space)[!limits],
                              "displa.min" = rownames(space)[displa],
                              "displa.max" = rownames(space)[!displa],
                              "densit.min" = rownames(space)[densit],
                              "densit.max" = rownames(space)[!densit])
        ## Coercing into numerics
        custom_groups <- lapply(custom_groups, as.numeric)

        ## Fast disparity calculation
        fast.disparity <- function(metric, space, groups) {
            ## Function for disparity on one group
            group.disparity <- function(group, space, metric) {
                ## Setting up the default args
                args <- list(matrix = space[group,])
                ## Simple level 1 metric
                if(length(metric) == 1) {
                    return(do.call(metric, args))
                } 
                ## Simple level 2 + 1 metric
                if(is.null(names(metric))) {
                    return(metric[[1]](do.call(metric[[2]], args)))
                }
                ## Handle the named arguments
                args <- c(args, metric[-1])

                ## Level 1 metric + args
                if(length(metric[[1]]) == 1) {
                    return(do.call(metric[[1]], args))
                } 
                ## Level 2 + 1 metric + args
                return(metric[[1]][[1]](do.call(metric[[1]][[2]], args)))
            }
            return(matrix(unlist(lapply(groups, group.disparity, space, metric)), ncol = 1, dimnames = list(names(groups))))
        }
        if(verbose) cat(".")
        return(lapply(metrics_list, fast.disparity, space = space, groups = custom_groups))
    }

    ## Run all simulations
    disparity_results <- replicate(replicates, one.simulation(elements, dimensions, distributions, remove, metrics_list, verbose, scree, cor.matrix), simplify = FALSE)

    ## Merging into a single list
    for(list in 2:replicates) {
        disparity_results[[1]] <- mapply(cbind, disparity_results[[1]], disparity_results[[list]], SIMPLIFY = FALSE)
    }

    if(verbose) cat("Done\n")

    return(disparity_results[[1]])
}