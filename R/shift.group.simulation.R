#' @title Shift group
#'
#' @description Makes shifted spaces groups for a space
#'
#' @param space a space
#' @param remove percentage to remove
#' @param verbose whether to be verbose or not
#' 
#' @examples
#' ## Generate a space
#' space <- space.simulation(distributions = list(distribution = rnorm),
#'                          dimensions = 2, elements = 20, replicates = 1)[[1]]
#' 
#' ## Make the shifts
#' shift.group.simulation(space, remove = 0.5)
#' 
#' @seealso space.simulation metric.simulation
#' 
#' @author Thomas Guillerme
#' @export
shift.group.simulation <- function(space, remove, verbose = TRUE) {

    ## Double check for full TRUE or full FALSE
    double.check.reduce <- function(reduction, space, type, remove, verbose) {
        if(all(reduction == "TRUE") || all(reduction == "FALSE") || length(which(reduction)) == 1 || length(which(!reduction)) == 1) {
            if(verbose) cat(paste0("Double checking reduction for ", type, ":"))

            ## Counter
            counter <- 1
            while(all(reduction == TRUE) || all(reduction == FALSE) || length(which(reduction)) == 1 || length(which(!reduction)) == 1) {
                if(verbose) cat(".")
                reduction <- reduce.space(space, type = type, remove = remove+sample(c(0.01, -0.01), 1))
                counter <- counter + 1
                if(counter == 100) {
                    sample(c(TRUE, FALSE), nrow(space), replace = TRUE)
                    warning("Impossible to reduce space: reduction is now random")
                    break()
                }
            }
            if(verbose) cat("Done.\n")
        }
        return(reduction)
    }

    ## Removing elements
    random <- reduce.space(space, type = "random", remove = remove)
    random <- double.check.reduce(random, space, type = "random", remove = remove, verbose = TRUE)
    limits <- reduce.space(space, type = "limit", remove = remove)
    limits <- double.check.reduce(limits, space, type = "limit", remove = remove, verbose = TRUE)
    displa <- reduce.space(space, type = "displacement", remove = remove)
    displa <- double.check.reduce(displa, space, type = "displacement", remove = remove, verbose = TRUE)
    densit <- reduce.space(space, type = "density", remove = remove)
    densit <- double.check.reduce(densit, space, type = "density", remove = remove, verbose = TRUE)


    ## Adding rownames (if necessary)
    if(is.null(rownames(space))) {
        rownames(space) <- 1:nrow(space)
    }

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
    return(custom_groups)
}