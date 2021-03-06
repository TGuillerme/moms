#' @title Reduce space
#'
#' @description Remove elements from a multidimensional space
#'
#' @param space the trait space
#' @param type how to reduce the space (either \code{"random"}, \code{"limit"}, \code{"displacement"} or \code{"density"})
#' @param remove the proportion of elements to be removed (in probability)
#' @param parameters the parameter(s) for removal selection (see details). If left empty, the \code{parameters} is estimated to reach the amount set by \code{remove}.
#' @param tuning Optinal parameters for tuning the parameter estimations (if remove is required and parameters is missing) a list of three parameters: "max" for the maximum of operations, "tol" for the tuning (e.g. 0.1 close), "inc.steps" for the initial increment value during optimisation (default = 2 - the bigger the value, the slower the increment).
#' @param verbose wether to be verbose or not
#' @param return.optim logical, whether to also return the optimal value.
#' 
#' @details
#' - \code{limit.removal parameters}: a list of \code{parameters$centre}, the centre from which to count the radius (if missing, is set to \code{0}); and \code{parameters$radius}, the radius for removal.
#' 
#' - \code{displacement.removal parameters}: a list of \code{parameters$value}, value the threshold value from which to remove elements.
#' 
#' - \code{density.removal parameters}: a list of \code{parameters$what} "close" (default) for close neighbours or "distant" for distant ones; \code{parameters$diameter} the diameter for considering closeness or distance; \code{parameters$output} either "singles" or "pairs" to return the pairs of neighbours or one of them only (the first).
#' 
#' @examples
#' set.seed(1)
#' ## Creating a two dimensional space
#' space <- dispRity::space.maker(100, 2, distribution = stats::rnorm)
#' 
#' ## Generating the four types of reductions
#' random <- reduce.space(space, "random", remove = 0.5)
#' limit <- reduce.space(space, "limit", remove = 0.5)
#' displacement <- reduce.space(space, "displacement", remove = 0.5)
#' density <- reduce.space(space, "density", remove = 0.5)
#' 
#' ## Plotting the four different results
#' par(mfrow = c(2,2))
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(random)],
#'      main = "Random removal") 
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(limit)],
#'      main = "Limit removal")
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(displacement)],
#'      main = "Displacement removal")
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(density)],
#'      main = "Density removal")
#' 
#' 
#' @author Thomas Guillerme

reduce.space <- function(space, type, remove, parameters, tuning, verbose = FALSE, return.optim = FALSE) {

    ## Add sanitizing
    type_available <- c("random", "limit", "displacement", "density", "evenness")
    ## Tolerance
    if(missing(tuning)) {
        tuning <- list()
    }
    if(is.null(tuning$max)) {
        tuning$max <- 100
    }
    if(is.null(tuning$tol)) {
        tuning$tol <- 0.01
    }
    if(is.null(tuning$inc.steps)) {
        tuning$inc.steps <- 2
    }

    ## Complex removals
    if(missing(parameters)) {
        parameters <- list()
    }


    switch(type,
        random = {
            ## Number of elements
            elements <- nrow(space)
            ## Return a portion of the space
            to_remove <- sample(1:elements, elements*remove)
            return(1:elements %in% to_remove)
        },
        limit = {
            ## Type function
            fun <- run.limit.removal
            ## Parameters
            if(is.null(parameters$centre)) {
                parameters$centre <- apply(space, 2, mean)
            } 
            if(is.null(parameters$radius)) {
                parameters$radius <- 1
            }
            ## Parameter to optimise
            parameters$optimise <- parameters$radius
            ## List of arguments
            args <- list("space" = space, "parameters" = parameters)
        },
        displacement = {
            ## Type function
            fun <- run.limit.removal
            ## Parameters
            if(is.null(parameters$centre)) {
                parameters$centre <- apply(space, 2, max)
            } 
            if(is.null(parameters$radius)) {
                parameters$radius <- 1
            }
            ## Parameter to optimise
            parameters$optimise <- parameters$radius
            ## List of arguments
            args <- list("space" = space, "parameters" = parameters)
        },
        density = {
            ## Type function
            fun <- run.density.removal
            ## Parameters
            if(is.null(parameters$distance)) {
                parameters$distance <- as.matrix(dist(space))
            }
            if(is.null(parameters$diameter)) {
                parameters$diameter <- 0.5
            }  
            ## Parameter to optimise
            parameters$optimise <- parameters$diameter
        },
        evenness = {
            ## Parameters
            if(is.null(parameters$bw)) {
                parameters$bw <- bw.nrd0
            }
            ## Make the probability vector
            prob_vector <- get.prob.vector(space, bw = parameters$bw)

            ## Number of elements
            elements <- nrow(space)

            ## Return a portion of the space
            to_remove <- sample(1:elements, elements*remove,
                                prob = prob_vector)
            return(1:elements %in% to_remove)
        }
    )

    ## List of arguments
    args <- list("space" = space, "parameters" = parameters)
    ## Run the complex removal
    to_remove <- do.call(fun, args)

    ## Optimise the function (if necessary)
    if(!missing(remove)) {

        ## Get out of the corner case of all being TRUE or FALSE
        if(all(to_remove) || all(!to_remove)) {
            args$parameters$optimise <- runif(1)
            to_remove <- do.call(fun, args)
        }

        ## Optimise
        to_remove <- optimise.results(to_remove, fun = fun, remove = remove, args = args, tuning = tuning, verbose = verbose, space = space, return.optim = return.optim)
    }

    if(!return.optim) {
        return(to_remove)
    } else {
        return(list(remove = to_remove$remove, optim = to_remove$optim))
    }
}
