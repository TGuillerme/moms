#' @title Reduce space
#'
#' @description Remove elements from a multidimensional space
#'
#' @param space the trait space
#' @param type how to reduce the space (either \code{"random"}, \code{"limit"}, \code{"displacement"} or \code{"density"})
#' @param remove the proportion of elements to be removed (in probability)
#' @param parameters the parameter(s) for removal selection (see details). If left empty, the \code{parameters} is estimated to reach the amount set by \code{remove}.
#' @param tuning Optinal parameters for tuning the parameter estimations (if remove is required and parameters is missing) a list of three parameters: "max" for the maximum of operations, "tol" for the tuning (e.g. 0.1 close) "good" for when to decide it's good enough (i.e. stop if it reaches the tuning after X number of times).
#' @param verbose
#' 
#' @details
#' - \code{limit.removal parameters}: a list of \code{parameters$centre}, the centre from which to count the radius (if missing, is set to \code{0}); and \code{parameters$radius}, the radius for removal.
#' 
#' - \code{displacement.removal parameters}: a list of \code{parameters$value}, value the threshold value from which to remove elements.
#' 
#' - \code{density.removal parameters}: a list of \code{parameters$what} "close" (default) for close neighbours or "distant" for distant ones; \code{parameters$diameter} the diameter for considering closeness or distance; \code{parameters$output} either "singles" or "pairs" to return the pairs of neighbours or one of them only (the first).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme

reduce.space <- function(space, type, remove, parameters, tuning, verbose = FALSE) {

    ## Add sanitizing
    type_available <- c("random", "limit", "displacement", "density")   

    ## Simple removal (simple)
    if(type == "random") {
        ## Number of elements
        elements <- nrow(space)
        ## Return a portion of the space
        to_remove <- sample(1:elements, elements*remove)
        return(1:elements %in% to_remove)
    }

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

    ## Set parameters for specific cases
    if(type == "limit") {
        ## Type function
        fun <- run.limit.removal
        ## Parameters
        if(is.null(parameters$centre)) {
            parameters$centre <- rep(0, ncol(space))
        } 
        if(is.null(parameters$radius)) {
            parameters$radius <- 1
        }
        ## Parameter to optimise
        parameters$optimise <- parameters$radius
        ## List of arguments
        args <- list("space" = space, "parameters" = parameters)
    }

    if(type == "displacement") {
        ## Type function
        fun <- run.displacement.removal
        ## Parameters
        if(is.null(parameters$value)) {
            parameters$value <- 1
        } 
        ## Parameter to optimise
        parameters$optimise <- parameters$value
    }

    if(type == "density") {
        ## Type function
        fun <- run.density.removal
        ## Parameters
        if(is.null(parameters$distance)) {
            parameters$distance <- as.matrix(dist(space))
        }
        if(is.null(parameters$diameter)) {
            parameters$diameter <- 0.5
        }  
        ## Parameter to optimise
        parameters$optimise <- parameters$diameter
    }

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
        to_remove <- optimise.results(to_remove, fun = fun, remove = remove, args = args, tuning = tuning, verbose = verbose, space = space)
    }

    return(to_remove)
}





# #' @export
# random.removal <- function(space, remove, parameters, tuning) {
#     ## Count the elements
#     elements <- nrow(space)
#     ## Return a portion of the space
#     to_remove <- sample(1:elements, elements*remove)
#     return(1:elements %in% to_remove)
# }

# #' @export
# limit.removal <- function(space, remove, parameters, tuning) {    
    
#     if(missing(parameters)) {
#         parameters <- list()
#     }

#     ## Set the default centre
#     if(is.null(parameters$centre)) {
#         parameters$centre <- rep(0, ncol(space))
#     } 

#     ## Set the default centre
#     if(is.null(parameters$radius)) {
#         ## Default radius (will be optimised)
#         parameters$radius <- 1
#     } 


#     ## Set the arguments
#     args <- list("space" = space, "parameters" = parameters)
#     args$parameters$optimise <- parameters$radius

#     ## Select the bits to remove
#     run.limit.removal <- function(space, parameters) {
#         apply(space, 1, point.in.circle, centre = parameters$centre, radius = parameters$optimise)
#     }

#     ## Run the function
#     to_remove <- do.call(run.limit.removal, args)
#     ## Optimise the function (if necessary)
#     to_remove <- optimise.results(to_remove, fun = run.limit.removal, remove = remove, args = args, tuning = tuning, verbose = TRUE, space = space)

#     return(to_remove)
# }

# #' @export
# displacement.removal <- function(space, remove, parameters, tuning) {    
   
#     if(missing(parameters)) {
#         parameters <- list()
#     }


#     ## Set the default centre
#     if(is.null(parameters$value)) {
#         ## Default value (will be optimised)
#         parameters$value <- 1
#     } 

#     ## Set the arguments
#     args <- list("space" = space, "parameters" = parameters)
#     args$parameters$optimise <- parameters$value

#     ## Select the bits to remove
#     run.displacement.removal <- function(space, parameters) {
#         apply(space, 1, select.value, value = parameters$optimise)
#     }

#     ## Run the function
#     to_remove <- do.call(run.displacement.removal, args)
#     ## Optimise the function (if necessary)
#     to_remove <- optimise.results(to_remove, fun = run.displacement.removal, remove = remove, args = args, tuning = tuning, verbose = TRUE, space = space)

#     return(to_remove)
# }


# #' @export
# density.removal <- function(space, remove, parameters, tuning) {  
    
#     if(missing(parameters)) {
#         parameters <- list()
#     }  

#     ## Set the default what
#     if(is.null(parameters$what)) {
#         parameters$what <- "close"
#     }
#     ## Set the default output
#     if(is.null(parameters$output)) {
#         parameters$output <- "pairs"
#     }
#     ## Set the default centre
#     if(is.null(parameters$diameter)) {
#         ## Default value (will be optimised)
#         parameters$diameter <- 1
#     } 

#     ## Set the arguments
#     args <- list("space" = space, "parameters" = parameters)
#     args$parameters$optimise <- parameters$diameter

#     ## Select the bits to remove
#     run.density.removal <- function(space, parameters) {
#         close_neigbhours <- get.neigbhours(space, what = parameters$what, diameter = parameters$optimise, output = parameters$output)
#         return(1:nrow(space) %in% close_neigbhours)
#     }

#     ## Run the function
#     to_remove <- do.call(run.density.removal, args)
#     ## Optimise the function (if necessary)
#     to_remove <- optimise.results(to_remove, fun = run.density.removal, remove = remove, args = args, tuning = tuning, verbose = TRUE, space = space)

#     return(to_remove)
# }