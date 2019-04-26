#' @title Fast disparity
#' 
#' @description Fast disparity calculation (skips all the sanitizing and formatting of dispRity)
#' 
#' @param group The elements to select in space
#' @param space The space
#' @param metric The metric (in dispRity format)
#' @param rare.dim The dimensions to select in space
#' 
#' @details Prefer using the proper \code{\link[dispRity]{dispRity}} function.
#' This function is a quick shortcut that can create unwanted problems in disparity pipelines.

fast.disparity <- function(group, space, metric, rare.dim) {
    ## Setting up the default args
    args <- list(matrix = space[group, 1:rare.dim])
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
#' @param rare.dim the number of dimensions to use (not to generate)
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

simulate.metrics <- function(replicates, elements, dimensions, arguments = list(NULL), distributions, remove, metrics_list, verbose = FALSE, scree = NULL, cor.matrix = NULL, rare.dim) {

    if(verbose) cat(paste0("Running ", replicates, " replicates:"))

    if(missing(rare.dim)) {
        rare.dim <- dimensions
    }

    ## Run one simulation
    one.simulation <- function(elements, dimensions, distributions, arguments, remove, metrics_list, verbose, scree, cor.matrix, rare.dim) {

        ## Sort the correlation matrix
        if(!is.null(cor.matrix)) {
            
            if(cor.matrix == "random"){
                ## Set random correlation values
                correlations_values <- round(runif((dimensions*dimensions)/2-(dimensions/2), min = 0.1, max = 0.9), 1)
                cor.matrix <- matrix(1, dimensions, dimensions)
                cor.matrix[upper.tri(cor.matrix)] <- correlations_values
                cor.matrix[lower.tri(cor.matrix)] <- correlations_values
            }

        }

        ## Sort the dimensions (if random)
        if(length(distributions) == 1 && class(distributions) != "function") {
            if(distributions == "random") {
                distributions <- sample(c(rnorm,runif,rlnorm), dimensions, replace = TRUE)
                distribution_names <- lapply(distributions, function(fun) gsub("C_", "", as.character(body(fun)[[2]])))
                ## Set the arguments
                get.args <- function(fun) {
                    switch(fun,
                        runif = {return(list("min" = -0.5, "max" = 0.5))},
                        rnorm = {return(list("mean" = 0, "sd" = 1))},
                        rlnorm = {return(list("meanlog" = 0, "sdlog" = 1))}
                    )
                }
                arguments <- lapply(distribution_names, get.args)
            }
        }

        ## Simulate the space
        if(is.null(arguments[[1]])) {
            space <- space.maker(elements, dimensions, distribution = distributions, scree = scree, cor.matrix = cor.matrix)
        } else {
            space <- space.maker(elements, dimensions, distribution = distributions, arguments = arguments, scree = scree, cor.matrix = cor.matrix)
        }
        ## Adding rownames
        rownames(space) <- 1:elements

        ## Reducing the spaces
        custom_groups <- run.reduce.spaces(space, remove = remove)

        ## Fast disparity calculation
        group.disparity <- function(metric, space, groups, rare.dim) {
            return(matrix(unlist(lapply(groups, fast.disparity, space, metric, rare.dim)), ncol = 1, dimnames = list(names(groups))))
        }

        if(verbose) cat(".")
        
        return(lapply(metrics_list, group.disparity, space = space, groups = custom_groups, rare.dim = rare.dim))
    }

    ## Run all simulations
    disparity_results <- replicate(replicates, one.simulation(elements, dimensions, distributions, arguments = arguments, remove, metrics_list, verbose, scree, cor.matrix, rare.dim = rare.dim), simplify = FALSE)

    ## Merging into a single list
    for(list in 2:replicates) {
        disparity_results[[1]] <- mapply(cbind, disparity_results[[1]], disparity_results[[list]], SIMPLIFY = FALSE)
    }

    if(verbose) cat("Done\n")

    return(disparity_results[[1]])
}

#' @title Reduce spaces
#'
#' @description Run the different space reductions
#'
#' @param space a space
#' @param remove percentage to remove
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

run.reduce.spaces <- function(space, remove) {

    ## Double check for full TRUE or full FALSE
    double.check.reduce <- function(reduction, space, type, remove, verbose) {
        if(all(reduction == "TRUE") || all(reduction == "FALSE") || length(which(reduction)) == 1 || length(which(!reduction)) == 1) {
            if(verbose) cat(paste0("Double checking reduction for ", type, ":"))
            while(all(reduction == TRUE) || all(reduction == FALSE) || length(which(reduction)) == 1 || length(which(!reduction)) == 1) {
                if(verbose) cat(".")
                reduction <- reduce.space(space, type = type, remove = remove+sample(c(0.01, -0.01), 1))
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