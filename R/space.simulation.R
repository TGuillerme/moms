#' @title space.simulation
#'
#' @description Wrapper function for simulating the spaces
#'
#' @param distributions the list of arguments to be passed to \code{\link[dispRity]{space.maker}}
#' @param dimensions number of dimensions
#' @param elements number of elements
#' @param replicates number of simulations
#' 
#' @examples
#' ## Generate 10 normal spaces
#' space.simulation(distributions = list(distribution = rnorm),
#'                  dimensions = 2, elements = 20, replicates = 10)
#'
#' ## Generate 10 normal spaces with a correlation matrix and three different distributions
#' space.simulation(distributions = list(distribution = list(rnorm, runif, rnorm),
#'                                      cor.matrix = matrix(c(1,0,0,0,1,0.5,0,0.5,1), 3, 3)),
#'                  dimensions = 3, elements = 20, replicates = 10)
#' 
#' ## Generate 10 random spaces with random correlations
#' space.simulation(distributions = list(distribution = "random", cor.matrix = "random"),
#'                  dimensions = 3, elements = 20, replicates = 10)
#' 
#' ## Generate 10 random spaces with a normal scree
#' space.simulation(distributions = list(distribution = rnorm, scree = "normal"),
#'                  dimensions = 3, elements = 20, replicates = 10)
#' 
#' @seealso shift.space.simulation metric.simulation
#' 
#' @author Thomas Guillerme

space.simulation <- function(distributions, dimensions, elements, replicates) {

    ## Randomising a correlation matrix
    random.cor.matrix <- function(dimensions) {
        correlations_values <- round(runif((dimensions*dimensions)/2-(dimensions/2), min = 0.1, max = 0.9), 1)
        cor.matrix <- matrix(1, dimensions, dimensions)
        cor.matrix[upper.tri(cor.matrix)] <- correlations_values
        cor.matrix[lower.tri(cor.matrix)] <- correlations_values
        return(cor.matrix)
    }

    ## Random distribution sampler
    random.distribution <- function(dimensions) {
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
        ## Output
        return(list(distributions, arguments))
    }

    ## Make the list of arguments
    list_args <- list(dimensions = dimensions, elements = elements)
    list_args <- c(list_args, distributions)

    ## Check if the correlation matrix has to be randomised
    if(!is.null(list_args$cor.matrix)) {
        if(list_args$cor.matrix[[1]] == "random") {
            ## Make a random correlation
            list_args$cor.matrix <- random.cor.matrix(dimensions)
        }
    }

    ## Check if the correlation matrix has to be randomised
    if(class(list_args$distribution) != "function") {
        if(list_args$distribution[1] == "random") {
            ## Sample random distributions
            rand_distr <- random.distribution(dimensions)
            list_args$distribution <- rand_distr[[1]]
            list_args$arguments <- rand_distr[[2]]
        }
    }

    ## Check the scree argument
    if(!is.null(list_args$scree)) {
        switch(list_args$scree,
               normal = {scree <- rev(cumsum(rep(1/dimensions, dimensions)))},
               lognormal = {scree <- cumprod(rep(1/2, dimensions))}
               )
        list_args$scree <- scree
    }

    ## Get all the spaces
    return(replicate(replicates, space.maker(list_args$elements, list_args$dimensions, list_args$distribution, list_args$arguments, list_args$cor.matrix, list_args$scree), simplify = FALSE))
}