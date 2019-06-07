## Runs multiple similar simulations
## Parameters are in the function header
simulation.spaces <- function(remove, replicates, metrics_list) {
  
    elements <- 300
    dimensions <- 50
    normal_scree <- rev(cumsum(rep(1/dimensions, dimensions)))
    lognormal_scree <-  c(1, cumprod(rep(1/dimensions*10, dimensions)))[-dimensions+1]
    centre_runif <- list(list("min" = -0.5, "max" = 0.5))
    centre_rnorm <- list(list("mean" = 0, "sd" = 1))

    d2_uniform <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = 2,
                                  distributions = runif,
                                  arguments = centre_runif,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = NULL,
                                  cor.matrix = NULL)
    d50_uniform <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = runif,
                                  arguments = centre_runif,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = NULL,
                                  cor.matrix = NULL)
    d50_unif_cor <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = runif,
                                  arguments = centre_runif,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = NULL,
                                  cor.matrix = "random")
    d2_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = 2,
                                  distributions = rnorm,
                                  arguments = NULL,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = NULL,
                                  cor.matrix = NULL)
    d50_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = NULL,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = NULL,
                                  cor.matrix = NULL)
    d50_norm_cor <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = NULL,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = NULL,
                                  cor.matrix = "random")
    d50_random <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = "random",
                                  arguments = centre_rnorm,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = lognormal_scree,
                                  cor.matrix = NULL)
    pco_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = NULL,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = normal_scree,
                                  cor.matrix = NULL)
    pca_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = NULL,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE,
                                  scree = lognormal_scree,
                                  cor.matrix = NULL)

    return(list("d2_unif" = d2_uniform,
                "d50_uni" = d50_uniform,
                "d50_uni_c" = d50_unif_cor,
                "d2_norm" = d2_normal,
                "d50_nor" = d50_normal,
                "d50_nor_c" = d50_norm_cor,
                "d50_rand" = d50_random,
                "pco_nor" = pco_normal,
                "pca_nor" = pca_normal))
}


## Running simulations for one space with rarefaction
simulation.rarefaction <- function(remove, replicates, metrics_list, what) {
  
    elements <- 300
    dimensions <- 50
    distributions <- rnorm
    normal_scree <- rev(cumsum(rep(1/dimensions, dimensions)))
    
    ## Getting the arguments list
    simulate_arguments <- list(replicates = replicates,
                               elements = elements,
                               dimensions = dimensions,
                               distributions = distributions,
                               arguments = NULL,
                               remove = remove,
                               metrics_list = metrics_list,
                               verbose = TRUE,
                               scree = normal_scree,
                               cor.matrix = NULL)

    if(what == "dimensions") {
        ## Lapply wrapper for the rarefactions
        run.rarefactions <- function(rarefaction, simulate_arguments) {
            ## Update the arguments list
            simulate_arguments$rare.dim <- rarefaction
            ## Return the results
            return(do.call(simulate.metrics, simulate_arguments))
        }
        
        ## Get the rarefaction numbers
        rarefaction <- round(seq(from = 2, to = dimensions, length.out = 10))
    }

    if(what == "elements") {
        ## Lapply wrapper for the rarefactions
        run.rarefactions <- function(rarefaction, simulate_arguments) {
            ## Update the arguments list
            simulate_arguments$elements <- rarefaction
            ## Return the results
            return(do.call(simulate.metrics, simulate_arguments))
        }
        
        ## Get the rarefaction numbers
        rarefaction <- round(seq(from = elements*0.1, to = elements, length.out = 10))
    }

    ## Run the rarefactions
    rars <- lapply(as.list(rarefaction), run.rarefactions, simulate_arguments)
    names(rars) <- as.character(rarefaction)
    return(rars)
}


## Save/load simulation results
save.results <- function(results, path = "../data/processed/") {
    match_call <- match.call()
    save(results, file = paste0(path, as.character(match_call$results), ".Rda"))
}

load.results <- function(results, path = "../data/processed/") {
    load(paste0(path, results, ".Rda"))
    output <- results
    return(output)
}
