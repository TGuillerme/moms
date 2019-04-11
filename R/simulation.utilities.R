## Runs multiple similar simulations
## Parameters are in the function header
simulation.bundle <- function(remove, replicates, metrics_list) {
  
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
                                  verbose = TRUE, scree = NULL, cor.matrix = NULL)
    d50_uniform <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = runif,
                                  arguments = centre_runif,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE, scree = NULL, cor.matrix = NULL)
    d2_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = 2,
                                  distributions = rnorm,
                                  arguments = centre_rnorm,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE, scree = NULL, cor.matrix = NULL)
    d50_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = centre_rnorm,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE, scree = NULL, cor.matrix = NULL)
    pco_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = centre_rnorm,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE, scree = normal_scree, cor.matrix = NULL)
    pca_normal <- simulate.metrics(replicates = replicates,
                                  elements = elements,
                                  dimensions = dimensions,
                                  distributions = rnorm,
                                  arguments = centre_rnorm,
                                  remove = remove,
                                  metrics_list = metrics_list,
                                  verbose = TRUE, scree = lognormal_scree, cor.matrix = NULL)
    
    return(list("d2_unif" = d2_uniform,
                "d50_uni" = d50_uniform,
                "d2_norm" = d2_normal,
                "d50_nor" = d50_normal,
                "pco_nor" = pco_normal,
                "pca_nor" = pca_normal))
}
