#' @title Summarise metrics
#'
#' @description Summarises the metric simulations the results for plotting and tables
#'
#' @param results_list the list of results
#' @param cent.tend the central tendency to use
#' @param quantiles the quantiles to display
#' @param sale whether to center and scale the results
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

summarise.metrics <- function(results_list, cent.tend = median, quantiles = c(95, 50), scale = TRUE){

    ## Converts one or more CI into quantile probabilities
    CI.converter <- function(CI) {
        sort(c(50-CI/2, 50+CI/2)/100)
    }

    ## Scaling the results (baseline being "all")
    scale.metric <- function(one_metric) {
        ## Get the centre (the observed disparity in for the full morphospace)
        centered <- apply(one_metric, 2, function(X) as.numeric(X) - as.numeric(X)[1])
        scaled <- apply(centered, 2, function(X) X/(max(abs(X))))
        return(scaled)
    }

    ## Scaling
    if(scale){
        results_list <- lapply(results_list, scale.metric)
    }

    ## Getting the central tendency and CIs
    central_tendency <- lapply(results_list, function(x) apply(x, 1, cent.tend))
    CIs <- lapply(results_list, function(x) apply(x, 1, quantile, prob = CI.converter(quantiles)))

    return(list("cent.tend" = central_tendency, "CIs" = CIs))
}