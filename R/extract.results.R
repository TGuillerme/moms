#' @title Extract results
#'
#' @description Extract specific results from the simulation lists
#' 
#' @param all_results the output from a simulation bundle
#' @param metric which metric to extract
#' @param element which element to extract (e.g. number of dimensions, type of space, etc.) can be "all"
#' @param transfo which transformation to extract (e.g. random, density, etc.) can be "all"
#' 
#' @author Thomas Guillerme

extract.results <- function(all_results, metric, element = "all", transfo = "all") {

    ## Get the metric number to extract
    metric_n <- match(metric, names(all_results[[1]]))
    if(is.na(metric_n)) stop(paste0("metric name ", metric, " not found."))

    ## Extract the metric
    results_metric <- lapply(all_results, function(X, metric_n) return(X[[metric_n]]), metric_n)
    
    ## Get the element to extract
    if(element == "all") {
        element <- names(results_metric)
    }
    element_n <- match(element, names(all_results))
    if(any(is.na(element_n))) stop(paste0("One element name not found (", paste0(element, collapse = ", ") ,")."))
    
    ## Extract the element
    results_element <- results_metric[element_n]

    ## Get the transformation to extract
    if(transfo == "all") {
        transfo <- rownames(results_element[[1]])
    }
    transfo_n <- match(transfo, rownames(results_element[[1]]))
    if(any(is.na(transfo_n))) stop(paste0("One transformation name not found (", paste0(transfo, collapse = ", ") ,")."))

    ## Extract the transformation
    results_transfo <- lapply(results_element, function(X, transfo_n) return(X[transfo_n,]), transfo_n)

    return(results_transfo)
}