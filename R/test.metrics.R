#' @title Test metrics
#'
#' @description Applies an test to the results lists
#'
#' @param results_list the list of results
#' @param test the test to apply
#' @param scale whether to scale and centre the results around the non-reduced spaces
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export


# testing.fun <- function(data) {return(aov(glm(disparity ~ factor, data = data)))}

test.metrics <- function(all_results, test, scale = TRUE) {

    ## Get the number of metrics
    factors <- names(all_results)

    ## Get the metric names
    metrics_names <- names(all_results[[1]])

    ## Get the results for each metric
    get.results <- function(metric, all_results) {
        return(extract.results(all_results, metric = metric, element = "all", transfo = "all"))
    }
    results_per_metric <- lapply(as.list(metrics_names), get.results, all_results)

    if(scale){
        ## Centre the results
        centre.fun <- function(result) {
            output <- apply(result, 2, function(X) as.numeric(X) - as.numeric(X)[1])
            output <- apply(output, 2, function(X) X/(max(abs(X))))
            output <- output[-1, ]
            rownames(output) <- rownames(result)[-1]
            return(output)
        }
        results_per_metric <- lapply(results_per_metric, lapply, centre.fun)
    }

    ## Transform the results into tables for glm
    convert.table <- function(metric_results) {
        table <- data.frame(lapply(metric_results, c))
        return(data.frame("disparity" = unname(unlist(table)), "factor" = rep(names(table), each = nrow(table))))
    }

    ## List of tables per metrics
    tables <- lapply(results_per_metric, convert.table)

    ## Run the test
    test_results <- lapply(tables, test)
    
    ## Summarising the tests
    summarised_results <- lapply(test_results, summary)
    names(summarised_results) <- metrics_names

    return(summarised_results)
}