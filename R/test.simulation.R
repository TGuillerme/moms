#' @title Test metrics
#'
#' @description Applies an test to the results lists
#'
#' @param results the list of results
#' @param test the test to apply
#' @param scale whether to scale and centre the results around the non-reduced spaces
#' @param factors optional, a list of factors (space names) to test
#' 
#' @examples
#' ## Generate two spaces
#' space1 <- space.simulation(distributions = list(distribution = rnorm),
#'                            dimensions = 2, elements = 20, replicates = 3)
#' space2 <- space.simulation(distributions = list(distribution = runif),
#'                            dimensions = 2, elements = 20, replicates = 3)
#' space3 <- space.simulation(distributions = list(distribution = rlnorm),
#'                            dimensions = 2, elements = 20, replicates = 3)
#' 
#' spaces <- list("normal" = space1, "uniform" = space2, "lognorm" = space3)
#' 
#' ## Make the shifts
#' groups <- lapply(spaces, lapply, shift.group.simulation, remove = 0.4)
#' 
#' ## List of metrics
#' metrics <- list("sum.var" = c(sum, variances),
#'                 "sum.ran" = c(sum, ranges))
#' 
#' ## Apply the metrics
#' results <- mapply(metrics.simulation, spaces, groups,
#'                   MoreArgs = list(metric = metrics), SIMPLIFY = FALSE)
#' 
#' ## Get a simple test function
#' test.fun <- function(data) {return(aov(glm(disparity ~ factor, data = data)))}
#' 
#' ## Apply the test to the whole dataset
#' test.simulation(results, test.fun)
#' 
#' ## Apply the test only to the normal spaces
#' test.simulation(results, test.fun, factors = c("normal", "lognorm"))
#' 
#' 
#' @author Thomas Guillerme

test.simulation <- function(results, test, scale = TRUE, factors) {

    ## Get the metric names
    metrics_names <- names(results[[1]])

    ## Get the results for each metric
    get.results <- function(metric, results) {
        return(extract.results(results, metric = metric, element = "all", transfo = "all"))
    }
    results_per_metric <- lapply(as.list(metrics_names), get.results, results)

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

        ## Check for NaNs
        table_data <- ifelse(is.nan(unname(unlist(table))), 0, unname(unlist(table)))

        return(data.frame("disparity" = table_data, "factor" = rep(names(table), each = nrow(table))))
    }

    ## List of tables per metrics
    tables <- lapply(results_per_metric, convert.table)

    if(!missing(factors)) {
        ## Select only the relevant factors in the tables
        select.factors <- function(table, factors) {
            return(table[(table$factor %in% factors), ])
        }
        tables <- lapply(tables, select.factors, factors)
    }

    ## Run the test
    test_results <- lapply(tables, test)
    
    ## Summarising the tests
    summarised_results <- lapply(test_results, summary)
    
    names(summarised_results) <- metrics_names

    return(summarised_results)
}