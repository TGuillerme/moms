#' @title Test metrics
#'
#' @description Applies an test to the results lists
#'
#' @param results_list the list of results
#' @param test the test to apply
#' @param relative whether to make the results relative to the non-changed space
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

test.metrics <- function() {



    ## Making the results relative
    relative.metric <- function(one_metric) {
        return(apply(one_metric, 2, function(X) (X/X[1]-1)))
    }

    return()
}