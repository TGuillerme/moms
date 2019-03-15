#' @title Average
#'
#' @description Does some complex math to get an average value
#'
#' @param vector A numeric vector
#' 
#' @return A numeric value
#' 
#' @examples
#' average(rnorm(10))
#' 
#' @author Thomas Guillerme
#' @export
## A really complex function
average <- function(vector) {
    return(sum(vector)/length(vector))
}