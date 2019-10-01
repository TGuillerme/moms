# #' @title shinyMatrix utilities
# #'
# #' @description Alias functions for shinyMatrix
# #'
# #' @param x an input
# #' @param type either \code{"input"} or \code{"update"}
# #' 
# #' @author Thomas Guillerme

# shinyMatrix.alias <- function(x, type = "input") {
#     if(type == "input") {
#         out <- shinyMatrix::matrixInput(x)
#     } else {
#         out <- shinyMatrix::updateMatrixInput(x)
#     }
#     return(out)
# }
