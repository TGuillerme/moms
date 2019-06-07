#' @title prepare.disparity
#'
#' @description Prepares data for the empirical disparity analysis (from processed data)
#'
#' @param chain the chain name
#' @param path the path where the "matrices/" and "trees/" folders are
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

prepare.disparity <- function(chain, path = "../data/") {

    ## Read the FADLA file
    FADLAD <- read.csv(paste0(path, "FADLAD/", chain, ".csv"), row.names = 1, header = TRUE)

    ## Read the tree file
    tree <- ape::read.nexus(paste0(path, "processed/tree.", chain, ".tre"))

    ## Remove the taxa not present in the tree but in the FADLAD
    absent_in_tree <- dispRity::clean.data(FADLAD, tree)$dropped_rows
    if(!any(is.na(absent_in_tree))) {
        FADLAD <- FADLAD[-match(absent_in_tree, rownames(FADLAD)),]
    }

    ## Scaling with the latest FADLAD
    tree$root.time <- max(dispRity::tree.age(tree)[,1] + min(FADLAD[,2]))

    ## Loading the distance matrices
    matrix_dist <- readRDS(paste0(path, "processed/distance_matrix.", chain ,".Rda"))

    ## Ordinating the matrix
    morphospace <- cmdscale(matrix_dist, k = nrow(matrix_dist) - 2, add = TRUE)$points

    return(list(morphospace = morphospace, tree = tree, FADALD = FADLAD))
}