## Minimal spanning tree distances evenness
func.eve <- function(matrix) {
    ## Distance matrix
    distances <- dist(matrix)
    ## weighted evenness (EW) for equal weighted species
    branch_lengths <- (distances/2)[which(as.dist(ape::mst(distances)) != 0)]
    ## partial weighted evenness (PEW)
    rel_br_lentghs <- branch_lengths/sum(branch_lengths)
    ## Regular abundance value (1/(S-1))
    regular <- 1/(nrow(matrix) - 1)
    ## Get the minimal distances
    min_distances <- sapply(rel_br_lentghs, function(x, y) min(c(x, y)), y = regular)
    ## Return the Functional eveness
    return((sum(min_distances) - regular) / (1 - regular))
}

## Distance from centroid deviation ratio
func.div <- function(matrix) {
    ## The distance from centroid (dGi)
    dist_centroid <- centroids(matrix)
    ## The mean distance from centroid (dG)
    mean_dis_cent <- mean(dist_centroid)
    ## The number of observations
    obs <- length(dist_centroid)
    ## The FDiv metric
    return((sum(dist_centroid) - mean_dis_cent * (obs-1)) / ((sum(abs(dist_centroid - mean_dis_cent) + dist_centroid))/obs))
}






test_that("ecology.metric", {
    set.seed(1)
    dummy_matrix <- matrix(rnorm(25),5,5, dimnames = list(letters[1:5]))
    x <- dist(dummy_matrix)
    x_pco <- cmdscale(as.matrix(x), k = ncol(as.matrix(x))-1)

    library(FD)
    FD_test <- dbFD(x)

    ## Convex Hull volume
    expect_equal(geometry::convhulln(x_pco, "FA")$vol, FD_test$FRic)

    ## Regularity of distances on the minimum spanning tree (Villéger et al. 2008)
    expect_equal(func.eve(x_pco), FD_test$FEve)

    ## Distance from centroid deviation ratio (Villéger et al. 2008)
    expect_equal(func.div(x_pco), FD_test$FDiv)

})


FDis <- FD_test$FDis # Laliberté and Legendre 2010 - Functional dispersion = Average distance to centroid (if all species have the same abundance) =  multivariate dispersion - Anderson (2006))
# mean(centroids(as.matrix(x)))
func.disp <- function(matrix){unname(FD::fdisp(dist(matrix))$FDis)}


mean(centroids(as.matrix(x)))









RaoQ <- FD_test$RaoQ # Rao's quadratic entropy (Q) - Botta-Dukát (2005)
ade4::divc(data.frame(matrix(1, ncol = 1, nrow = nrow(as.matrix(x)), dimnames = list(c(rownames(as.matrix(x)))))),
            x, scale = FALSE)


# # Villéger, S., N. W. H. Mason and D. Mouillot (2008) New multidimensional functional diversity indices for a multifaceted framework in functional ecology. Ecology 89:2290-2301.
# # Legendre, P. and L. Legendre (1998) Numerical Ecology. 2nd English edition. Amsterdam: Elsevier.
# # Laliberté, E. and P. Legendre (2010) A distance-based framework for measuring functional diversity from multiple traits. Ecology 91:299-305.
# # Anderson, M. J. (2006) Distance-based tests for homogeneity of multivariate dispersions. Biometrics 62:245-253.



