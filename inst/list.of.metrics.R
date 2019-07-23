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

## Function dispersion (should be mean distance from centroid)
func.disp <- function(matrix) {
    if(is.null(rownames(matrix))) {
        rownames(matrix) <- 1:nrow(matrix)
    }
    distance <- dist(matrix)
    return(unname(FD::fdisp(distance)$FDis))
}

## Function for convex hull
convhull <- function(matrix) {
    if(ncol(matrix) > 10) {
        return(0)
    } else {
        return(geometry::convhulln(matrix, "FA")$vol)
    }
}


## Small metrics list (for testing)
## Selected metrics
metrics_list    <- list("func.disp" = func.disp,
                       "sum.var"    = c(sum, variances),
                       "ellips.vol" = ellipse.volume,
                       "span.tree"  = function(matrix) span.tree.length(matrix)/nrow(matrix),
                       "ave.neigh"  = c(mean, neighbours),
                       "av.displa"  = c(mean, displacements),
                       "var.centre" = function(matrix) var(centroids(matrix, centroid = 0)))

metric_names <- c("Average distance from centroid",
                  "Sum of variances",
                  "Ellipsoid volume",
                  "Minimum spanning tree average distance",
                  "Average nearest neighbour distance",
                  "Average displacements",
                  "Variance of the distance from centre")