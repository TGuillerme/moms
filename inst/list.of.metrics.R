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