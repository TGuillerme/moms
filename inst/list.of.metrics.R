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
                        "sum.var"   = c(sum, variances),
                        "sum.range" = c(sum, ranges),
                        "ellips.vol"= ellipse.volume,
                        "span.tree" = c(mean, span.tree.length),
                        "func.eve"  = func.eve,
                        "ave.neigh" = c(mean, neighbours),
                        "av.displa" = c(mean, displacements))

metric_names <- c("Average distance from centroid",
                  "Sum of variances",
                  "Sum of ranges",
                  "Ellipsoid volume",
                  "Minimum spanning tree average distance",
                  "Minimum spanning tree distances evenness",
                  "Average nearest neighbour distance",
                  "Average displacements")