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
                       "span.tree"  = span.tree.length,
                       "ave.neigh"  = c(mean, neighbours),
                       "av.displa"  = c(mean, displacements),
                       "median.var" = function(matrix) median(abs(apply(var(matrix), 2, sum))))

metric_names <- c("Functional dispersion",
                  "Sum of variances",
                  "Ellipsoid volume",
                  "Minimum span tree length",
                  "Average nearest neighbour distance",
                  "Median sum of the var-covar.")


## All metrics (exploration)
# metrics_list <- list("av.pairwise" = function(matrix) return(mean(pairwise.dist(matrix)^2)),
#                     "Procrustes" = function(matrix) return(sum(matrix^2)/nrow(matrix)),
#                     "Volume" = ellipse.volume,
#                     # "PoR" = c(prod, ranges),
#                     # "SoR" = c(sum, ranges),
#                     # "PoV" = c(prod, variances),
#                     "SoV" = c(sum, variances),
#                     # "PoQ" = c(prod, quantiles),
#                     # "SoQ" = c(sum, quantiles),
#                     "av.displac" = c(mean, displacements),
#                     "av.neighbo" = c(mean, neighbours))

# metric_names <- c("Average squared\ndistance", "Procrustes\nvariance", "Ellipsoid\nvolume", 
#                   "Sum of\nVariances", "Mean\ndisplacement", "Mean nearest\nneighbours distance")

## Shorter vector name version (for clarity in the fable)
# name <- gsub("\n", " ", metric_names)
#"Product of Quantiles", "Sum of Quantiles", 
#"Product of\nRange", "Sum of\nRanges", "Product of\nVariances",