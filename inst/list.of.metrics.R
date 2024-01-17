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
                        "ellips.vol"= dispRity::ellipsoid.volume,
                        "span.tree" = c(mean, span.tree.length),
                        "func.eve"  = func.eve,
                        "ave.neigh" = c(mean, neighbours),
                        "av.displa" = c(mean, displacements))

metric_names <- c("Average Euclidean distance from centroid",
                  "Sum of variances",
                  "Sum of ranges",
                  "Ellipsoid volume",
                  "Minimum spanning tree average distance",
                  "Minimum spanning tree distances evenness",
                  "Average nearest neighbour distance",
                  "Average displacements")

## Prints the number of metrics in english (single = FALSE) or the list of metrics (single = TRUE)
metric.names <- function(single = FALSE, names = metric_names) {
  english_numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  if(!single) {
    if(length(names) > 10) {
      ## Return the digits
      return(length(names))
    } else {
      ## Return the english number
      return(english_numbers[length(names)])
    }
  } else {
    return(paste0(names, sep = ", "))
  }
}

## Table number