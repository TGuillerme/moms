# ## DEBUG
# stop("DEBUG server")
# input <- list()
# input$space_type <- "Input"
# input$demo_data <- "Beck and Lee 2014"
# input$n_dimensions <- 2
# input$n_elements <- 300
# input$distributions <- "Normal"
# input$rnorm_mean <- 0
# input$rnorm_sd <- 1
# input$scree <- "Uniform"
# input$correlation <- "Uncorrelated"
# input$use_input_matrix <- TRUE
# input$upload_input_matrix$name <- "trait_space_small.csv"


# input$reduce <- "Limit"
# input$remove <- 0.4
# input$proportion_remove <- FALSE
# input$inverse_remove <- FALSE
# input$use_demo_groups <- FALSE

# input$metric_choice <- "User"
# input$metric1 <- "Median distance from centre (Manhattan)"
# input$metric_specific1 <- "mean"
# input$metric_specific2 <- "neighbours"
# input$metric_arguments <- FALSE
# input$show_metric <- FALSE
# input$edit_metric <- FALSE

# input$refresh <- 0
# input$axis_2 <- 2
# input$axis_1 <- 1
# input$color_scheme <- "Greyscale"
# input$scale_axis <- FALSE

## Get all functions
sourceDir("R/")


## Get the space details
## Return a space, or an error message to be written to output.
get.space <- function(input, args.only = FALSE){

    ## Input space
    if(input$space_type == "Input"){
        ## Load the file
        shiny::req(input$upload_input_matrix)
        space <- as.matrix(read.csv(file = input$upload_input_matrix$name, row.names = NULL, header = FALSE))

        ## Check whether it can work out with rownames
        if(class(space) != "matrix") {
            ## Was not a matrix
            return("Not a matrix")
        }

        ## First check if is numeric
        if(!is.numeric(space)) {
            ## Is not numeric
            ## Try reload with row or col names
            space <- as.matrix(read.csv(file = input$upload_input_matrix$name, row.names = 1, header = FALSE))
            ## Try changing parameters
            if(!is.numeric(space)) {
                space <- as.matrix(read.csv(file = input$upload_input_matrix$name, row.names = NULL, header = TRUE))
            } 
            if(!is.numeric(space)) {
                space <- as.matrix(read.csv(file = input$upload_input_matrix$name, row.names = 1, header = TRUE))
            }
        }

        ## Check again if space is numeric (after multiple loadings)
        if(!is.numeric(space)) {
            return("Input matrix does not contain only numeric values.")
        }

        ## Check class and space
        if(class(space) != "matrix" && any(is.na(space))) {
            return("Impossible to read the input matrix.\nThe input matrix should have no missing characters, and only numeric values.")
        }

        return(space)
    }

    ## Demo space
    if(input$space_type == "Demo"){
        ## Load the demo data
        data(demo_data)

        ## Select the right data
        dataset <- switch.demo.dataset(input)
        return(demo_data[[dataset]]$matrix)

    }

    ## Getting the arguments
    space_args <- list()
    ## Base arguments
    space_args$elements <- input$n_elements
    space_args$dimensions <- input$n_dimensions

    ## Distributions
    switch(input$distributions,
        Normal    = {
            single_distribution <- rnorm
            single_parameters <- list(list("mean" = input$rnorm_mean, "sd" = input$rnorm_sd))
        },
        LogNormal = {
            single_distribution <- rlnorm
            single_parameters <- list(list("meanlog" = input$rlnorm_mean, "sdlog" = input$rlnorm_sd))
        },
        Uniform   = {
            single_distribution <- runif
            single_parameters <- list(list("min" = input$runif_min, "max" = input$runif_max))
        },
        Gamma     = {
            single_distribution <- rgamma
            single_parameters <- list(list("shape" = input$rgamma_shape, "rate" = input$rgamma_rate))
        },
        Poisson   = {
            single_distribution <- rpois
            single_parameters <- list(list("lambda" = input$rpois_lambda))
        },
        Specific  = {
            single_distribution <- NULL
            single_parameters <- NULL
        }
    )

    ## Handle distribution arguments
    if(!is.null(single_distribution)) {
        ## Set a single distribution
        space_args$distribution <- single_distribution
        space_args$arguments <- single_parameters
    } else {
        ## Use multiple distributions (needs check)
        space_args$distribution <- eval(parse(text = input$distribution_list))

        ## Check space_args$distributions dimension and class
        if(length(space_args$distribution) != input$n_dimensions) {
            return("The number of specific distributions does not match the number of dimensions.")
        }
        if(any(lapply(space_args$distribution, class) != "function")) {
            return("At least one specific distribution is not a function from the stats package.")
        }

        ## Optional arguments
        if(input$optional_arguments) {
            space_args$arguments <- eval(parse(text = input$distribution_arguments))

            ## Check space_args$arguments dimension and class
            if(length(space_args$arguments) != input$n_dimensions) {
                return("The number of specific optional arguments does not match the number of dimensions.")
            }
            if(any(lapply(space_args$arguments, class) != "list")) {
                return("At least one specific optional argument is not a list.")
            }
        } else {
            space_args$arguments <- NULL
        }
    }

    switch(input$scree,
        "Uniform"    = {
            space_args$scree <- NULL
        },
        "Decreasing" = {
            scree <- rev(cumsum(rep(1/input$n_dimensions, input$n_dimensions)))
            space_args$scree <- scree#scree/sum(scree)
        },
        "LogNormal"  = {
            scree <- cumprod(rep(1/2, input$n_dimensions))
            space_args$scree <- scree/max(scree)
        }
    )

    ## Correlationa rgument
    switch(input$correlation,
        Uncorrelated = {
            space_args$cor.matrix <- NULL
        },
        Vector       = {
            correlation_values <- as.numeric(strsplit(input$correlation_value_vector, split = ",")[[1]])
            cor.matrix <- matrix(1, input$n_dimensions, input$n_dimensions)
            ## Check vector length
            if(length(correlation_values) != length(which(lower.tri(cor.matrix)))) {
                return(paste0("The number of correlations input does not match the ", length(which(lower.tri(cor.matrix))), " possible correlations."))
            }
            if(any(correlation_values > 1)) {
                return("Correlations cannot be > 1.")
            }
            ## Fill the matrix
            cor.matrix[lower.tri(cor.matrix)] <- correlation_values
            cor.matrix[upper.tri(cor.matrix)] <- correlation_values
            space_args$cor.matrix <- cor.matrix
        },
        Matrix       = {
            ## Check if any value is > 1
            if(any(input$cor.matrix > 1)) {
                return("Correlations cannot be > 1.")
            }
            ## Error if more than 15 dimensions
            if(any(input$cor.matrix > 1)) {
                return("Correlation:Matrix option is limited to 15 dimensions.\nTry toggling to \"Vector\" or \"Upload\" options.")
            }
            cor.matrix <- input$cor.matrix
            ## Mirroring the lower triangle
            cor.matrix[upper.tri(cor.matrix)] <- cor.matrix[lower.tri(cor.matrix)]
            space_args$cor.matrix <- cor.matrix
        },
        Upload       = {
            correlation_value_csv <- input$correlation_value_csv
            if(!is.null(correlation_value_csv)) {
                ## Read the matrix
                cor.matrix <- as.matrix(read.csv(file = correlation_value_csv$name, header = FALSE))
                diag(cor.matrix) <- 1
                cor.matrix[upper.tri(cor.matrix)] <- cor.matrix[lower.tri(cor.matrix)]
                if(any(cor.matrix > 1)) {
                    return("Correlations cannot be > 1.")
                }
                if(ncol(cor.matrix) != input$n_dimensions) {
                    return(paste0("Correlation matrix is ", ncol(cor.matrix), "x", ncol(cor.matrix), " but the number of dimensions is ", input$n_dimensions, "."))
                }
                space_args$cor.matrix <- cor.matrix
            } else {
                return("Impossible to read the uploaded csv file.\nTry another file or toggling to \"Vector\" or \"Matrix\" options.")
            }
        }
    )

    ## Return only the arguments
    if(args.only) {
        return(space_args)
    }

    ## Making the space
    space <- do.call(dispRity::space.maker, space_args, quote = TRUE)

    if(!is.matrix(space)) {
        return("Impossible to generate space.\nTry changing the parameters combinations\nor the distribution parameters.")
    }

    return(space)
}


#' Getting the character details
#' @param session Shiny session (to allow updating of character selection)
#' @return a character string if character extracted correctly,
#'  a list (detailing the error message to be displayed) if there's an error.
get.reduction <- function(input, space, session) {


    ## Demo space
    if(input$use_demo_groups == TRUE){
        ## Load the demo data
        data(demo_data)

        ## Select the right data
        dataset <- switch.demo.dataset(input)
        return(1:dim(demo_data[[dataset]]$matrix)[1] %in% c(demo_data[[dataset]]$subsets[[1]]$elements))
    }





    ## Set the parameters
    switch(input$reduce,
        Random = {
            ## Simple removal
            return(reduce.space(space, type = "random", 1-input$remove, verbose = FALSE, return.optim = FALSE))
        },
        Limit = {
            type <- "limit"
        },
        Displace = {
            type <- "displacement"
        },
        Density = {
            type <- "density"
        }
    )

    ## Default tuning
    tuning <- list(max = 50, tol = 0.01)

    ## Make the dimensions proportional?
    if(input$proportion_remove) {
        ## Getting the range per dimension
        scree <- apply(space, 2, FUN = function(X) diff(range(X)))/diff(range(space[,1]))
        ## Scaling each dimension to have the same range
        space_to_reduce <- space %*% diag(1/scree)
    } else {
        space_to_reduce <- space
    }

    ## Reducing the space
    remove <- reduce.space(space_to_reduce, type, 1-input$remove, tuning, verbose = FALSE, return.optim = FALSE)

    if(all(remove) || all(!remove)) {
        return("Impossible to remove data.\nTry hitting the \"refresh\" button,\nchanging the parameters combinations\nor the \"remove\" value.")
    }

    return(remove)
}


## Handles the dispRity metric
handle.metrics <- function(input, dispRity_args, session) {

    ## Metrics selection
    switch(input$metric_choice,
        Volume = {
            metric_name <- input$metric1
            switch(input$metric1,
                "Ellipsoid volume" = {
                    dispRity_args$metric <- ellipse.volume
                    dispRity_code <- "dispRity::ellipse.volume(matrix)"
                },
                "Convex hull surface" = {
                    if(input$n_dimensions > 5) {
                        return("For saving computational time, this version cannot\ncalculate convex hull for more than 5 dimensions.")
                    } else {
                        dispRity_args$metric <- convhull.surface
                        dispRity_code <- "dispRity::convhull.surface(matrix)"
                    }
                },
                "Convex hull volume" = {
                    if(input$n_dimensions > 5) {
                        return("For saving computational time, this version cannot\ncalculate convex hull for more than 5 dimensions.")
                    } else {
                        dispRity_args$metric <- convhull.volume
                        dispRity_code <- "dispRity::convhull.volume(matrix)"
                    }
                },
                "Median distance from centroid (Euclidean)" = {
                    dispRity_args$metric <- c(median, centroids)
                    dispRity_args$method <- "euclidean"
                    dispRity_code <- "stats::median(dispRity::centroids(matrix))"
                },
                "Median distance from centroid (Manhattan)" = {
                    dispRity_args$metric <- c(median, centroids)
                    dispRity_args$method <- "manhattan"
                    dispRity_code <- "stats::median(dispRity::centroids(matrix, method = \"manhattan\"))"
                },
                "n-ball volume" = {
                    dispRity_args$metric <- n.ball.volume
                    dispRity_code <- "dispRity::n.ball.volume(matrix)"
                },
                "Procrustes variance (geomorph::morphol.disparity)" = {
                    dispRity_args$metric <- function(X) return(sum(X^2)/nrow(X))
                    dispRity_code <- "sum(matrix^2)/nrow(matrix)"
                },
                "Product of variances" = {
                    dispRity_args$metric <- c(prod, variances)
                    dispRity_code <- "prod(dispRity::variances(matrix))"
                },
                "Product of ranges" = {
                    dispRity_args$metric <- c(prod, ranges)
                    dispRity_code <- "prod(dispRity::ranges(matrix))"
                },
                "Product of quantiles" = {
                    dispRity_args$metric <- c(prod, quantiles)
                    dispRity_code <- "prod(dispRity::quantiles(matrix))"
                },
                "Sum of ranges" = {
                    dispRity_args$metric <- c(sum, ranges)
                    dispRity_code <- "sum(dispRity::ranges(matrix))"
                },
                "Sum of variances" ={
                    dispRity_args$metric <- c(sum, variances)
                    dispRity_code <- "sum(dispRity::variances(matrix))"
                },
                "Sum of quantiles" = {
                    dispRity_args$metric <- c(sum, quantiles)
                    dispRity_code <- "sum(dispRity::quantiles(matrix))"
                }
            )
        },
        Density = {
            metric_name <- input$metric2
            switch(input$metric2,
                "Average Manhattan distance (geiger::dtt)" = {
                    dispRity_args$metric <- c(mean, pairwise.dist)
                    dispRity_args$method <- "manhattan"
                    dispRity_code <- "mean(dispRity::pairwise.dist(matrix, method = \"manhattan\"))"
                },
                "Average squared Euclidean distance (geiger::dtt)" = {
                    dispRity_args$metric <- function(X) mean(pairwise.dist(X)^2)
                    dispRity_code <- "mean(dispRity::pairwise.dist(matrix)^2)"
                },
                "Average nearest neighbours distance (Euclidean)" = {
                    dispRity_args$metric <- c(mean, neighbours)
                    dispRity_code <- "mean(dispRity::neighbours(matrix))"
                },
                "Average nearest neighbours distance (Manhattan)" = {
                    dispRity_args$metric <- c(mean, neighbours)
                    dispRity_args$method <- "manhattan"
                    dispRity_code <- "mean(dispRity::neighbours(matrix, method = \"manhattan\"))"
                },
                "Functional divergence (Villéger et al. 2008)" = {
                    dispRity_args$metric <- func.div
                    dispRity_code <- "dispRity::func.div(matrix)"
                },
                "Functional evenness (Villéger et al. 2008)" = {
                    dispRity_args$metric <- func.eve
                    dispRity_code <- "dispRity::func.eve(matrix)"
                },
                "Median pairwise distance (Euclidean)" = {
                    dispRity_args$metric <- c(median, pairwise.dist)
                    dispRity_args$method <- "euclidean"
                    dispRity_code <- "stats::median(dispRity::pairwise.dist(matrix))"
                },
                "Median pairwise distance (Manhattan)" = {
                    dispRity_args$metric <- c(median, pairwise.dist)
                    dispRity_args$method <- "manhattan"
                    dispRity_code <- "stats::median(dispRity::pairwise.dist(matrix, method = \"manhattan\"))"
                },
                "Minimum spanning tree average length" = {
                    dispRity_args$metric <- function(matrix) sum(span.tree.length(matrix))/nrow(matrix)
                    dispRity_code <- "sum(dispRity::span.tree.length(matrix))/nrow(matrix)"
                }
            )
        },
        Position = {
            metric_name <- input$metric3
            switch(input$metric3,
                "Average displacement (Euclidean)" = {
                    dispRity_args$metric <- c(mean, displacements)
                    dispRity_code <- "mean(dispRity::displacements(matrix))"
                },
                "Average displacement (Manhattan)" = {
                    dispRity_args$metric <- c(mean, displacements)
                    dispRity_args$method <- "manhattan"
                    dispRity_code <- "mean(dispRity::displacements(matrix, method = \"manhattan\"))"
                },
                "Median distance from centre (Euclidean)" = {
                    dispRity_args$metric <- c(median, centroids)
                    dispRity_args$method <- "euclidean"
                    dispRity_args$centroid <- 0
                    dispRity_code <- "stats::median(dispRity::centroids(matrix, centroid = 0))"
                },
                "Median distance from centre (Manhattan)" = {
                    dispRity_args$metric <- c(median, centroids)
                    dispRity_args$method <- "manhattan"
                    dispRity_args$centroid <- 0
                    dispRity_code <- "stats::median(dispRity::centroids(matrix, centroid = 0, method = \"manhattan\"))"
                }
            )
        },
        User = {
            ## Get name
            metric_name <- input$metric_specific1
            
            ## Get functions
            if(input$metric_specific2 == "NULL") {
                dispRity_args$metric <- eval(parse(text = input$metric_specific1))
                ## Export the code (for eventual display)
                dispRity_code <- paste0(input$metric_specific1, "(matrix)")
            } else {
                ## Update function
                dispRity_args$metric <- c(eval(parse(text = input$metric_specific1)), eval(parse(text = input$metric_specific2)))
                ## Update name
                metric_name <- paste0("c(",input$metric_specific1, ", ", input$metric_specific2, ")")
                ## Export the code (for eventual display)
                dispRity_code <- paste0(input$metric_specific1, "(", paste0(input$metric_specific2, "(matrix)"), ")")
            }
        }
    )

    if(input$show_metric == TRUE) {
        ## Update the metric display
        metric_display <- paste0("user.metric <- function(matrix) {\n\t", dispRity_code, "\n}")
        shiny::updateTextAreaInput(session, "manually_show_metric", value = metric_display)
    }

    if(input$edit_metric == TRUE) {
        ## Clean the arguments list
        dispRity_args <- dispRity_args[1]

        ## Default display
        error_msg <- paste0("Incorrect user metric format. You can start by copy/pasting: ", metric_display)
        if(input$manually_edit_metric == "copy/paste and edit the function above.") {
            return(error_msg)
        }

        ## Metric is user made
        dispRity_args$metric <- eval(parse(text = input$manually_edit_metric))

        ## Check if metric works
        if(!is.numeric(dispRity_args$metric(matrix(1, 5, 5)))) {
            return(error_msg)
        }
        ## Name is user made
        metric_name <- "user metric"
        ## Export the code (for eventual display)
        dispRity_code <- input$manually_show_metric
    }


    return(list(args = dispRity_args, name = metric_name, code = dispRity_code))
}

## Rendering the code snippet
render.snippet <- function(input) {
    ## Libraries
    libraries_head <- "## Loading the libraries"
    libraries <- "library(dispRity) ; library(moms)"

    ## Space maker
    space_maker_head <- "## Simulating a multidimensional space"
    space_args <- get.space(input, args.only = TRUE)
    space_maker <- paste0("space <- dispRity::space.maker(", paste(c("arg1", "arg2"), collapse = ", "), ")")

    ## Reduce space
    reduce_space_head <- "## Reduce the space"
    reduce_args <- input
    reduce_space <- paste0("remove <- moms::reduce.space(", paste(c("arg1", "arg2"), collapse = ", "), ")")

    ## Make groups
    custom_groups_head <- "## Make groups"
    custom_groups <- paste0("groups <- dispRity::custom.subsets(space, groups = ", paste(c("arg1", "arg2"), collapse = ", "), ")")

    ## Measure disparity
    meas_disparity_head <- "## Measure disparity"
    meas_disparity <- paste0("disparity <- dispRity::dispRity(groups, metric = ", paste(c("arg1", "arg2"), collapse = ", "), ")")

    ## Summarise
    sum_disparity_head <- "## Summarising the results"
    sum_disparity <- paste0("summary(disparity)")

    return(paste(c(libraries_head, libraries, space_maker_head, space_maker, reduce_space_head, reduce_space, custom_groups_head, custom_groups, meas_disparity_head, meas_disparity, sum_disparity_head, sum_disparity), collapse = "\n"))
}


## Display the error message
plot.error <- function(text, col = "#cc6644", font = 1, cex = 1.2, ...) {
    ## Empty plot
    plot(NULL, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
    ## Error message
    text(0.5, 0.5, paste("Error:", text), col = col, font = font, cex = cex, ...)
}

## Get proportional change in a table
get.prop.change <- function(table, change = "change") {
    ## Get the proportional change
    proportional_change <- table[2,3]/table[1,3]*100-100
    ## Adding the proportional change
    table <- cbind(table, c("", paste(round(proportional_change, 2), "%")))
    ## Change the column name
    colnames(table)[4] <- change

    return(table)
}

switch.demo.dataset <- function(input) {
    switch(input$demo_data,
       "Beck and Lee 2014"  = {dataset <- 1},
       "Wright 2017"        = {dataset <- 2},
       "Marcy et al. 2016"  = {dataset <- 3},
       "Hopkins et al. 2016"= {dataset <- 4},
       "Jones et al. 2015"  = {dataset <- 5},
       "Healy et al. 2019"  = {dataset <- 6}
       )
    return(dataset)
}