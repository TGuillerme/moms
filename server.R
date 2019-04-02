# ## DEBUG
# stop("DEBUG server")
# input <- list()
# input$n_dimensions <- 3
# input$n_elements <- 300
# input$distributions <- "Specific"
# input$distribution_list <- "list(rnorm, runif, rlnorm)"
# input$optional_arguments <- TRUE
# input$distribution_arguments <- "list(list(mean = 0, sd = 1), list(NULL), list(meanlog = 5))"
# input$reduce <- "Limit"
# input$remove <- 0.5
# input$runif_min <- 0
# input$runif_max <- 1
# input$axis_2 <- 2
# input$axis_1 <- 1
# input$scree <- "LogNormal"
# input$refresh <- 0



library(shiny)
library(dispRity)
library(moms)

## Load the R functions
source("helpers.R")

## Get the space details
## Return a space, or an error message to be written to output.
get.space <- function(input) {

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
            scree <- c(1, cumprod(rep(1/input$n_dimensions, input$n_dimensions)))[-input$n_dimensions+1]
            space_args$scree <- scree#scree/sum(scree)
        }
    )

    ## Default extra arguments
    length_args <- length(space_args)
    space_args[length_args + 1] <- list(NULL)
    names(space_args)[length_args + 1] <- c("cor.matrix")

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

    ## Set the parameters
    switch(input$reduce,
        Random = {
            ## Simple removal
            return(reduce.space(space, type = "random", input$remove, verbose = FALSE, return.optim = FALSE))
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
    remove <- reduce.space(space_to_reduce, type, input$remove, tuning, verbose = TRUE, return.optim = FALSE)

    if(all(remove) || all(!remove)) {
        return("Impossible to remove data.\nTry hitting the \"refresh\" button,\nchanging the parameters combinations\nor the \"remove\" value.")
    }

    return(remove)
}

## Generate the seeds for plotting
seeds <- sample(1:200)*sample(1:10)

# server.R
shinyServer(
    function(input, space, output, session) {
        # Plotting function
        output$plot_out <- renderPlot({

            ## Reset the seed when hitting the refresh button
            set.seed(seeds[(input$refresh)+1])

            ## ~~~~~~~~~~
            ## Making the space
            ## ~~~~~~~~~~
            space <- get.space(input)

            ## Return error
            if(class(space) == "character") {
                plot.error(space)
                return(NULL)
            }

            ## Update number of dimensions (if required)
            if(input$axis_1 > input$n_dimensions) {
                shiny::updateNumericInput(session, "axis_1", max = input$n_dimensions, value = input$n_dimensions)
            } else {
                shiny::updateNumericInput(session, "axis_1", max = input$n_dimensions)
            }
            if(input$axis_2 > input$n_dimensions) {
                shiny::updateNumericInput(session, "axis_2", max = input$n_dimensions, value = input$n_dimensions)
            } else {
                shiny::updateNumericInput(session, "axis_2", max = input$n_dimensions)
            }

            ## ~~~~~~~~~~
            ## Reducing the space
            ## ~~~~~~~~~~
            if(input$reduce != "None") {
                to_remove <- get.reduction(input, space, session)

                ## Return error
                if(class(to_remove) == "character") {
                    plot.error(to_remove)
                    return(NULL)
                }

            }

            ## ~~~~~~~~~~
            ## Plotting the space
            ## ~~~~~~~~~~
            ## Default plotting options
            defaults <- list()
            defaults$pch <- 19
            defaults$palette <- c("black", "grey")
            defaults$lab <- "Trait"
            defaults$cex <- 1

            ## Background plot
            if(input$reduce == "None") {            
                plot(space[, c(input$axis_1, input$axis_2)],
                    pch = defaults$pch,
                    xlim = defaults$xlim,
                    ylim = defaults$ylim,
                    col = defaults$palette[1],
                    main = NULL,
                    xlab = paste(defaults$lab, input$axis_1),
                    ylab = paste(defaults$lab, input$axis_2),
                    cex = defaults$cex)

            } else {
                ## Selecting which points to remove
                if(input$inverse_remove) {
                    points_remove <- !to_remove
                } else {
                    points_remove <- to_remove
                }

                plot(space[, c(input$axis_1, input$axis_2)],
                    pch = defaults$pch,
                    xlim = defaults$xlim,
                    ylim = defaults$ylim,
                    col = defaults$palette[2],
                    main = NULL,
                    xlab = paste(defaults$lab, input$axis_1),
                    ylab = paste(defaults$lab, input$axis_2),
                    cex = defaults$cex)

                ## Plotting the points
                points(space[!points_remove, c(input$axis_1, input$axis_2)],
                       pch = defaults$pch,
                       col = defaults$palette[1],
                       cex = defaults$cex)
            }


            ## ~~~~~~~~~~
            ## Disparity
            ## ~~~~~~~~~~
            ## Make some dummy table

            ### Some dummy table
            output$table_out <- renderTable({

                rownames(space) <- 1:input$n_elements

                ## Make the disparity object
                if(input$reduce == "None") {
                    groups <- space
                } else {

                    ## Error in points remove
                    if(!exists("points_remove")) {
                        return("Error in table.")
                    }
                    groups <- dispRity::custom.subsets(space,
                                                       group = list("Full space" = rownames(space),
                                                                    "Reduced space" = rownames(space)[points_remove]))
                }

                ## Measure disparity
                metric <- c(sum, dispRity::variances) ; warning("Fix metric input")
                metric_name <- "sum of variances"

                ## Error handling in metrics
                # if(!exists("points_remove")) {
                #     return("Error in table.")
                # }

                ## Measure disparity
                disparity <- dispRity::dispRity(groups, metric = metric)

                ## Rendering the output table
                output <- summary(disparity)
                #TODO: get the right number of digits
                #TODO: add the values row by row.
                
                ## Add names
                rownames(output) <- output$subsets
                colnames(output)[3] <- metric_name

                ## Print output
                output[,-1]
            })
        },
        height = reactive(ifelse(!is.null(input$innerWidth), input$innerWidth*3/6.5, 0))
        # height = reactive(ifelse(!is.null(input$innerWidth),ifelse(input$innerWidth < 6, input$innerWidth*2, input$innerWidth/2.25),0)),
        # width = reactive(ifelse(!is.null(input$innerWidth),ifelse(input$innerWidth < 6, input$innerWidth*2, input$innerWidth/2.25),0))
        )
    }
)
