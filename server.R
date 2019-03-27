# ## DEBUG
# stop("DEBUG server")
# input <- list()
# input$n_dimensions <- 10
# input$n_elements <- 300
# input$distributions <- "Normal"
# input$reduce <- "Limit"
# input$remove <- 0.5
# input$rnorm_mean <- 2
# input$rnorm_sd <- 10
# input$axis_2 <- 2
# input$axis_1 <- 1
# input$scree <- "Uniform"



library(shiny)
library(dispRity)
library(moms)

## Load the R functions
source("helpers.R")

## Get the space details
## Return a space, or an error message to be written to output.
get.space <- function(input) {

    ## Default error
    space <- "Space parameters not specified."

    ## Getting the arguments
    space_args <- list()
    ## Base arguments
    space_args$elements <- input$n_elements
    space_args$dimensions <- input$n_dimensions

    ## Distributions
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
        ## Set a single distribution
        space_args$distribution <- single_distribution
        space_args$arguments <- single_parameters
    } else {
        ## Use multiple distributions (needs check)
        return("Multiple distributions not implemented yet.")
    }

    switch(input$scree,
        Uniform    = {
            space_args$scree <- NULL
        },
        Decreasing = {
            scree <- rev(cumsum(rep(1/input$n_dimensions, input$n_dimensions)))
            space_args$scree <- scree/sum(scree)
        },
        LogNormal   = {
            scree <- cumprod(rep(1/input$n_dimensions, input$n_dimensions))
            space_args$scree <- scree/sum(scree)
        }
    )

    ## Default extra arguments
    length_args <- length(space_args)
    space_args[length_args + 1] <- list(NULL)
    names(space_args)[length_args + 1] <- c("cor.matrix")

    ## Making the space
    space <- do.call(dispRity::space.maker, space_args, quote = TRUE)
    return(space)
}


#' Getting the character details
#' @param session Shiny session (to allow updating of character selection)
#' @return a character string if character extracted correctly,
#'  a list (detailing the error message to be displayed) if there's an error.
get.reduction <- function(input, space, session) {

    ## Default error
    remove <- "Error in removing things."

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
    #TG: make these parameters dynamic, the bigger the space, the rougher the parameter.
    #TG: input$n_dimensions * input$n_elements


    #TODO Check the parameters (if available go straight for it, if missing use optim)
    #TODO + add to remove a "relative" option

    ## Reducing the space
    remove <- reduce.space(space, type, input$remove, tuning, verbose = FALSE, return.optim = TRUE)
    ## Update the parameter value
    shiny::updateNumericInput(session, "optimise", value = remove$optim)


    return(remove$remove)
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
            }

            ## ~~~~~~~~~~
            ## Plotting the space
            ## ~~~~~~~~~~
            ## Default plotting options
            defaults <- list()
            defaults$pch <- 19
            defaults$col1 <- "black"
            defaults$lab <- "Trait"
            defaults$cex <- 1

            ## Background plot
            plot(space[, c(input$axis_1, input$axis_2)],
                pch = defaults$pch,
                xlim = defaults$xlim,
                ylim = defaults$ylim,
                col = "black",
                main = NULL,
                xlab = paste(defaults$lab, input$axis_1),
                ylab = paste(defaults$lab, input$axis_2),
                cex = defaults$cex)

            ## Removal plot
            if(input$reduce != "None") {
                points(space[to_remove, c(input$axis_1, input$axis_2)],
                       pch = defaults$pch,
                       col = "grey",
                       cex = defaults$cex+0.1)
            }

        })

        ## Output plot
        output$plot.ui <- renderUI({

            error <- NULL

            if (is.null(error)) {
                plotOutput("plot_out", height = 750, width = 750)
                #TODO: make the plot size dynamic!!!!
            } else {
                plotOutput("plot_out", width ="100%", height = "40px")
                plotError(tree)
            }
        })
    }
)
