# ## DEBUG
# stop("DEBUG server")
# input <- list()
# input$n_dimensions <- 2
# input$n_elements <- 300
# input$distributions <- "Normal"
# input$reduce <- "Random"
# input$remove <- 0.5
# input$rnorm_mean <- 2
# input$rnorm_sd <- 10
# input$axis_2 <- 2
# input$axis_1 <- 1


library(shiny)
library(dispRity)
library(moms)

## Load the R functions
source("helpers.R")

## Get the space details
## Return a space, or an error message to be written to output.
get.space <- function(input, session) {

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

    ## Default extra arguments
    length_args <- length(space_args)
    space_args[length_args + 1] <- space_args[length_args + 2] <- list(NULL)
    names(space_args)[length_args + 1:2] <- c("cor.matrix", "scree")

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

    ## Reducing the space
    remove <- reduce.space(space, type, remove, parameters, tuning, verbose = FALSE)


    return(remove)
}

## Generate the seeds for plotting
seeds <- sample(1:200)*sample(1:10)

# server.R
shinyServer(
    function(input, space, output, session) {
        # Plotting function
        output$plot_out <- renderPlot({

            ## ~~~~~~~~~~
            ## Making the space
            ## ~~~~~~~~~~

            space <- get.space(input, session)

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

            ## ~~~~~~~~~~
            ## Plotting the space
            ## ~~~~~~~~~~
            if(input$reduce == "None") {

                plot(space[, c(input$axis_1, input$axis_2)])

            } # else {

            #}

            # par(bty = "n")
            # plot.space <- function(space, remove, main, defaults, axis = c(1,2), ...) {
            #     ## Plot the first space
            #     plot(space[, axis], pch = defaults$pch, xlim = defaults$xlim, ylim = defaults$ylim, col = defaults$col1,
            #          main = main, xlab = paste(defaults$xlab, axis[1]), ylab = paste(defaults$xlab, axis[2]), cex = defaults$cex, ...)
                
            #     ## Plot the second space
            #     points(space[remove, axis], pch = defaults$pch, col = defaults$col2, cex = defaults$cex)
            # }

        })

        ## Output plot
        output$plot.ui <- renderUI({

            ## Reset the seed when hitting the refresh button
            set.seed(seeds[(input$refresh)+1])

            # if (class(tree) == "character") {
            #     plotOutput("plot_out", width ="100%", height = "40px")
            #     plotError(tree)
            # } else {
            #     n_tip <- length(tree$tip.label)
            #     ## Set the plot window
                plotOutput("plot_out")#, width ="100%", height = "100%")#paste(round((n_tip + 3.3) * 0.4) * 90L, "px", sep = ""))
            # }
        })
    }
)
