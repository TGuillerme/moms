library(shiny)
library(dispRity)
library(moms)

## Load the R functions
source("helpers.R")

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

            ## Update the matrix input
            if(input$n_dimensions != nrow(input$cor.matrix) && input$n_dimensions < 15) {
                shinyMatrix::updateMatrixInput(session, "cor.matrix", value = diag(input$n_dimensions))
            }

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
            defaults$lab <- "Trait"
            defaults$cex <- 1

            ## colours
            switch(input$color_scheme,
                Greyscale = {
                    defaults$palette <- list("black", "grey")
                    },
                Contrast  = {
                    defaults$palette <- list("blue", "orange")
                    },
                Pink      = {
                    defaults$palette <- list("purple", "pink")
                    },
                Rainbow   = {
                    defaults$palette <- list(rainbow(input$n_elements*input$remove), "grey")
                    }
                )


            ## Background plot
            if(input$reduce == "None") {            
                plot(space[, c(input$axis_1, input$axis_2)],
                    pch = defaults$pch,
                    xlim = defaults$xlim,
                    ylim = defaults$ylim,
                    col = defaults$palette[[1]],
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
                    col = defaults$palette[[2]],
                    main = NULL,
                    xlab = paste(defaults$lab, input$axis_1),
                    ylab = paste(defaults$lab, input$axis_2),
                    cex = defaults$cex)

                ## Plotting the points
                points(space[!points_remove, c(input$axis_1, input$axis_2)],
                       pch = defaults$pch,
                       col = defaults$palette[[1]],
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
                        return("Reduction parameter did not remove any points.\nTry different parameter combinations.")
                    }
                    groups <- dispRity::custom.subsets(space,
                                                       group = list("Full space" = rownames(space),
                                                                    "Reduced space" = rownames(space)[points_remove]))
                }

                # dispRity_args <- list(data = groups)

                metrics_handle <- handle.metrics(input, dispRity_args = list(data = groups))

                ## Metrics selection
                # switch(input$metric_choice,
                #     Volume = {
                #         metric_name <- input$metric1
                #         switch(input$metric1,
                #             "Ellipsoid volume" = {
                #                 dispRity_args$metric <- ellipse.volume
                #             },
                #             "Convex hull surface" = {
                #                 if(input$n_dimensions > 15) {
                #                     return("For saving computational time, this version cannot\ncalculate convex hull for more than 15 dimensions.")
                #                 }
                #                 dispRity_args$metric <- convhull.surface
                #             },
                #             "Convex hull volume" = {
                #                 if(input$n_dimensions > 15) {
                #                     return("For saving computational time, this version cannot\ncalculate convex hull for more than 15 dimensions.")
                #                 }
                #                 dispRity_args$metric <- convhull.volume
                #             },
                #             "Median distance from centroid (Euclidean)" = {
                #                 dispRity_args$metric <- c(median, centroids)
                #                 dispRity_args$method <- "euclidean"
                #             },
                #             "Median distance from centroid (Manhattan)" = {
                #                 dispRity_args$metric <- c(median, centroids)
                #                 dispRity_args$method <- "manhattan"
                #             },
                #             "n-ball volume" = {
                #                 dispRity_args$metric <- n.ball.volume
                #             },
                #             "Procrustes variance (geomorph::morphol.disparity)" = {
                #                 dispRity_args$metric <- function(X) return(sum(X^2)/nrow(X))
                #             },
                #             "Product of variances" = {
                #                 dispRity_args$metric <- c(prod, variances)
                #             },
                #             "Product of ranges" = {
                #                 dispRity_args$metric <- c(prod, ranges)
                #             },
                #             "Sum of ranges" = {
                #                 dispRity_args$metric <- c(sum, ranges)
                #             },
                #             "Sum of variances" ={
                #                 dispRity_args$metric <- c(sum, variances)
                #             }
                #         )
                #     },
                #     Density = {
                #         metric_name <- input$metric2
                #         switch(input$metric2,
                #             "Average Manhattan distance (geiger::dtt)" = {
                #                 dispRity_args$metric <- c(mean, pairwise.dist)
                #                 dispRity_args$method <- "manhattan"
                #             },
                #             "Average squared Euclidean distance (geiger::dtt)" = {
                #                 dispRity_args$metric <- function(X) mean(pairwise.dist(X)^2)
                #             },
                #             "Mean pairwise distance (Euclidean)" = {
                #                 dispRity_args$metric <- c(median, pairwise.dist)
                #                 dispRity_args$method <- "euclidean"
                #             },
                #             "Mean pairwise distance (Manhattan)" = {
                #                 dispRity_args$metric <- c(median, pairwise.dist)
                #                 dispRity_args$method <- "manhattan"
                #             },
                #             "Minimum spanning tree length" = {
                #                 dispRity_args$metric <- span.tree.length
                #             }
                #         )
                #     },
                #     Position = {
                #         metric_name <- input$metric3
                #         switch(input$metric3,
                #             "Median distance from centre (Euclidean)" = {
                #                 dispRity_args$metric <- c(median, centroids)
                #                 dispRity_args$method <- "euclidean"
                #                 dispRity_args$centroid <- 0
                #             },
                #             "Median distance from centre (Manhattan)" = {
                #                 dispRity_args$metric <- c(median, centroids)
                #                 dispRity_args$method <- "manhattan"
                #                 dispRity_args$centroid <- 0
                #             }
                #         )
                #     },
                #     User = {
                #         ## Personalised metric
                #         metric_name <- input$metric_specific1
                #         print("Pre-condition:")
                #         print(input$metric_specific1)
                #         print(input$metric_specific2)

                #         if(input$metric_specific2 == "NULL") {

                #             print("condition 1")
                #             print(input$metric_specific1)
                #             print(eval(parse(text = input$metric_specific1)))
                #             dispRity_args$metric <- eval(parse(text = input$metric_specific1))

                #             print(metric_name)
                #             print(dispRity_args[-1])


                #         } else {

                #             print("condition 2")
                #             print(input$metric_specific2)
                #             print(eval(parse(text = input$metric_specific2)))
                #             print(c(eval(parse(text = input$metric_specific1)), eval(parse(text = input$metric_specific2))))
                #             dispRity_args$metric <- c(eval(parse(text = input$metric_specific1)), eval(parse(text = input$metric_specific2)))
                #             metric_name <- paste0("c(",input$metric_specific1, ", ", input$metric_specific2, ")")

                #             print(metric_name)
                #             print(dispRity_args[-1])
                #         }


                #         ## Optional arguments
                #         if(input$metrics_arguments) {
                #             return("Optional arguments for personalised metrics are not yet available in this version.")
                #             # dispRitys_args <- list(dispRity_args, eval(parse(text = input$metric_optional_arguments)))
                #         }
                #     }
                # )

                ## Measuring disparity
                disparity <- do.call(dispRity, metrics_handle$args)

                ## Rendering the output table
                output <- summary(disparity)
                
                ## Add names
                rownames(output) <- output$subsets
                colnames(output)[3] <- metrics_handle$name

                ## Print output
                output[,-1]
            })
        },


        ## Plot size
        height = reactive(ifelse(!is.null(input$innerWidth), input$innerWidth*3/7.5, 0))
        # height = reactive(ifelse(!is.null(input$innerWidth),ifelse(input$innerWidth < 6, input$innerWidth*2, input$innerWidth/2.25),0)),
        # width = reactive(ifelse(!is.null(input$innerWidth),ifelse(input$innerWidth < 6, input$innerWidth*2, input$innerWidth/2.25),0))
        )
    }
)
