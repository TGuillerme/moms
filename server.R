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
            defaults$lab <- "Dimension"
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

            ## Some dummy table
            output$table_out <- renderTable({

                ## Name the elements
                rownames(space) <- 1:input$n_elements

                ## Make the disparity object
                if(input$reduce == "None") {
                    ## Simple space
                    groups <- space
                } else {
                    ## Error in points remove
                    if(!exists("points_remove")) {
                        return("Reduction parameter did not remove any points.\nTry different parameter combinations.")
                    }
                    ## Custom subsets
                    groups <- custom.subsets(space,
                                    group = list("Full space" = rownames(space),
                                                 "Reduced space" = rownames(space)[points_remove]))
                }

                # if(input$add.metric)

                ## Handling the disparity metrics
                metrics_handle <- handle.metrics(input, dispRity_args = list(data = groups))

                ## Errors from metrics_handle
                if(class(metrics_handle) == "character") {
                    return(metrics_handle)
                }

                ## Measuring disparity
                disparity <- do.call(dispRity, metrics_handle$args)

                ## Rendering the output table
                table_out <- summary(disparity)


                ## Add names
                rownames(table_out) <- table_out$subsets
                colnames(table_out)[3] <- metrics_handle$name

                if(input$reduce != "None") {
                    ## Get the proportional change
                    proportional_change <- table_out[2,3]/table_out[1,3]*100-100

                    ## Adding the proportional change
                    table_out <- cbind(table_out, c("", paste(round(proportional_change, 2), "%")))
                    ## Change the column name
                    colnames(table_out)[4] <- "change"
                }
                
                ## Adding and removing metrics
                # if(input$add.metric){
                #     new_metrics_handle <- handle.metrics(input, dispRity_args = list(data = groups))

                #     ## Measuring disparity
                #     new_disparity <- do.call(dispRity, new_metrics_handle$args)

                #     ## Rendering the output table
                #     new_output <- summary(new_disparity)
                    
                #     ## Add names
                #     rownames(output) <- output$subsets
                #     colnames(output)[3] <- metrics_handle$name
                # }
                # if(input$remove.metric){
                #     print("remove metric")
                #     print(input$remove.metric)
                # }


                ## Print output
                table_out[,-1]

            })


            ## ~~~~~~~~~
            ## Code snippet
            ## ~~~~~~~~~
            if(input$display_code_snippet) {
                output$code_snippet  <- renderText({
                    ## Get the code snippet
                    snippet_out <- render.snippet(input)

                    ## Export
                    switch(input$export_code_snippet,
                        "In app display" = {
                            ## Export the code text
                            snippet_out
                        },
                        "R code" = {
                            ## Export the R code file
                            "Not implemented yet"
                        },
                        "R markdown" = {
                            ## Export the R markdown file
                            "Not implemented yet"
                        }
                    )

                })
            }

        },
        ## Plot size
        height = reactive(ifelse(!is.null(input$innerWidth), input$innerWidth*3/7.5, 0))
        )
    }
)
