library(shiny)
library(shinyMatrix)
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

            # ## Profiling
            # Rprof(moms_profiling <- "moms_profiling.log", memory.profiling = TRUE )

            ## Reset the seed when hitting the refresh button
            set.seed(seeds[(input$refresh)+1])

            ## ~~~~~~~~~~
            ## Making the space
            ## ~~~~~~~~~~
            space <- get.space(input)

            ## Update the dimensions if matrix is input/demo
            if(input$space_type == "Input") {
                shiny::updateNumericInput(session, "n_dimensions", max = ncol(space), value = ncol(space))
                shiny::updateNumericInput(session, "n_elements", max = nrow(space), value = nrow(space))
            }

            ## Update the matrix input
            if(input$space_type == "User") {
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
            ## Profiling toggle off
            
            ## ~~~~~~~~~~
            ## Reducing the space
            ## ~~~~~~~~~~
            if(input$reduce != "None") {
                reduced_space <- get.reduction(input, space, session)

                ## Return error
                if(class(reduced_space) == "character") {
                    plot.error(reduced_space)
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
            defaults$xlim <- range(space[, (input$axis_1)])
            defaults$ylim <- range(space[, (input$axis_2)])
            if(input$scale_axis) {
                ## Make both axis the same scale
                defaults$xlim <- defaults$ylim <- range(space[, c(input$axis_1, input$axis_2)])
            }

            ## Get the variance per axis
            all_variance <- apply(space, 2, var)
            var_axis_1 <- round(var(space[, (input$axis_1)])/sum(all_variance)*100, 2)
            var_axis_2 <- round(var(space[, (input$axis_2)])/sum(all_variance)*100, 2)

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
                    xlab = paste(defaults$lab, input$axis_1, paste0("(",var_axis_1,"%)")),
                    ylab = paste(defaults$lab, input$axis_2, paste0("(",var_axis_2,"%)")),
                    cex = defaults$cex)

            } else {
                ## Selecting which points to remove
                if(input$inverse_remove) {
                    if(input$space_type != "Demo") {
                        ## Switch reduce space only if not demo data
                        reduced_space_points <- !reduced_space
                    } else {
                        ## If demo data, check if groups are demo or not
                        if(input$use_demo_groups == TRUE) {
                            reduced_space_points <- reduced_space
                        } else {
                            reduced_space_points <- !reduced_space
                        }
                    }
                } else {
                    reduced_space_points <- reduced_space
                }

                plot(space[!reduced_space_points, c(input$axis_1, input$axis_2)],
                    pch = defaults$pch,
                    xlim = defaults$xlim,
                    ylim = defaults$ylim,
                    col = defaults$palette[[2]],
                    main = NULL,
                    xlab = paste(defaults$lab, input$axis_1, paste0("(",var_axis_1,"%)")),
                    ylab = paste(defaults$lab, input$axis_2, paste0("(",var_axis_1,"%)")),
                    cex = defaults$cex)

                ## Plotting the points
                points(space[reduced_space_points, c(input$axis_1, input$axis_2)],
                       pch = defaults$pch,
                       col = defaults$palette[[1]],
                       cex = defaults$cex)

                ## Add the legend for the default spaces
                if(input$space_type == "Demo" && input$use_demo_groups == TRUE) {
                    ## Select the dataset names
                    subset_names <- names(demo_data[[switch.demo.dataset(input)]]$subsets)
                
                    ## Get the legend text
                    legend_text <- c(paste(subset_names[1], paste0("(", length(which(reduced_space_points)),")")),
                                     paste(subset_names[2], paste0("(", length(which(!reduced_space_points)),")")))
                    legend("topleft", legend = legend_text, col = c(defaults$palette[[1]][1], defaults$palette[[2]][1]), pch = defaults$pch, cex = defaults$cex)
                }
            }

            ## ~~~~~~~~~~
            ## Disparity
            ## ~~~~~~~~~~

            ## Some dummy table
            output$table_out <- renderTable({

                ## Name the elements
                rownames(space) <- 1:nrow(space)

                ## Make the disparity object
                if(input$reduce == "None") {
                    ## Simple space
                    groups <- space
                } else {
                    ## Custom subsets

                    if(input$space_type == "Demo" && input$use_demo_groups == TRUE) {
                        ## Select the demo dispRity object
                        groups <- demo_data[[switch.demo.dataset(input)]]
                    } else {
                        ## Create dispRity groups
                        groups <- custom.subsets(space,
                                        group = list("Full space" = rownames(space),
                                                     "Reduced space" = rownames(space)[reduced_space_points]))
                    }
                }

                ## Handling the disparity metrics
                metrics_handle <- handle.metrics(input, dispRity_args = list(data = groups), session)

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
                    ## Add the change column (if groups are not pre-made)
                    if(input$space_type == "Demo" && input$use_demo_groups == TRUE) {
                        ## Return full table with proportional difference
                        return(get.prop.change(table_out, change = "difference"))
                    } else {
                        ## Get the proportional change
                        table_out <- get.prop.change(table_out)

                        ## Print the output
                        return(table_out[,-1])
                    }
                } else {
                    ## Print the output
                    return(table_out[,-1])
                }
            })
            # ~~~~~~~~~
            # Code snippet
            # ~~~~~~~~~
            # if(input$display_code_snippet) {
            #     output$code_snippet  <- renderText({
            #         ## Get the code snippet
            #         snippet_out <- render.snippet(input)

            #         ## Export
            #         switch(input$export_code_snippet,
            #             "In app display" = {
            #                 ## Export the code text
            #                 snippet_out
            #             },
            #             "R code" = {
            #                 ## Export the R code file
            #                 "Not implemented yet"
            #             },
            #             "R markdown" = {
            #                 ## Export the R markdown file
            #                 "Not implemented yet"
            #             }
            #         )

            #     })
            # }

        },
        ## Plot size
        height = reactive(ifelse(!is.null(input$innerWidth), input$innerWidth*3/7.5, 0))
        )
    }
)
