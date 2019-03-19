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


library(shiny)
library(dispRity)
# library(ape)

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
            single_parameters <- list(list("mean" = as.numeric(input$rnorm_mean), "sd" = as.numeric(input$rnorm_sd)))
        },
        LogNormal = {
            single_distribution <- rlnorm
            single_parameters <- list(list("meanlog" = as.numeric(input$rlnorm_mean), "sdlog" = as.numeric(input$rlnorm_sd)))
        },
        Uniform   = {
            single_distribution <- runif
            single_parameters <- list(list("min" = as.numeric(input$runif_min), "max" = as.numeric(input$runif_max)))
        },
        Gamma     = {
            single_distribution <- rgamma
            single_parameters <- list(list("shape" = as.numeric(input$rgamma_shape), "rate" = as.numeric(input$rgamma_rate)))
        },
        Poisson   = {
            single_distribution <- rpois
            single_parameters <- list(list("lambda" = as.numeric(input$rpois_lambda)))
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

            






            # ## Getting the parameters
            # tree <- get.tree(input, session)
            # if (class(tree) == 'character') {
            #   return(plotError(tree))
            # } else if (class(tree) != 'phylo'){
            #   return(plotError("The tree must be of class 'phylo'."))
            # }
            
            # character <- get.character(input, tree, session)
            # if (class(character) == 'list') {
            #   return(plotError(paste(character, sep = '', collapse = '')))
            # }

            # if (length(character) == 0) {
            #   return(plotError("Character of length zero."))
            # }

            # n_tip <- length(tree$tip.label)
            # if (class(character) == 'matrix') {
            #     state_labels <- attr(character, 'state.labels')[[1]]
            #     # TODO! If unobserved states are labelled, they should be removed!

            #     character_name <- colnames(character)

            #     if (all(tree$tip.label %in% rownames(character))) {
            #         character <- character[tree$tip.label, ]
            #     } else {
            #         return(plotError(paste("No entries in character list correspond to ",
            #                          paste(tree$tip.label[!(tree$tip.label %in% rownames(character))]))))
            #     }
            # } else {
            #     character_name <- NULL
            #     state_labels <- NULL
            # }

            # ## Transform character
            # if(class(character) != "list") {
            #     character <- convert.char(character)
            # }

            # ## Check if the character is the same length as the tree
            # if (n_tip != length(character)) {
            #     return(plotError(paste(n_tip, " tips in the tree, but ",
            #                            length(character), " entries in the data matrix")))
            # }

            # ## Run the algorithm
            # if(as.numeric(input$method) == 1) {
            #     states_matrix <- apply.reconstruction(tree, character, method = "NA", inapplicable = NULL, match.tip.char = as.logical(input$matchtipchar))
            # } else {
            #     states_matrix <- apply.reconstruction(tree, character, method = "Fitch", inapplicable = as.numeric(input$fitch_inapp), match.tip.char = as.logical(input$matchtipchar))
            # }

            # ## ~~~~~~~~~~
            # ## Plotting the results
            # ## ~~~~~~~~~~

            # ## Graphical options
            # if(is.null(input$showlabels)) {
            #     showlabels <- NULL
            # } else {
            #     showlabels <- as.numeric(input$showlabels)
            # }

            # ## Passes
            # if(as.numeric(input$method) == 1) {
            #     show_passes <- as.vector(as.numeric(input$showPassInapp))
            # } else {
            #     show_passes <- as.vector(as.numeric(input$showPassFitch))
            # }

            # plot.states.matrix(states_matrix, passes = show_passes,
            #                    show.labels = showlabels,
            #                    counts = as.vector(as.numeric(input$counts)),
            #                    col.states = input$colour_states,
            #                    state.labels = state_labels)
            # mtext(side=c(1, 3), character_name, font=2)

            ## Exporting data
            # output$downloadData <- downloadHandler(

            #     ## Filename management
            #     filename = function() {
            #         ## Managing the output suffix
            #         suffix <- input$output_type
            #         suffix <- ifelse(suffix == "newick", "tre", suffix)
            #         suffix <- ifelse(suffix == "nexus", "nex", suffix)
            #         suffix <- ifelse(suffix == "C-test", "txt", suffix)
            #         ## Getting the output name
            #         paste(paste("Inapp", format(Sys.time(), "%Y-%m-%d-%H%M%S"), sep = "_"), sep = ".", suffix)
            #     },

            #     ## Export management
            #     content = function(file) {
            #         ## Save as a csv
            #         if(input$output_type == "csv") {
            #             write.csv(make.output.data.frame(states_matrix), file)
            #         }
            #         ## Save as a pdf
            #         if(input$output_type == "pdf") {
            #             pdf(file)
            #             plot.states.matrix(states_matrix, passes = show_passes, show.labels = showlabels, counts = as.vector(as.numeric(input$counts)))
            #             dev.off()
            #         }
            #         ## Save as a newick
            #         if(input$output_type == "newick") {
            #             tree$edge.length <- NULL
            #             states_dataframe <- make.output.data.frame(states_matrix)
            #             node_notes <- lapply(as.list(1:(states_matrix$n_tip + states_matrix$n_node)), create.note, states_dataframe)
            #             write.tree.commented(tree, file, comments = node_notes, append = FALSE, digits = 10, tree.names = FALSE)
            #         }
            #         ## Save as a nexus
            #         if(input$output_type == "nexus") {
            #             tree$edge.length <- NULL
            #             states_dataframe <- make.output.data.frame(states_matrix)
            #             node_notes <- lapply(as.list(1:(states_matrix$n_tip + states_matrix$n_node)), create.note, states_dataframe)
            #             write.nexus.commented(tree, file, comments = node_notes, translate = TRUE)
            #         }
            #         ## Save as a C-test
            #         if(input$output_type == "C-test") {
            #             tree$edge.length <- NULL

            #             ## Setting the C variable name
            #             tree_var <- "char *test_tree"
            #             char_var <- "char *test_matrix"
            #             node_var <- "int node_pass"
            #             node_var <- paste0(node_var, 1:4, "[", states_matrix$n_tip + states_matrix$n_node, "] = ")

            #             ## Translate the tip labels
            #             if(!all(tree$tip.label == "numeric")) {
            #                 if(length(grep("t", tree$tip.label)) != 0) {
            #                     tree$tip.label <- gsub("t", "", tree$tip.label)
            #                 } else {
            #                     tree$tip.label <- seq(1:states_matrix$n_tip)
            #                 }
            #             }

            #             ## Get the newick tree
            #             newick_tree_out <- paste0(tree_var, " = \"", ape::write.tree(tree), "\";")

            #             ## Get the matrix
            #             ## Get all the possible states (for ?)
            #             all_states <- sort(unique(unlist(states_matrix$Char)))
            #             ## Converts the missing data
            #             raw_matrix <- lapply(states_matrix$Char, get.missing, all_states)
            #             ## Collapse multiple states
            #             raw_matrix <- unlist(lapply(raw_matrix, paste, collapse = ""))
            #             ## Convert the NA
            #             raw_matrix <- gsub("-1", "-", raw_matrix)
            #             ## C output
            #             raw_matrix_out <- paste0(char_var, " = \"", paste(raw_matrix, collapse = ""), "\";")

            #             ## Get the node array
            #             node_values <- lapply(lapply(states_matrix[2:5], convert.binary.value, states_matrix), unlist)

            #             ## Get the right traversal order here
            #             if(input$traversal_order == "") {
            #                 traversal_order <- seq(from = 1, to = states_matrix$n_tip + states_matrix$n_node)
            #             } else {
            #                 traversal_order <- as.numeric(unlist(strsplit(input$traversal_order, ",")))
            #             }

            #             ## Sort the passes by traversal
            #             passes_values <- list()
            #             for(pass in 1:4) {
            #                 passes_values[[pass]] <- node_values[[pass]][traversal_order]
            #             }

            #             ## Get the node values in C format
            #             C_node_values <- lapply(passes_values, function(x) paste0("{", paste(x, collapse = ", "), "};"))
            #             C_node_values <- mapply(paste0, as.list(node_var), C_node_values)

            #             ## Combine both outputs
            #             txt_out <- c(raw_matrix_out, newick_tree_out, unlist(C_node_values))
            #             writeLines(txt_out, file)
            #         }
            #     }
            # )
        })

        ## Output plot
        output$plot.ui <- renderUI({

            ## Reset the seed when hitting the refresh button
            set.seed(seeds[(input$refresh)+1])

            plot(1,1)

            space <- get.space(input, session)
            print(head(space))
            
            # if (class(tree) == "character") {
            #     plotOutput("plot_out", width ="100%", height = "40px")
            #     plotError(tree)
            # } else {
            #     n_tip <- length(tree$tip.label)
            #     ## Set the plot window
            #     plotOutput("plot_out", width ="100%", height = paste(round((n_tip + 3.3) * 0.4) * 90L, "px", sep = ""))
            # }
        })
    }
)
