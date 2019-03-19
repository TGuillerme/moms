
shinyUI(fluidPage(
  theme = 'slimline.css',

  wellPanel(

    titlePanel("Measuring Occupancy in Multidimensional Spaces"),
    p("Guillerme, T., Puttick, M., and Smith, M. R. (2019). Some paper. Some journal. doi:", a(href="https://dx.doi.org/10.1093/sysbio/syy083", "some DOI"), "."),
    hr(),

    fluidRow(

        ## ---------------
        ## Space parameters
        ## ---------------
        column(width = 4,
          h3("Multidimensional space parameters"),
          ## Number of dimensions input - input$n_dimensions
          sliderInput("n_dimensions", label = "Number of dimensions:", min = 2, max = 100, value = 3),
          ## Number of elements input - input$n_elements
          sliderInput("n_elements", label = "Number of elements:", min = 3, max = 1000, value = 300),

          ## Distributions - input$distributions
          selectInput("distributions", label = "Distributions", choices = list("Normal", "Log Normal", "Uniform", "Gamma", "Poisson", "Specific"), selected = "Normal"),

          ## Conditional distribution
          ## Conditional distribution
          conditionalPanel(condition = "input.distributions == \"Specific\"",
            ## Distributions for D1
            textInput("distribution_list", label = h5("Distribution list"), value = "rnorm, runif, rlnorm"),
            helpText("Enter the distributions as a list of function names from the 'stats' package separated by a comma (e.g. rnorm, runif for the normal distribution to be applied to D1 and the uniform distribution to by applied to D2). Make sure you enter the same number of distribution functions as the number of requested distributions.")
            ## TODO: add optional arguments
          )
        ),

        ## --------------------
        ## Disrupt space
        ## --------------------
        column(width = 4,
          ## Modify space - input$reduce
          selectInput("reduce", label = h3("Space modification"), choices = list("None", "Random", "Limit", "Displace", "Density"), selected = "None"),

          ## Random removal
          conditionalPanel(condition = "input.reduce == \"Random\"",
            sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5)
          ),

          ## Limit removal
          conditionalPanel(condition = "input.reduce == \"Limit\"",
            sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5),
            ## input$limit_optimise
            textInput("limit_optimise", label = h5("Radius around the centre")),
            helpText("The radius around the centre from which to keep or select the points. If this is left empty, it is estimated automatically to match the requested proportion."),
            ## input$reduce_type -> TRUE = to_remove, FALSE = !to_remove
            checkboxInput("reduce_type", label = "Remove outside the radius", value = FALSE)
          ),

          ## Displacement removal
          conditionalPanel(condition = "input.reduce == \"Displace\"",
            sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5),
            ## input$displacement_optimise
            textInput("displacement_optimise", label = h5("Threshold")),
            helpText("The threshold value from which to remove elements. If this is left empty, it is estimated automatically to match the requested proportion."),
            ## input$reduce_type -> TRUE = to_remove, FALSE = !to_remove
            checkboxInput("diplacement_type", label = "Remove below the threshold", value = FALSE)
          ),

          ## Displacement removal
          conditionalPanel(condition = "input.reduce == \"Density\"",
            sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5),
            ## input$displacement_optimise
            textInput("density_optimise", label = h5("Tolerance")),
            helpText("The tolerance from which to consider points are neighbours. If this is left empty, it is estimated automatically to match the requested proportion."),
            ## input$reduce_type -> TRUE = to_remove, FALSE = !to_remove
            checkboxInput("density_type", label = "Remove neighbours", value = FALSE)
          )
        ),

        ## -------
        ## Disparity metric (and display)
        ## -------
        column(width = 4,
          h3("Disparity metric"),

          ## Metric - input$level1
          selectInput("metric1", label = "Metric 1", choices = list("centroids", "diagonal", "ellipse.volume", "max", "mean", "median", "min", "n.ball.volume", "pairwise.dist", "prod", "ranges", "sd", "span.tree.length", "sum", "variances"), selected = "sum"),
          ## Metric - input$level2
          selectInput("metric2", label = "Metric 2", choices = list("centroids", "diagonal", "ellipse.volume", "max", "mean", "median", "min", "n.ball.volume", "pairwise.dist", "prod", "ranges", "sd", "span.tree.length", "sum", "variances"), selected = "variances"),

          hr(),
          ## Refresh button - input$refresh
          actionButton("refresh", label = "Refresh")

        )
    )
  ),

  ## Displaying the results
  fluidRow(
    ## Plots the algorithm results
    uiOutput("plot.ui")
  )

))



