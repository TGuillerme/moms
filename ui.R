
shinyUI(fluidPage(
  theme = 'slimline.css',

  wellPanel(

    titlePanel("Measuring Occupancy in Multidimensional Spaces"),
    p("Guillerme, T., Puttick, M., and Weisbecker, V (2019). Some paper. Some journal. doi:", a(href="https://dx.doi.org/10.1093/sysbio/syy083", "some DOI"), "."),
    hr(),

    fluidRow(

      ## Side Bar
      column(width = 4,

        ## ---------------
        ## Space parameters
        ## ---------------

        h3("Multidimensional space parameters"),
        ## Number of dimensions input - input$n_dimensions
        sliderInput("n_dimensions", label = "Number of dimensions:", min = 2, max = 100, value = 3),
        ## Number of elements input - input$n_elements
        sliderInput("n_elements", label = "Number of elements:", min = 3, max = 1000, value = 300),

        ## Distributions - input$distributions
        selectInput("distributions", label = "Distributions", choices = list("Normal", "LogNormal", "Uniform", "Gamma", "Poisson", "Specific"), selected = "Normal"),

        ## Normal parameters
        conditionalPanel(condition = "input.distributions == \"Normal\"",
          ## Parameters
          numericInput("rnorm_mean", label = "mean", value = 0),
          numericInput("rnorm_sd", label = "standard deviation", value = 1)
        ),

        ## LogNormal parameters
        conditionalPanel(condition = "input.distributions == \"LogNormal\"",
          ## Parameters
          numericInput("rlnorm_mean", label = "mean log", value = 0),
          numericInput("rlnorm_sd", label = "standard deviation log", value = 1)
        ),

        ## Uniform parameters
        conditionalPanel(condition = "input.distributions == \"Uniform\"",
          ## Parameters
          numericInput("runif_min", label = "minimum", value = 0),
          numericInput("runif_max", label = "maximum", value = 1)
        ),

        ## Gamma parameters
        conditionalPanel(condition = "input.distributions == \"Gamma\"",
          ## Parameters
          numericInput("rgamma_shape", label = "shape (alpha)", value = 5),
          numericInput("rgamma_rate", label = "rate (beta)", value = 1)
        ),

        ## Poisson parameters
        conditionalPanel(condition = "input.distributions == \"Poisson\"",
          ## Parameters
          numericInput("rpois_lambda", label = "lambda", value = 5)
        ),

        ## Multiple distributions
        conditionalPanel(condition = "input.distributions == \"Specific\"",
          ## Distributions for D1
          textInput("distribution_list", label = "Distribution list", value = "rnorm, runif, rlnorm"),
          helpText("Enter the distributions as a list of function names from the 'stats' package separated by a comma (e.g. rnorm, runif for the normal distribution to be applied to D1 and the uniform distribution to by applied to D2). Make sure you enter the same number of distribution functions as the number of requested distributions.")
          ## TODO: add optional arguments
        ),

        ## Scree - input$scree
        selectInput("scree", label = h5("Dimensions variance"), choices = list("Uniform", "Decreasing", "LogNormal"), selected = "Uniform"),
        conditionalPanel(condition = "input.scree == \"Uniform\"",
          ## Parameters
          helpText("Variance is the same on each axis.")
        ),
        conditionalPanel(condition = "input.scree == \"Decreasing\"",
          ## Parameters
          helpText("Variance is uniformly decreasing on each axis.")
        ),
        conditionalPanel(condition = "input.scree == \"LogNormal\"",
          ## Parameters
          helpText("Variance is log-normally decreasing on each axis.")
        ),

        ## Correlation - input$correlation
        # selectInput("correlation", label = "Dimensions correlation", choices = list("Uniform", "Decreasing", "LogNormal"), selected = "Uniform"),
        # helpText("Variance is uniformly decreasing on each axis.")
        # TODO: add correlation profiles.

        ## --------------------
        ## Disrupt space
        ## --------------------

        ## Modify space - input$reduce
        selectInput("reduce", label = h3("Space modification"), choices = list("None", "Random", "Limit", "Displace", "Density"), selected = "None"),

        ## All removals
        conditionalPanel(condition = "input.reduce != \"None\"",
          sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5),
          checkboxInput("inverse_remove", label = "Inverse removal", value = FALSE),

          conditionalPanel(condition = "input.scree != \"Uniform\"",
              checkboxInput("proportion_remove", label = "Proportional removal", value = FALSE),
              helpText("Whether to remove data uniformly across each dimensions (untick) or proportional (tick).")
            )

        ),

        # ## Limit removal
        # conditionalPanel(condition = "input.reduce == \"Limit\"",
        #   # sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5),
        #   ## input$optimise
        #   # numericInput("optimise", label = h5("Radius around the centre"), value = 0),
        #   # helpText("The radius around the centre from which to keep or select the points. If this is left empty, it is estimated automatically to match the requested proportion."),
        #   ## input$reduce_type -> TRUE = to_remove, FALSE = !to_remove
        #   checkboxInput("reduce_type", label = "Remove outside the radius", value = FALSE)
        # ),

        ## -------
        ## Disparity metric (and display)
        ## -------
        h3("Disparity metric"),

        ## Metric - input$level1
        selectizeInput("metric", label = "Metric 1", choices = list("centroids", "diagonal", "ellipse.volume", "max", "mean", "median", "min", "n.ball.volume", "pairwise.dist", "prod", "ranges", "sd", "span.tree.length", "sum", "variances"), multiple = TRUE, options = list(maxItems = 2)),
        helpText("Select one or two metrics (e.g. 'ellipsoid.volume' or 'sum variances').")

      ),

      ## Main panel
      column(width = 6,
        ## Plots the algorithm results
        # uiOutput("plot.ui", width = "auto"),

        tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                      Shiny.onInputChange("innerWidth", window.innerWidth);
                      });
                      $(window).resize(function(e) {
                      Shiny.onInputChange("innerWidth", window.innerWidth);
                      });
                      ')),

        plotOutput("plot_out", width = "100%", height = "auto"),
      
        # fluidRow(  #TG: toggle on if necessary
          # column(5, offset = 2,
            ## Some results
            h4("Observations"),
            tableOutput("table_out")
        #   )
        # )
      ),

      ## Main panel
      column(width = 2,
        h3("Display"),
        numericInput("axis_1", label = h5("Horizontal axis"), value = 1, min = 1),
        numericInput("axis_2", label = h5("Vertical axis"), value = 2, min = 1),
        
        ## Refresh button - input$refresh
        hr(),
        actionButton("refresh", label = "Refresh")

      )
    )
  )
))



