
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
          textInput("distribution_list", label = "Distribution list", value = "list(rnorm, runif, rlnorm)"),
          helpText("Enter the distributions as a list of function names from the 'stats' package (e.g. rnorm for the normal distribution to be applied to dimensions 1, etc.)."),
          checkboxInput("optional_arguments", label = "Optional arguments", value = FALSE),
          conditionalPanel(condition = "input.optional_arguments == true",
            ## Parameters
            textInput("distribution_arguments", label = "Argument list", value = "list(list(mean = 0, sd = 1), list(), list(meanlog = 5))"),
            helpText("Enter a list of arguments to be applied to each distribution above. The format should be \"c(list(arguments1), list(arguments2), etc..)\" where \"arguments1\" are used for the first distribution, etc. If one argument needs no specific argument (default), use an empty list: list().")
          )
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
        conditionalPanel(condition = "input.scree == \"Log-normal\"",
          ## Parameters
          helpText("Variance is log-normally decreasing on each axis.")
        ),

        ## Correlation - input$correlation
        selectInput("correlation", label = h5("Dimensions correlation"), choices = list("Uncorrelated", "Matrix", "Vector", "Upload"), selected = "Uncorrelated"),
        conditionalPanel(condition = "input.correlation == \"Vector\"",
          textInput("correlation_value_vector", label = "Correlations", value = "0.1,0.2,0.3"),
          ## Parameters
          helpText("Enter the correlation between each axis as the lower diagonal of a correlation matrix (separated by a comma). For example, for three dimensions D1, D2, D3, D4 enter 0.1,0.2,0.3,0.4,0.5,0.6 for a correlation of respectively 0.1 for D1 and D2; 0.2 for D1,D3; 0.3 for D1,D4;, 0.4 for D2,D3; 0.5 for D2,D4; and 0.6 for D3,D4.")
        ),
        conditionalPanel(condition = "input.correlation == \"Matrix\"",
          shinyMatrix::matrixInput(inputId = "cor.matrix", value = diag(3), class = "numeric"),
          ## Parameters
          helpText("Enter the correlation value between each axis. The upper triangle and the diagonal are ignored. Note that this input option is ignored if using more than 15 dimensions.")
        ),
        conditionalPanel(condition = "input.correlation == \"Upload\"",
          ## Parameters
          fileInput("correlation_value_csv", label = "Select a matrix in csv format."),
          helpText("The matrix must have no header. The diagonal and the upper triangle are ignored.")
        ),


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
        h3("Multidimensional space occupancy (disparity):"),
        tableOutput("table_out")
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



