
shinyUI(fluidPage(
  theme = 'slimline.css',

  wellPanel(

    titlePanel("moms: Measuring Occupancy in Multidimensional Spaces"),
    p("Guillerme T, Puttick MN, Marcy AE, and Weisbecker V (2020). Shifting spaces: which disparity or dissimilarity measurement best summarise occupancy in multidimensional spaces? doi:", a(href="https://doi.org/10.1002/ece3.6452", "10.1002/ece3.6452"), "."),
    p(a(href="https://rawcdn.githack.com/TGuillerme/moms/master/inst/moms_vignette.html", "USER MANUAL", rel = "noopener noreferrer", target = "_blank")),
    hr(),

    fluidRow(

      ## Side Bar
      column(width = 3,



        h2("Multidimensional space parameters"),

        selectInput("space_type", label = h3("Select the type of space to use:"),
                    choices = list(
                                  "User",
                                  "Input",
                                  "Demo"
                                  ), selected = "User"),

        ## ---------------
        ## Simulate space
        ## ---------------
        conditionalPanel(condition = "input.space_type == \"User\"",

          ## Number of dimensions input - input$n_dimensions
          sliderInput("n_dimensions", label = "Number of dimensions (traits):", min = 2, max = 100, value = 3),
          ## Number of elements input - input$n_elements
          sliderInput("n_elements", label = "Number of elements (observations):", min = 3, max = 1000, value = 300),

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
            numericInput("runif_min", label = "minimum", value = -0.5),
            numericInput("runif_max", label = "maximum", value = 0.5)
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
          selectInput("correlation", label = h5("Dimensions correlation"), choices = list("Uncorrelated", "Vector", "Upload"), selected = "Uncorrelated"),
          conditionalPanel(condition = "input.correlation == \"Vector\"",
            textInput("correlation_value_vector", label = "Correlations", value = "0.1,0.2,0.3"),
            ## Parameters
            helpText("Enter the correlation between each axis as the lower diagonal of a correlation matrix (separated by a comma). For example, for three dimensions D1, D2, D3, D4 enter 0.1,0.2,0.3,0.4,0.5,0.6 for a correlation of respectively 0.1 for D1 and D2; 0.2 for D1,D3; 0.3 for D1,D4;, 0.4 for D2,D3; 0.5 for D2,D4; and 0.6 for D3,D4.")
          ),
          # conditionalPanel(condition = "input.correlation == \"Matrix\"",
          #   shinyMatrix::matrixInput(inputId = "cor.matrix", value = diag(3), class = "numeric"),
          #   ## Parameters
          #   helpText("Enter the correlation value between each axis. The upper triangle and the diagonal are ignored. Note that this input option is ignored if using more than 15 dimensions.")
          # ),
          conditionalPanel(condition = "input.correlation == \"Upload\"",
            ## Parameters
            fileInput("correlation_value_csv", label = "Select a matrix in csv format."),
            helpText("The matrix must have no header. The diagonal and the upper triangle are ignored.")
          )
        ),
          


        ## ---------------
        ## Input space
        ## ---------------
        conditionalPanel(condition = "input.space_type == \"Input\"",

          fileInput("upload_input_matrix", label = "Select a multidimensional matrix in csv format."),
          helpText("Upload your own multidimensional matrix! The matrix must be in .csv format, with numeric values no NAs.")
        ),



        ## ---------------
        ## Demo spaces
        ## ---------------
        conditionalPanel(condition = "input.space_type == \"Demo\"",
          selectInput("demo_data", label = h5("Select a demo matrix:"),
                      choices = list(
                                    "Beck and Lee 2014",
                                    "Wright 2017",
                                    "Marcy et al. 2016",
                                    "Hopkins et al. 2016",
                                    "Jones et al. 2015",
                                    "Healy et al. 2019"
                                    ), selected = "Beck and Lee 2014"),

          conditionalPanel(condition = "input.demo_data == \"Beck and Lee 2014\"",
              helpText("A palaeobiology study of mammals. The data is a 105 dimensions ordination (PCO) of the distances between 106 mammals based on discrete morphological characters.
                Beck, R. M., & Lee, M. S. (2014). Ancient dates or accelerated rates? Morphological clocks and the antiquity of placental mammals. Proceedings of the Royal Society B: Biological Sciences, 281(1793), 20141278.")
              # Ref: google scholar Harvard format
              ),
          conditionalPanel(condition = "input.demo_data == \"Wright 2017\"",
              helpText("A palaeobiology study of crinoids. The data is a 41 dimensions ordination (PCO) of the distances between 42 crinoids based on discrete morphological characters.
                Wright, D. F. (2017). Bayesian estimation of fossil phylogenies and the evolution of early to middle Paleozoic crinoids (Echinodermata). Journal of Paleontology, 91(4), 799-814.")
              ),
          conditionalPanel(condition = "input.demo_data == \"Marcy et al. 2016\"",
              helpText("A geometric morphometric study of gophers (rodents). The data is a 134 dimensions ordination (PCA) the Procrustes superimposition of landmarks from 454 gopher skulls.
                Marcy, A. E., Hadly, E. A., Sherratt, E., Garland, K., & Weisbecker, V. (2016). Getting a head in hard soils: convergent skull evolution and divergent allometric patterns explain shape variation in a highly diverse genus of pocket gophers (Thomomys). BMC evolutionary biology, 16(1), 207.")
              ),
          conditionalPanel(condition = "input.demo_data == \"Hopkins et al. 2016\"",
              helpText("A geometric morphometric study of trilobites. The data is a 134 dimensions ordination (PCA) the Procrustes superimposition of landmarks from 46 trilobites cephala.
                Hopkins, M.J. and Pearson, J.K., 2016. Non-linear ontogenetic shape change in Cryptolithus tesselatus (Trilobita) using three-dimensional geometric morphometrics. Palaeontologia Electronica, 19(3), pp.1-54.")
              ),
          conditionalPanel(condition = "input.demo_data == \"Jones et al. 2015\"",
              helpText("An ecological landscape study. The data is a 47 dimensions ordination (PCO) of the Jaccard distances between 48 field sites based on species composition.
                Jones, N. T., Germain, R. M., Grainger, T. N., Hall, A. M., Baldwin, L., & Gilbert, B. (2015). Dispersal mode mediates the effect of patch size and patch connectivity on metacommunity diversity. Journal of Ecology, 103(4), 935-944.")
              ),
          conditionalPanel(condition = "input.demo_data == \"Healy et al. 2019\"",
              helpText("A life history analysis of the pace of life in animals. The data is a 6 dimensions ordination (PCA) of 6 life history traits from 285 animal species.
                Healy, K., Ezard, T.H., Jones, O.R., Salguero-Gomez, R. and Buckley, Y.M., 2019. Animal life history is shaped by the pace of life and the distribution of age-specific mortality and reproduction. Nature ecology & evolution, p.1.")
              )
        ),



        hr(),
        ## --------------------
        ## Disrupt space
        ## --------------------

        ## Modify space - input$reduce
        selectInput("reduce", label = h2("Space modification"), choices = list("None", "Random", "Size", "Position", "Density", "Evenness"), selected = "None"),

        conditionalPanel(condition = "input.reduce == \"Size\"", helpText("Expecting changes in size.")),
        conditionalPanel(condition = "input.reduce == \"Position\"", helpText("Expecting changes in position.")),
        conditionalPanel(condition = "input.reduce == \"Density\"", helpText("Expecting changes in density.")),
        conditionalPanel(condition = "input.reduce == \"Evenness\"", helpText("Expecting changes in density.")),

        ## All removals
        conditionalPanel(condition = "input.reduce != \"None\"",
          sliderInput("remove", label = "Proportion to remove:", min = 0.1, max = 0.9, value = 0.5),
          checkboxInput("inverse_remove", label = "Inverse removal", value = FALSE),

          conditionalPanel(condition = "input.scree != \"Uniform\"",
              checkboxInput("proportion_remove", label = "Proportional removal", value = FALSE),
              helpText("Whether to remove data uniformly across each dimensions (untick) or proportional (tick).")
            ),

          conditionalPanel(condition = "input.space_type == \"Demo\"",
              checkboxInput("use_demo_groups", label = "Use demo groups", value = FALSE),

            conditionalPanel(condition = "input.use_demo_groups == true",

              conditionalPanel(condition = "input.demo_data == \"Beck and Lee 2014\"",
                  helpText("The data is divided into two groups, the stem and crown mammals.")
                  ),
              conditionalPanel(condition = "input.demo_data == \"Wright 2017\"",
                  helpText("The data is divided into two groups, crinoids before and after the Ordovician-Silurian extinction (422.5 Mya).")
                  ),
              conditionalPanel(condition = "input.demo_data == \"Marcy et al. 2016\"",
                  helpText("The data is divided into two groups, the genera Megascapheus and Thomomys.")
                  ),
              conditionalPanel(condition = "input.demo_data == \"Hopkins et al. 2016\"",
                  helpText("The data is divided into two groups, juveniles (specimens with a log centroid size < 2.3) and adults.")
                  ),
              conditionalPanel(condition = "input.demo_data == \"Jones et al. 2015\"",
                  helpText("The data is divided into two groups, the aspen grassland sites.")
                  ),
              conditionalPanel(condition = "input.demo_data == \"NONAME2\"",
                  helpText("The data is divided into two groups, ectothermic and endothermic animals.")
                  )
                ),
            conditionalPanel(condition = "input.use_demo_groups == false",
              helpText("Tick to use the groupings from the demo data.")
              )
            )

        )

        # hr(),
        # ## -------
        # ## Placeholder for dispRity metrics
        # ## -------

      ),

      ## Main panel
      column(width = 4,
        ## Plots the algorithm results
        # uiOutput("plot.ui", width = "auto"),

        ## Dynamic plot window resizing
        tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                      Shiny.onInputChange("innerWidth", window.innerWidth);
                      });
                      $(window).resize(function(e) {
                      Shiny.onInputChange("innerWidth", window.innerWidth);
                      });
                      ')),

        plotOutput("plot_out", width = "100%", height = "auto"),    
        h3("Multidimensional space occupancy (disparity):"),
        tableOutput("table_out"),

        h3("Extra analyses:"),
        helpText("The following extra analyses are based on ", a(href="https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.6452", "(Guillerme et al. 2020)", rel = "noopener noreferrer", target = "_blank"), ". WARNING: they can take between few seconds to several minutes to run depending on the parameters and the shiny app traffic."),

        actionButton("simulate", label = "Run simulations"),
        # checkboxInput("simulate", label = "Run simulations", value = FALSE),
        helpText("Runs simulations for the space modification and compare them to random change."),
        conditionalPanel(condition = "input.simulate > 0",
          conditionalPanel(condition = "input.reduce != \"None\"",
            conditionalPanel(condition = "input.space_type == \"User\"",
              plotOutput("plot_simulations")
            ),
            conditionalPanel(condition = "input.space_type != \"User\"",
                helpText("Simulations can only run for user defined spaces. Select \"User\" in the \"Select the type of space to use:\" panel and rerun the simulations.")
            )                  
          ),
          conditionalPanel(condition = "input.reduce == \"None\"",
            helpText("Simulations can only run for a selected space modification.")
          )
        ),

        actionButton("testmetric", label = "Test metric"),
        helpText("Tests whether the metric captures the space modification. A slope indicates changes in the metric relative to the trait space size."),
        conditionalPanel(condition = "input.testmetric > 0",
          conditionalPanel(condition = "input.reduce != \"None\"",
            plotOutput("plot_testmetric")                 
          ),
          conditionalPanel(condition = "input.reduce == \"None\"",
            helpText("Simulations can only run for a selected space modification.")
          )
        )
    ),

      ## Left panel
      column(width = 3,

        ## -------
        ## Disparity metric (and display)
        ## -------
        h2("Space occupancy metric (disparity)"),

        ## Metric - input$level1
        selectInput("metric_choice", label = "Metric type", choices = list("Size", "Density", "Position", "User"), selected = "Size"),

        conditionalPanel(condition = "input.metric_choice == \"Size\"",
          selectInput("metric1", label = h5("Size metric"),
                      choices = list(
                                    "Ellipsoid volume",
                                    "Convex hull surface",
                                    "Convex hull volume",
                                    "Median distance from centroid (Euclidean)",
                                    "Median distance from centroid (Manhattan)",
                                    "n-ball volume",
                                    "Procrustes variance (geomorph::morphol.disparity)",
                                    "Product of quantiles",
                                    "Product of ranges",
                                    "Product of variances",
                                    "Sum of quantiles",
                                    "Sum of ranges",
                                    "Sum of variances"
                                    ), selected = "Sum of variances")
        ),

        conditionalPanel(condition = "input.metric_choice == \"Density\"",
          selectInput("metric2", label = h5("Density metric"),
                      choices = list(
                                    "Average Manhattan distance (geiger::dtt)",
                                    "Average squared Euclidean distance (geiger::dtt)",
                                    "Average nearest neighbours distance (Euclidean)",
                                    "Average nearest neighbours distance (Manhattan)",
                                    "Functional divergence (Villéger et al. 2008)",
                                    "Functional evenness (Villéger et al. 2008)",
                                    "Median pairwise distance (Euclidean)",
                                    "Median pairwise distance (Manhattan)",
                                    "Minimum spanning tree average length",
                                    "Nearest neighbours standard deviation (Euclidean)",
                                    "Nearest neighbours standard deviation (Manhattan)"
                                    ), selected = "Median nearest neighbours distance (Euclidean)")
        ),

        conditionalPanel(condition = "input.metric_choice == \"Position\"",
          selectInput("metric3", label = h5("Position metric"),
                      choices = list(
                                    "Angles deviations",
                                    "Average displacement (Euclidean)",
                                    "Average displacement (Manhattan)",
                                    "Deviations variation coefficient",
                                    "Median distance from centre (Euclidean)",
                                    "Median distance from centre (Manhattan)"
                                    ), selected = "Median displacement (Euclidean)")
        ),

        conditionalPanel(condition = "input.metric_choice == \"User\"",
          selectInput("metric_specific1", label = "Dimension level 1 metrics:",
                      choices = list("convhull.volume",
                                    "convhull.surface",
                                    "diagonal",
                                    "ellipse.volume",
                                    "func.div",
                                    "func.eve",
                                    "max",
                                    "mean",
                                    "median",
                                    "min",
                                    "n.ball.volume",
                                    "prod",
                                    "sd",
                                    "sum",
                                    "mode.val",
                                    "NULL"), selected = "mean"),
          selectInput("metric_specific2", label = "Dimension level 2 metrics:",
                      choices = list("NULL",
                                    "angles",
                                    "centroids",
                                    "deviations",
                                    "displacements",
                                    "neighbours",
                                    "pairwise.dist",
                                    "quantiles",
                                    "radius",
                                    "ranges",
                                    "variances",
                                    "span.tree.length"), selected = "NULL"),
          helpText("Select one or two metrics (e.g. 'ellipsoid.volume' and 'NULL' or 'sum' and 'variances').")
        ),

        ## Sampling
        checkboxInput("rarefaction", label = "Change sampling (rarefaction)", value = FALSE),
        conditionalPanel(condition = "input.rarefaction == true",
          sliderInput("n_rarefaction", label = "Sampling proportion (%):", min = 1, max = 100, value = 100),
          helpText("Changing the proportion of the number of elements to sample in both groups (down to three elements).")
          ),

        ## Show the metric
        checkboxInput("show_metric", label = "Show metric code", value = FALSE),
        conditionalPanel(condition = "input.show_metric == true",
          textAreaInput("manually_show_metric", label = NULL, value = "user.metric <- function(matrix) {\n\tsum(variances(matrix))\n}"),

          ## Edit the metric
          checkboxInput("edit_metric", label = "Edit metric", value = FALSE),
            conditionalPanel(condition = "input.edit_metric == true",
              textAreaInput("manually_edit_metric", label = NULL, value = "copy/paste and edit the function above."),
              helpText("Input your very own occupancy metric as a function that takes the argument <matrix> and output a single numeric value. Note that you can pre-populate this window by selecting Dimension level 1 or 2 metrics.")
              )
        ),

        p("You can find more informations about the metric types in the ", a(href="https://tguillerme.github.io/dispRity.html", "dispRity manual", rel="noopener noreferrer", target="_blank"), "."),

        # actionButton("add.metric", label = "Add a metric"),
        # actionButton("remove.metric", label = "Remove last metric"),

        # h4("dispRity code snippets"),
        # # selectInput("metric_choice", label = "Output type", choices = list("R code snippet", "R code file", "R markdown file"), selected = "R code snippet"),
        # # helpText("This interface is based on three functions: dispRity::space.maker to generate parametrised multidimensional spaces, moms::reduce.space to remove elements from a multidimensional space using various removal algorithm and dispRity::dispRity to measure disparity! Have a look at each functions manuals in R or on the related vignettes."),
        # checkboxInput("display_code_snippet", label = "Display code", value = FALSE),
        # conditionalPanel(condition = "input.display_code_snippet == true",
        #   selectInput("export_code_snippet", label = "Export snippet", choices = list("In app display", "R code", "R markdown"), selected = "In app display"),
        #   conditionalPanel(condition = "input.export_code_snippet == \"In app display\"",
        #     ## Display code in line
        #     verbatimTextOutput("code_snippet")
        #   )
        # ),
  
        hr(),

        h3("Display"),
        numericInput("axis_1", label = h5("Horizontal axis"), value = 1, min = 1),
        numericInput("axis_2", label = h5("Vertical axis"), value = 2, min = 1),
        selectInput("color_scheme", label = h5("Colours"), choices = list("Greyscale", "Contrast", "Pink", "Rainbow"), selected = "Greyscale"),
        checkboxInput("scale_axis", label = "Axis with the same scale", value = FALSE),
        
        ## Refresh button - input$refresh
        hr(),
        actionButton("refresh", label = "Refresh"),

        # Export code
        hr(),
        downloadButton("export.code", "Export code")
      )
    )
  )
))



