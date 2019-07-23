#' @title Generates fable plots
#'
#' @description Generates the plots for the fable. The R markdown compiler should name them as "<snippet_name>-<ID>.<type>"
#'
#' @param data the disparity data
#' @param metric the metric ID
#' @param what the name of the chain as given pas the code snippet generating the plots
#' @param scale whether to scale and centre the results around the non-reduced spaces
#' @param overlap whether to add the Bhattacharyya Coefficients or not
#' @param plot.param the plot parameters
#'
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

generate.fable.plot <- function(data, metric, what, scale = TRUE, overlap = FALSE, plot.param) {

    pool.data <- function(data, metric, what, scale) {
        ## Extract the metric
        pooled_metric <- lapply(remove_05, function(X, metric) return(X[[metric]]), metric = metric)

        if(scale){
            ## Centre the results
            centre.fun <- function(result) {
                output <- apply(result, 2, function(X) as.numeric(X) - as.numeric(X)[1])
                output <- apply(output, 2, function(X) X/(max(abs(X))))
                output <- output[-1, ]
                rownames(output) <- rownames(result)[-1]
                return(output)
            }
            pooled_metric <- lapply(pooled_metric, centre.fun)
        }

        ## Extract the category
        get.category <- function(data, what, scale) {
            ## Get the right categories
            categories <- grep(what, rownames(data))
            ## Add the random category (number 2 or 1 - if scaled)
            categories <- c(ifelse(scale, 1, 2), categories)
            ## Return a matrix
            return(sapply(categories, function(X, data) {return(data[X,])}, data = data))
        }

        ## Get the pooled data
        pooled_data <- lapply(pooled_metric, get.category, what = what, scale = scale)
        return(do.call(rbind, pooled_data))
    }

    empty.plot <- function(plot.param, metric) {
        ## Plot margins
        par(bty = "n", mar = c(3,1,1,1))
        ## Plot size
        plot(NULL, ylim = c(0.8, 3.2), pch = 19, xlim = c(-1,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
        ## Adding lines
        abline(v = 0, lty = 2, col = plot.param$bg.col, lwd = 1 + plot.param$scaler)

        ## Adding the x axis
        if(metric != plot.param$metric.max) {
            axis(1, at = c(-1, -0.5, 0, 0.5, 1), labels = FALSE, tick = TRUE, col.ticks = plot.param$bg.col, col = plot.param$bg.col, lwd = 1 + plot.param$scaler)
        } else {
            axis(1, at = c(-1, -0.5, 0, 0.5, 1), labels = c(-1, NA, 0, NA, 1), tick = TRUE, col.ticks = plot.param$bg.col, col = plot.param$bg.col, lwd = 1 + plot.param$scaler, cex.axis = 0.5 + (plot.param$scaler)/2)
        }
    }

    add.data <- function(data, plot.param) {
        ## Get the quantiles
        quantiles <- sort(c(50-plot.param$quantiles/2, 50+plot.param$quantiles/2)/100)

        ## Get the X values (quantiles or central tendency)
        quantile_vals <- apply(data, 2, quantile, probs = quantiles, na.rm = TRUE)
        centtend_vals <- apply(data, 2, plot.param$cent.tend, na.rm = TRUE)

        ## Loop through the lines
        for(column in 1:ncol(data)) {
            ## Get the x y values
            line_x_vals <- quantile_vals[, column]
            line_y_vals <- rep(column, 2)

            ## Add the lines
            n_cis <- length(quantiles)
            for(ci in 1:(n_cis/2)) {
                lines(x = line_x_vals[c(ci, n_cis-(ci-1))], y = line_y_vals, lty = (n_cis/2 - ci + 1), lwd = ci * 1.5 * (1+plot.param$scaler), col = plot.param$col[column])
            }
        }

        ## Add the points
        points(x = centtend_vals, y = 1:ncol(data), pch = plot.param$pch, col = plot.param$col, cex = 1 + plot.param$scaler)

        return(invisible())
    }

    ## Get the data
    pooled_data <- pool.data(data, metric = metric, what = what, scale = scale)

    ## Plot the results
    empty.plot(plot.param, metric)
    add.data(pooled_data, plot.param)

    if(overlap) {
        ## Get the Bhattacharrya coefficients
        coefs <- apply(pooled_data, 2, function(X, data) return(bhatt.coeff(X, pooled_data[,1])))[-1]
        ## Round the coefficients
        coefs <- round(coefs, 2)
        ## y text positions
        y_pos <- c(1:ncol(pooled_data))[-1]
        ## x text positions
        x_pos <- vector()
        for(column in y_pos) {
            x_pos[column-1] <- ifelse(mean(pooled_data[, column]) < 0, 0.5, -0.5)
        }
        ## Add the text
        text(x = x_pos, y = y_pos, labels = coefs, cex = 1 + plot.param$scaler)

    }
}


#' @title Generates fable plots for the empirical data
#'
#' @description Generates the plots for the empirical example fable. The R markdown compiler should name them as "<snippet_name>-<ID>.<type>"
#'
#' @param data the disparity data
#' @param test the associated test with the data
#' @param plot.param the plot parameters
#' @param data_names the names of the datasets (to be plotted)
#' @param metrics_names the names of the metrics (to be plotted)
#' @param precision the value under which to consider values as zeros
#' @param precision the dataset number (to plot the y axis values or not)
#'
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
generate.fable.empirical <- function(data, test, precision, plot.param, dataset) {
    ## Scaling the results
    scale.vals <- function(disparity) {
        ## Centre
        centre <- function(x, min) return(x-min)
        min_val <- min(unlist(disparity$disparity))
        disparity$disparity <- lapply(disparity$disparity, lapply,
                                      centre, min = min_val)
        ## Scale
        scale <- function(x, max) return(x/max)
        max_val <- max(unlist(disparity$disparity))
        disparity$disparity <- lapply(disparity$disparity, lapply,
                                      function(x, max) x/max, max = max_val)
        return(disparity)
    }
    ## Get significance token
    get.token <- function(test) {

        if(is.nan(unlist(test)["p.value"]) ||
           is.na(unlist(test)["p.value"]) ||
           is.null(unlist(test)["p.value"])) {
            return("")
        }

        if(unlist(test)["p.value"] < 0.001) {
            return("***")
        } else {
            if(unlist(test)["p.value"] < 0.01) {
                return("**")
            } else {
                if(unlist(test)["p.value"] < 0.05) {
                    return("*")
                } else {
                    if(unlist(test)["p.value"] < 0.1) {
                        return(".")
                    } else {
                        return("")
                    }
                }
            }
        }
    }

    ## Scale the data
    data <- scale.vals(data)

    if(any(is.nan(unlist(data$disparity))) || all(unlist(data$disparity) < precision)) {
        ## Empty plot
        par(bty = "n")
        plot(NULL, ylim = c(0,1), xlim = c(0,1), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
        text(0.5, 0.5, "NA", cex = plot.param$na.cex)
    }  else {
        ## Setting the margins
        par(bty = "l", mar = c(1, (2 +(plot.param$scaler)/2), 1, 1))
        ## Plotting the results
        plot(data, col = plot.param$col, border = plot.param$border, las = 1,
            ylim = c(0,1),
            xaxt = "n",
            xlab = "",
            ylab = "",
            yaxt = "n",
            main = "",
            lwd = 1 + plot.param$scaler)
        ## Add the yaxis
        axis(2, at = seq(from = 0, to = 1, by = 0.2), labels = ifelse(dataset == 1, TRUE, FALSE), tick = TRUE, lwd = 1 + plot.param$scaler, cex.axis = 0.5 + (plot.param$scaler)/2, las = 2)
    
        ## Adding the difference (or not)
        text(1.5, 0.9, get.token(test), cex = plot.param$cex + plot.param$scaler)
    }
}

#' @title Plotting distribution by ID
#'
#' @description Plots one distribution for the fable following the plot ID
#'
#' @param plot.ID the number of the plot as generated by generate the R markdown compiler
#' @param path where the plots are generated by the compiler
#' @param chain the name of the chain as given pas the code snippet generating the plots
#' @param resolution the plot resolution in the fable
#' @param type the file type (e.g. ".png")
#'
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
plot.id <- function(plot.ID, path = "fig.path", resolution = 600) {
    type <- ".pdf"
    chain <- "fable_results"
    knitr::include_graphics(path = paste0(opts_chunk$get(path), paste0(chain, "-", plot.ID, type)),
                            dpi = resolution)
}

#' @title Display p-value from test
#'
#' @description Displays the p-value and significance token in the fable from the tests
#'
#' @param metric.ID The ID of the metric (i.e. row number)
#' @param type "s" or "r" for "space distributions" or "rarefaction"
#' @param test.results a list containing the results for either "s" or "r" mentioned above
#'
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
s.test <- function(metric.ID, type, test.results = list("s" = space_test, "r" = all_dim_test)) { 
    
    ## Select the results
    test_results <- test.results[[type]]
    ## Coerce the results into a data.frame
    print_results <- test_results[[metric.ID]][[1]]
    class(print_results) <- "data.frame"
    ## Rounding
    print_results <- as.matrix(round(print_results, digits = 3))
    ## White spaces
    rownames(print_results) <- c("f.", "res.")
    print_results <- ifelse(is.na(print_results), "", print_results)
    ## Add the tokens
    p_val <- as.numeric(print_results[1,5])

    if(is.na(p_val)){
        return(noquote("NA"))
    }

    if(p_val < 0.001) {
        # print_results <- cbind(print_results, c("***", ""))
        signif_token <- "***"
    } else {
        if(p_val < 0.01) {
            # print_results <- cbind(print_results, c("**", ""))
          signif_token <- "**"
        } else {
            if(p_val < 0.05) {
                # print_results <- cbind(print_results, c("*", ""))
              signif_token <- "*"
            } else {
                if(p_val < 0.1) {
                    # print_results <- cbind(print_results, c(".", ""))
                    signif_token <- "."
                } else {
                    # print_results <- cbind(print_results, c("", ""))
                    signif_token <- ""
                }
            }
        }
    }
    ## Display the results
    output <- paste("p = ", p_val, signif_token)
    return(noquote(output))
}