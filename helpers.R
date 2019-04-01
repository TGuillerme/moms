## Display the error message
plot.error <- function(text, col = "#cc6644", font = 1, cex = 1.2, ...) {
    ## Empty plot
    plot(NULL, xlim = c(0, 1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
    ## Error message
    text(0.5, 0.5, paste("Error:", text), col = col, font = font, cex = cex, ...)
}