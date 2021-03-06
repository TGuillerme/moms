% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vdp.R
\name{vdp.plot}
\alias{vdp.plot}
\title{Make vdp plot}
\usage{
vdp.plot(
  vdp,
  plots = 1:8,
  limits,
  pch = 19,
  xlab = "",
  ylab = "",
  disparity = NULL,
  plot.names,
  mfrow,
  ...
)
}
\arguments{
\item{vdp}{A list output from \code{\link{vdp.make}}}

\item{plots}{Which output from \code{\link{vdp.make}} to plot (default is \code{1:8} for all plots).}

\item{limits}{Optional, a set of plot limits}

\item{pch}{The dots type to plot (default = 19 - full round dots)}

\item{xlab, }{ylab The x and y labels (default is none - \code{""}).}

\item{disparity}{optional, disparity values obtained from \code{\link{vdp.dispRity}} to be displayed as x labels}

\item{plot.names}{optional, the plot names (passed as \code{main})}

\item{mfrow}{optional, the display disposition to be passed to \code{par(mfrow)}. If left empty, the nearest square matrix is used.}

\item{...}{any additional argument to be passed to \code{\link[base]{plot}}.}
}
\description{
Make a volume, density, position combination plot
}
\examples{
## Make a Volume/density/position list
vdp_list <- vdp.make()

## Plotting the transformations
vdp.plot(vdp_list)

## Calculate disparity
vdp_disp <- vdp.dispRity(vdp_list, volume = c(sum, variances),
                                   density = c(mean, neighbours),
                                   position = c(mean, displacements))

## Plotting the results with disparity
vdp.plot(vdp_list, disparity = vdp_disp)

}
\seealso{
vdp.make, vdp.dispRity
}
\author{
Thomas Guillerme
}
