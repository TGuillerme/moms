[![Build Status](https://travis-ci.org/TGuillerme/moms.svg?branch=master)](https://travis-ci.org/TGuillerme/moms)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/TGuillerme/moms/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/moms)
<!-- [![DOI](https://zenodo.org/badge/84838115.svg)](https://zenodo.org/badge/latestdoi/84838115)
 -->

**moms**: Measuring Occupancy in Multidimensional Space.

<!-- <a href="https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222). --> 
### What is moms?
moms is a shiny app that helps understanding the multidimensional space occupancy metrics that can be used in ecology and evolution.

### What isn't moms?
**moms is not a package for measuring disparity!** 
Although moms is distributed as a binary `R` package, it only contains functions for running the shiny app and making the [paper associated with moms] reproducible.
These functions allow the reduce the space, make fancy plots or wrap up analysis in the context of the shiny app or the paper only.
If you want to measure disparity, we highly recommend the use of the [`dispRity` `R` package](https://github.com/TGuillerme/dispRity) which is used by moms for all the disparity calculations.

## Running moms on your web browser
That's easy! In `R`, simply copy/paste the following:
```r
if(!require(devtools)) install.packages("devtools")
if(!require(shiny)) install.packages("shiny")
runGitHub("moms", "TGuillerme")
```
<!-- Upload the whole thing on shiny servers -->

## Running moms in your R console
Not much more complicated! In `R`, copy/paste the following:
```r
## Installing the App:
if(!require(devtools)) install.packages("devtools")
devtools::install_github("TGuillerme/moms")

## Running the App:
moms::runmoms()
```

<!-- Check out the package [vignette](https://tguillerme.github.io/moms.html) for (many) more details on the GUI possibilities. -->


## Associated paper

By the following associated paper: [Shifting spaces: how to summarise multidimensional spaces occupancy?]().
This paper details the behaviour of several disparity metrics and discuss the advantages and disadvantages of using them (or not).
This paper is also **fully reproducible!**

### How to reproduce the paper?

To reproduce the paper you can follow these steps:

 1. Open the file [`/inst/shiftingspace.Rmd`](https://github.com/TGuillerme/moms/blob/master/inst/shiftingspace.Rmd) with your favourite Rmarkdown editor (Rstudio, Sublime, Atom, etc.).
 2. Press on the compile button.
 3. Your reproduced paper should now be in the `inst/` directory.

Alternatively, you can directly compile the paper in html from the `R` console using:

```r
## Compiling the paper in html
rmarkdown::render("inst/shiftingspace.Rmd", html_document())
```

Authors
-------
[Thomas Guillerme](http://tguillerme.github.io), [Mark Puttick](https://puttickbiology.wordpress.com/), [Ariel Marcy](https://github.com/miracleray), [Vera Weisbecker](http://weisbeckerlab.com.au/)


<!-- Citations
-------
If you are using this package, please cite both the published description of this algorithm:

* Guillerme T, Puttick M, Weisbecker V. (**2018**) Shifting spaces: how to summarise multidimensional spaces occupancy?. *Methods in Ecology and Evolution*. [doi:]()

And the DOI of this package:

 * Guillerme T, Puttick M, Weisbecker V. (**2018**). moms: Measuring Occupacy in Multidimensional Spaces. *Zenodo*. [doi:10.5281/zenodo.1484656](http://doi.org/10.5281/zenodo.1484656)
 -->
