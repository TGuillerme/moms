[![Build Status](https://travis-ci.org/TGuillerme/moms.svg?branch=master)](https://travis-ci.org/TGuillerme/moms)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/TGuillerme/moms/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/moms)
<!-- [![DOI](https://zenodo.org/badge/84838115.svg)](https://zenodo.org/badge/latestdoi/84838115)
 -->

**moms**: Measuring Occupancy in Multidimensional Space.

<!-- <a href="https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222). --> 

This shiny app helps understanding the multidimensional space occupancy metrics that can be used in ecology and evolution.
This app is based on the [`dispRity` `R` package](https://github.com/TGuillerme/dispRity).

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


Authors
-------
[Thomas Guillerme](http://tguillerme.github.io), [Mark Puttick](https://puttickbiology.wordpress.com/), [Vera Weisbecker](http://weisbeckerlab.com.au/)


<!-- Citations
-------
If you are using this package, please cite both the published description of this algorithm:

* Brazeau MD, Guillerme T, Smith MR. (**2018**) An algorithm for morphological phylogenetic analysis with inapplicable data. *Systematic Biology*. [doi:10.1093/sysbio/syy083](https://academic.oup.com/sysbio/advance-article/doi/10.1093/sysbio/syy083/5238046)

And the DOI of this package:

 * Guillerme T, Brazeau MD, Smith MR. (**2018**). moms: Reconstruction of momslicable Discrete Characters on Phylogenetic Trees. *Zenodo*. [doi:10.5281/zenodo.1484656](http://doi.org/10.5281/zenodo.1484656)
 -->