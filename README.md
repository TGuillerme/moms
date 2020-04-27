[![Build Status](https://travis-ci.org/TGuillerme/moms.svg?branch=master)](https://travis-ci.org/TGuillerme/moms)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3466146.svg)](https://doi.org/10.5281/zenodo.3466146)


# **moms**: Measuring Occupancy in Multidimensional Space.

<!-- <a href="https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222). --> 
### What is `moms`?
`moms` is a shiny app that helps understanding the multidimensional space occupancy metrics that can be used in ecology and evolution.

### What isn't `moms`?
**`moms` is not a package for measuring disparity!** 
Although `moms` is distributed as a binary `R` package, it only contains functions for running the shiny app and making the [paper associated with `moms`](https://www.biorxiv.org/content/10.1101/801571v1) fully reproducible.
These functions allow the reduce the space, make fancy plots or wrap up analysis in the context of the shiny app or the paper only.
If you want to measure disparity, we highly recommend the use of the [`dispRity` `R` package](https://github.com/TGuillerme/dispRity) which is used by `moms` for all the disparity calculations.

Check out the <a href="https://figshare.com/articles/Shifting_spaces_which_disparity_or_dissimilarity_metrics_best_summarise_occupancy_in_multidimensional_spaces_/9922961"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
[presentation](https://figshare.com/articles/Shifting_spaces_which_disparity_or_dissimilarity_metrics_best_summarise_occupancy_in_multidimensional_spaces_/9922961) and the <a href="https://figshare.com/articles/Shifting_spaces_which_disparity_or_dissimilarity_metrics_best_summarise_occupancy_in_multidimensional_spaces_/9922961"><img src="http://tguillerme.github.io/images/OA.png" height="15" widht="15"/></a> 
[preprint](https://www.biorxiv.org/content/10.1101/801571v1) associated with this project.

## Running `moms` online
Super easy! Simply click on the following link:

<a href="https://tguillerme.shinyapps.io/moms/"><img src="http://tguillerme.github.io/images/shiny.png" height="100" widht="100"/></a>

## Running `moms` localy on your web browser
Not hard! In `R`, simply copy/paste the following:
```r
if(!require(devtools)) install.packages("devtools")
if(!require(shiny)) install.packages("shiny")
runGitHub("moms", "TGuillerme")
```

## Running `moms` in your R console
Not much more complicated! In `R`, copy/paste the following:
```r
## Installing the App:
if(!require(devtools)) install.packages("devtools")
devtools::install_github("TGuillerme/moms")

## Running the App (from the root of the repository, i.e. <some_path_in_my_machine>/moms/:
shiny::runApp(".")
```

Check out the app [manual](https://raw.githack.com/TGuillerme/moms/master/inst/moms_vignette.html) for more details on `moms` GUI possibilities.


## Associated paper

By the following associated paper: [Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces?](https://www.biorxiv.org/content/10.1101/801571v1)
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


Citations
-------
If you are using this package, please cite (if the DOI is in there, even better!):

* Guillerme, T., Puttick MN., Marcy AE., & Weisbecker V. (**2019**). moms: measuring occupancy in multidimensional spaces. ZENODO. http://doi.org/10.5281/zenodo.3466146

    ###### [BibTeX](https://zenodo.org/record/3466146/export/hx), [CSL](https://zenodo.org/record/3466146/export/csl), [DataCite](https://zenodo.org/record/3466146/export/dcite3), [Dublin core](https://zenodo.org/record/3466146/export/xd), [Mendeley](https://www.mendeley.com/import/?url=https://zenodo.org/record/3466146), [more...](https://zenodo.org/record/3466146/#.XTpLtlBS8W8)

If you are using the associated preprint, please cite:

* Guillerme, T., Puttick MN., Marcy AE., & Weisbecker V. (**2019**). Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces? *bioRxiv* 801571; doi: https://doi.org/10.1101/801571 

    ###### [BibTeX](https://www.biorxiv.org/highwire/citation/946606/bibtext), [RIS](https://www.biorxiv.org/highwire/citation/946606/ris), [End Note (xml)](https://www.biorxiv.org/highwire/citation/946606/endnote-8-xml), [Zotero](https://www.biorxiv.org/highwire/citation/946606/zotero), [Mendeley](https://www.biorxiv.org/highwire/citation/946606/mendeley), [more...](https://www.biorxiv.org/content/10.1101/801571v1)
