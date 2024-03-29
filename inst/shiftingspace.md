---
title: "Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces?"
author: "Thomas Guillerme, Mark N. Puttick, Ariel E. Marcy, Vera Weisbecker"
bibliography: references.bib
date: "2019-09-30"
output:
  html_document:
    fig_width: 8
    fig_height: 8
---


<!-- Keywords: disparity, dissimilarity, ecology, evolution, multidimensionality, statistics -->


<!-- When toggling to html, toggle snippet `compilation_html` below to eval = TRUE -->

<!-- To build the line numbered version (for submission) use the following yaml header snippet
output:
  pdf_document:
    fig_width: 8
    fig_height: 8
    keep_tex: true
  self_contained: false


output:
  html_document:
    fig_width: 8
    fig_height: 8
---
and edit the save .tex file to add the following

\usepackage{lineno} %in the package header

\linespread{2} %in the document header

\modulolinenumbers[1] % just after the \begin{document} tag
\linenumbers
 -->









# Abstract


<!-- 350 words and 4 points -->

<!-- Point 1: set the context for and purpose of the work;

Point 2: indicate the approach and methods;

Point 3: outline the main results;

Point 4: identify the conclusions and the wider implications.
 -->



Multidimensional analysis of traits are now a common toolkit in ecology and evolution.
Such analyses are based on multidimensional datasets in which each dimension represents a trait that summarise all observed and potential trait combinations in a multidimensional trait-space (e.g. a morphospace or an ecospace).
Observations of interest will typically only occupy a subset of this space, so researchers apply one or more metrics to quantify the way in which organisms "inhabit" that trait-space.
Researchers use these metrics to investigate how space occupancy changes through time, in relation to other groups of organisms, and in response to global environmental changes, such as global warming events or mass extinctions.

In macroevolutionary and ecological studies these metrics are often referred to as disparity or dissimilarity metrics, respectively, and can be generalised as "space occupancy metrics", a term that is applicable to both disciplines.
The mathematical and biological meaning of most space occupancy metrics is vague.
The majority of widely-used space-occupancy metrics in ecology and evolution lack formal description; this problem is acutely true in studies of macroevolution disparity. 
This has consequences when interpreting changes in space occupancy and linking them to biological phenomena.
For example, does a reduction in volume in the trait-space correspond to episodes of extinction or changes in traits distribution?
Does an overlap in trait-space correspond to an overlap in ecological niches?

Here we propose a broad classification of space occupancy metrics into three categories that capture changes in volume, density, or position of the trait-space.
We analyse the behaviour of eight space occupancy metrics including two novel ones to study changes in density and position on a series of simulated and empirical datasets.
We find no one metric describes all of trait space but that some metrics are better at capturing certain aspects compared to other approaches.
As no one metric is sufficient to describe the changes in trait space it is necessary for researchers to compare many metrics. This can be difficult as different softwares to do so will require different input data types and may require specialist skills.
Here we introduced [`moms`](@@@), a user-friendly tool based on a graphical interface that allows users to both visualise and measure changes space occupancy for any metric in simulated or imported trait-spaces.
Users are also provided with tools to transform their data in space (e.g. contraction, displacement, etc.).
This tool is designed to help researchers choose the right space occupancy metrics, given the properties of their trait-space and their biological question.


# Introduction

Groups of species and environments share specific, easily recognisable, correlated characteristics of many kinds: guilds or biomes with shared phenotypic, physiological, phylogenetic or behavioural traits.
Organisms or environments should therefore be studied as a set of traits rather than some specific traits in isolation [e.g. @donohue2013; @hopkins2017].
Increased computational power and statistical methods, as well as their implementation in `R` packages has facilitated a recent explosion of multiple traits studies approaches [@oksanen2007vegan; @adams2013geomorph; @momocs; @blonder2018; @disprity].
Ordination techniques (or dimensionality reduction) are a popular way to analyse sets of multiple traits and create a multidimensional [see @legendre2012 for a summary].
These trait-space approaches have been successfully used across many sub-disciplines in biology to either explore properties of the data or test hypotheses.
For example, in palaeobiology, workers use trait-spaces to study how groups of species' characteristics change through time [e.g. @wright2017]; in ecology, researchers study evidence of processes such as competition by the comparison of two populations to show whether they overlap in traits [e.g. @jones2015].
However, different fields use a different set of terms and approaches for such trait-space approaches (Table 1).
In macroevolution, for example, such trait-spaces are commonly referred to as "morphospaces" with the whole procedure often being often called "disparity analysis" [e.g. @adams2013geomorph; @marcy2016; @wright2017], while in ecology, such trait-spaces can be called "functional spaces" and or "dissimilarity analysis" [e.g. @diaz2016; @mammola2019].
Nonetheless, they are the same mathematical objects: matrices with columns representing an original or transformed trait value and rows representing observations, such as taxon, field site, etc. [@disprity].

In mathematics | In ecology | In macroevolution | In this paper
---------------|------------|-------------------|---------------
Matrix ($n \times d$) | Function-space, Eco-space, etc. | Morphospace, traitspace, etc. | trait-space
Rows (*n*) | Taxa, field sites, environments, etc. | Taxa, specimen, populations, etc. | observations
Columns (*d*) | Traits, Ordination scores, distances, etc. | Traits, Ordination scores, distances, etc. | dimensions
Matrix subset ($m \times d$; $m \leq n$) | Treatments, phylogenetic group (clade), etc. | Clades, geological stratum, etc. | group
Statistic | Dissimilarity index or metric, hypervolume, functional diversity | Disparity metric or index | space occupancy metric
Multidimensional analysis | Dissimilarity analysis, trait analysis, etc. | Disparity analysis, disparity-through-time, etc. | multidimensional analysis
Table 1@@@: terms and equivalence between mathematics, ecology and macroevolution.

Ecologists and evolutionary biologists are also using trait-spaces for similar reasons.
Across many sub-disciplines they will look at how their observations occupy the trait-space, often with respect to the same fundamental questions: are two groups overlapping in the trait-space?
Are some regions of the trait-space not occupied?
Or how do specific factors influence the occupancy of the trait-space?
Studying the occupancy of trait-spaces is achieved in various ways such as using disparity metrics in macroevolution [@wills2001; @hopkins2017; @disprity] or comparing hypervolumes in ecology [@donohue2013; @diaz2016; @blonder2018; @mammola2019].
Although these space occupancy metrics are common in ecology and evolution, surprisingly little work has been published on their behaviour [but see @ciampaglio2001; @villéger2008; @mammola2019].

Different space occupancy metrics capture different aspects of the trait-space [ciampaglio2001; @villéger2008; @mammola2019].
It may be widely-known by many in the field that distinct occupancy metrics elucidate different signals of the underlying space, but to our knowledge this is infrequently mentioned in peer-reviewed papers.
First, space occupancy metrics are often named as the biological aspect they are describing (e.g. "disparity" or "functional dispersion") rather than what they are measuring (e.g. the sum of dimensions variance, etc.) which obscures the differences or similarities in space occupancy metrics between studies in both ecology and evolution.
Second, in many studies in ecology and evolution, authors have focused on measuring the volume of the trait-space with different metrics [e.g. ellipsoid volume @donohue2013; hypervolume @diaz2016; product of variance @wright2017; Procrustes variance @marcy2016].
However, volume only represents a single aspects of multidimensional occupancy that disregards other aspects, such as the density [@geiger2008], the position [@wills2001; @ciampaglio2001], and the dissimilarity between groups [@oksanen2007vegan; @close2015].
For example, any analysis of volume occupancy through trait space may remain constant, and this will lead to supporting a certain biological conclusion.
Yet, an alternative aspect of multidimensional occupancy may indicate the position is changing while the volume is constant; this would likely lead to a different biological conclusion. 
Measuring the volume would miss position information that might be of interest to biologists (e.g. a group of species maintains the same diversity of traits while also shifting towards a new ecological niche).
Using metrics that only measure one aspect of the multidimensional trait space may restrain the potential of multidimensional analysis [@villéger2008].

Here we propose a broad classification of space occupancy metrics as used across ecology [@villéger2008] and evolution [@wills2001] and analyse their power to detect changes in multidimensional space occupancy in empirical and simulated data.
We provide a review of each broad type of space occupancy metrics and propose a unified terminology aiming to foster communication between ecology and evolution.
Unsurprisingly, we found no one metric describes all changes through a trait-space and the results from each metric are dependent on the characteristics of the space and the hypotheses.
Furtheremore, because there can potentially be as many metrics as there are trait-spaces and studies, it would be a daunting task to propose clear generalities to space occupancy metrics behavior.
Therefore, we propose [`moms`](@@@) user-friendly tool allowing researchers to design, experiment and visual their own space occupancy metric tailored for their specific project and helping them understanding the "null" behavior of their metrics of interest.
<!-- AM: So good! I want this in the abstract!  -->


## Space occupancy metrics

In this paper, we define trait-spaces as any matrix where rows are observations and columns are traits.
In practice, these traits can widely vary in number and types: they could be coded as discrete [e.g. presence of absence of a bone; @beck2014; @wright2017], continuous measurements [e.g. leaf area; @diaz2016] or more sophisticated measures of 2- or 3-dimensions [e.g. landmark position; @marcy2016].
Traits can also be measured by using relative observations such as community composition [e.g. @jones2015] or even some distance between observations [e.g. @close2015].
However, regardless of the methodology used to build a trait-space, three broad occupancy metrics can be measured to investigate how observations are distributed in the trait-space: the volume which will approximate the amount of space occupied, the density which will approximate the distribution in space and the position which will approximate the location in space [Fig. 1@@@; @villéger2008].
Of course any combination of these three aspects is always possible.

![Figure 1: different type of information captured by space occupancy metrics. A - Volume (e.g. sum of ranges); B - Density (e.g. average squared pairwise distances); C - Position (e.g. median distance from centroid).](figure/fig_metrics_types-1.png)

#### 1. Volume

Volume metrics measure the spread of a group in the trait-space.
They can be interpreted as the amount of the trait-space that is occupied by observations.
Typically, larger values for such metrics indicate the presence of more extreme trait combinations in the sample.
For example, if group A has a bigger volume than group B, the observations in group A achieve more extreme trait combinations than in group B.
This type of metric is widely used in both ecology [the hypervolume; @blonder2018; for example to compare group's niche overlap @mammola2019] and in evolution [the sum or product of ranges or variances @wills2001; @ciampaglio2001 or the Procrustes variance @adams2013geomorph].

Although volume metrics are a highly suitable indicator for comparing a group's trait-space occupancy, it is limited to comparing the range of extreme trait-combinations between groups.
Typically, volume metrics do not take into account the spread of the observations within a group.
In other words, they can make it difficult to determine whether all the observations are on the edge of the volume or whether the volume is simply driven by small number of extreme observations.

This aspect of space occupancy is used routinely in ecology and macroevolution [@wills2001; @blonder2018] because disparity or dissimilarity analysis often look at the "spread" of observations in multidimensional space.
For example @wright2017 uses the volume of trait-space to analyse how crinoid ecology changes through time relative to all the observed ecologies in the the crinoid trait-space.
A larger "spread" at a specific point in time [e.g. in the Early Carboniferous - 360 to 330 million years ago (Mya); @wright2017] indicates a larger diversity of ecological feeding strategies.
In general the volume of a group in the trait-space indicates how much trait combinations that group achieves in the trait-space.

#### 2. Density

Density metrics measure the distribution of a group in the trait-space.
They can be interpreted as an indication of the distribution of the observations *within* a group in the trait-space; this can be considered as a measure of the "packedness", so spaces with higher density have more observations "packed" into that region.  
For example, if group A is more dense than group B, observations in group A tend to be more similar between each other relatively than within group B.
Density is less commonly measured compared to trait-space volume, but it is still used in both ecology [e.g. the minimum spanning tree length; @oksanen2007vegan] and evolution [e.g. the average pairwise distance; @geiger2008].
For example, if group A has a greater volume than group B but both have the same density, similar mechanisms could be driving both groups' trait-space occupancy.
However, this might suggest that A is older and had more time to achieve more extreme trait combinations under essentially the same process.

#### 3. Position

Position metrics measure where a group lies in trait-space.
They can be interpreted as an indication of where a group lies in the trait-space either relative to the space itself or relative to another group.
For example, if group A has a different position than group B, observations in group A will have a different trait-combination than group B.

Position metrics may be harder to interpret in multidimensional spaces (i.e. beyond left/right and above/below) and are hence less often used.
However, when thinking about unidimensional data (one trait), this metric is obvious: for example, two groups A or B could have the same variance (i.e. "volume" or spread) with the same number of observations (i.e. the same density) but could have a totally different mean and thus be in different positions.
Although these metrics are rarer in macroevolution, they have been used in ecology to compare the position of two groups relatively to each other [@mammola2019].

## No metric to rule them all: benefits of considering multiple metrics

The use of multiple metrics to assess trait-space occupancy has the benefit of providing a more detailed characterization of occupancy changes.
For example, if the question of interest is, say, to look at how space occupancy changes in response to mass extinction, using a single metric to describe one aspect of occupancy (usually the volume) can miss part of the picture.
In this mass extinction example, a change in volume (or the absence thereof) could be decoupled from a change in the observations’ position or density in trait-space.
The Cretaceous-Palaeogene extinction (66 Mya) has been linked to an increase in volume of the mammalian trait-space [the mark of an adaptive radiation; @halliday2015] but more specific questions can be answered by looking at other aspects of trait-occupancy: does the radiation expands on previously existing morphologies [elaboration, increase in density; @endler2005] or does it explore entire new regions of the trait-space [innovation, change in position; @endler2005]?
Similarly, in ecology, if two groups occupy the same volume in the trait-space, it can be interesting to look at differences in density within these two hypothetical groups: different selection pressure can lead to different density within equal volume groups.
Such examples can be found throughout the literature in both evolution and ecology on different aspects of space occupancy [e.g. @close2015; @diaz2016].


![Figure 2: Illustration how three different volume (vol. - Product of ranges), density (den. - Average nearest neighbour distance) and position (pos. - Average displacement) metrics capture different occupancy aspects in this simplified trait-space. This illustrates how using only one specific space occupancy metric (e.g. the volume) will fail to capture changes in B (no change in volume), or partial changes in C (also change in density and position).](figure/fig_metric_captures-1.png)

<!-- To allow a more confident assessment of whether trait-space changes occur, and also to refine the characterization of trait-space change, we suggest using multiple metrics to describe space occupancy the same way that multiple statistics are required to describe distributions. -->

In this paper, we provide the first interdisciplinary review of 25 space occupancy metrics that uses the broad classification of metrics into volume, density and position to capture pattern changes in trait-space.
We assess the behaviour of metrics using simulations and a selection of interdisciplinary empirical datasets; these cover a wide range of potential data types and evolutionary/ecological questions.
We also introduce a tool for Measuring Occupancy in Multidimensional Space ([`moms`](@@@)), which is a user-friendly, open-source graphical interface to allow the tailored testing of metric behaviour for any use case.
[`moms`](@@@) will allow workers to comprehensively assess the properties of their trait-space and the metrics associated with their specific biological question.







<!--  -->
<!--  -->
<!--  -->
<!-- METHODS SECTION -->
<!--  -->
<!--  -->
<!--  -->








# Methods



Briefly, we tested how eight different space occupancy metrics relate to each other, are affected by modifications of traits space and affect group comparisons in empirical data.
To do so, we performed the following steps (explained in more detail below):

1.  We simulated 13 different spaces with different sets of parameters (dimensions, distributions, etc);
2.  We transformed these spaces by removing 50% of the observations following four different scenarios corresponding to different empirical scenarios: randomly, by limit (e.g. expansion or reduction of niches), by density (e.g. different degrees of competition within a guild) and by position (e.g. ecological niche shift).
3.  We measured occupancy on the resulting transformed spaces using eight different space occupancy metrics;
4.  We applied the same space occupancy metrics to six empirical datasets (covering a range of disciplines and a range of dataset properties).


| Test | Methods section | Results
|------|-----------------|------------------
| How do the metrics compare to each other? | Metrics comparisons       | Figure 4@@@
| How are metrics affected by space shifts? | Space shifts              | Table 6@@@
| How is that reflected in the real world?  | Empirical examples        | Table 7@@@
Table 2@@@: summary of the tests in this paper.

Note that the results of these four steps are described in the supplementary material 4@@@ for an additional 17 metrics not mentioned in the main text.

## Generating spaces

We generated trait spaces using the following combinations of size, distributions, variance and correlation:

| space name | size  | distribution(s)                | dimensions variance | correlation |
|------------|-------|--------------------------------|---------------------|-------------|
| Uniform3   |200*3  | Uniform (min = -0.5, max = 0.5)| Equal            | None        |
| Uniform15  |200*15 | Uniform                        | Equal            | None        |
| Uniform50  |200*50 | Uniform                        | Equal            | None        |
| Uniform150 |200*150| Uniform                        | Equal            | None        |
| Uniform50c |200*50 | Uniform                        | Equal            | Random (between 0.1 and 0.9) |
| Normal3    |200*3  | Normal  (mean = 0, sd = 1)     | Equal            | None        |
| Normal15   |200*15 | Normal                         | Equal            | None        |
| Normal50   |200*50 | Normal                         | Equal            | None        |
| Normal150  |200*150| Normal                         | Equal            | None        |
| Normal50c  |200*50 | Normal                         | Equal            | Random (between 0.1 and 0.9) |
| Random     |200*50 | Normal, Uniform, Lognormal (meanlog = 0, sdlog = 1)| Equal            | None        |
| PCA-like   |200*50 | Normal                         | Multiplicative           | None        |
| PCO-like   |200*50 | Normal                         | Additive              | None        |
Table 3@@@: different simulated space distributions.

The different in space sizes (200 $\times$ 3, 200 $\times$ 15, 200 $\times$ 50 or 200 $\times$ 150) reflects the range of trait-space dimensions in literature: "low-dimension" spaces ($<15$) are common in ecology [@mammola2019] whereas high dimension spaces ($>100$) are common in macroevolution [@hopkins2017].
We used a distinct range of distributions (uniform, normal or random) to test the effect of observation distributions on the metrics.
We used different levels of variance for each dimensions in the spaces by making the variance on each dimension either equal (i.e. $\sigma_{D1} \simeq \sigma_{D2} \simeq \sigma_{Di}$) or decreasing (i.e. $\sigma_{D1} < \sigma_{D2} < \sigma_{Di}$) with the decreasing factor being either multiplicative (using the cumulative product of the inverse of the number of dimensions: $\prod_i^d(1/d)$) or additive (using the cumulative sum of the inverse of the number of dimensions: $\sum_i^d(1/d)$).
Both multiplicative and cumulative reductions of variance are used to illustrate the properties of ordinations where the variance decreases per dimensions [in a lognormal way in principal components analysis - PCA; e.g. @marcy2016; healy2019; and in a normal way in Multidimensional Scaling - MDS, PCO or PCoA; e.g. @close2015; @wright2017].
Finally, we added a correlation parameter to take into account the potential correlation between different traits.
We repeated the simulation of each trait-space 20 times (resulting in 260 trait-spaces).

## Spatial occupancy metrics

We then measured eight different metrics on the resulting transformed spaces, including a new metric we produced, the average displacement, which we expect to be mainly influenced by changes in trait-space position.


| Name | Definition | Captures | Source | Notes  |
|------|------------|----------|--------|--------|
| Average distance from centroid | $\frac{\sqrt{\sum_{i}^{n}{({k}_{n}-Centroid_{k})^2}}}{d}$ | Volume | @laliberté2010 | equivalent to the functional dispersion (FDis) in @laliberté2010 (but not weighted by observation's abundance) |
| Sum of variances | $\sum_{i}^{d}{\sigma^{2}{k_i}}$ | Volume | @wills2001 | common metric used in palaeobiology [@ciampaglio2001] |
| Sum of ranges | $\sum_{i}^{d}{\|\text{max}(d_{i})-\text{min}(d_{i})\|}$ | Volume | @wills2001 | more sensitive to outliers than the sum of variances |
| Ellipsoid volume | $\frac{\pi^{d/2}}{\Gamma(\frac{d}{2}+1)}\displaystyle\prod_{i}^{d} (\lambda_{i}^{0.5})$ | Volume | @donohue2013 | less sensitive to outliers than the convex hull hypervolume [@diaz2016; @blonder2018] |
| Minimum spanning tree average distance | $\frac{\sum(\text{branch length})}{n}$ | Density | @oksanen2007vegan | similar to the unscaled functional evenness [@villéger2008] |
| Minimum spanning tree distances evenness | $\frac{\sum\text{min}\left(\frac{\text{branch length}}{\sum\text{branch length}}\right)-\frac{1}{n-1}}{1-\frac{1}{n-1}}$ | Density | @villéger2008 | the functional evenness without weighted abundance [FEve; @villéger2008] |
| Average nearest neighbour distance | $min\left(\sqrt{\sum_{i}^{n}{({q}_{i}-p_{i})^2}}\right)\times \frac{1}{n}$ | Density | @foote1990 | the density of pairs of observations in the trait-space (c.f. the Minimum spanning tree average distance above) |
| Average displacement | $\frac{\sqrt{\sum_{i}^{n}{({k}_{n})^2}}}{\sqrt{\sum_{i}^{n}{({k}_{n}-Centroid_{k})^2}}}$ | Position | This paper | the ratio between the observations' position from their centroid and the centre of the trait-space. A value of 1 indicates that the observations' centroid is the centre of the trait-space |
Table 4@@@: List of metrics with *n* being the number of observations, *d* the total number of dimensions, *k* any specific row in the matrix, *Centroid* being their mean and $\sigma^{2}$ their variance. $\Gamma$ is the Gamma distribution and $\lambda_{i}$ the eigen value of each dimension and ${q}_{i}$ and $p_{i}$ are any pairs of coordinates.


We selected these eight space occupancy metrics to illustrate how they capture different aspects of space occupancy (but note that this is not an expression of our preference).
The supplementary material 4@@@ contains the same analysis as described below, performed on a total of 25 metrics.
Furthermore, [`moms`](@@@) allows exploring the effect of many more metrics as well as the customisation of metrics by combining them or using user-designed functions.


<!--  -->
<!--  -->
<!--  -->
<!-- COMPARING METRICS -->
<!--  -->
<!--  -->
<!--  -->


## Metric comparisons

We compared the space occupancy correlations across all simulations between each pair of metrics to assess they captured similar signal [@villéger2008; @laliberté2010].
First, we used the metrics on the full 13 trait-spaces described above.
We then scaled the results and measured the pairwise Pearson correlation to test whether metrics were capturing a similar signal (high positive correlation), a different signal (correlation close to 0) or an opposite signal (high negative correlations).
We performed all the tests using the `psych` package [@psych].

<!--  -->
<!--  -->
<!--  -->
<!-- SHIFTING SPACES -->
<!--  -->
<!--  -->
<!--  -->

## Changing space {#changing-spaces}

To measure how the metrics responded to changes within trait-spaces, we reduced the spaces by removing 50% of elements each time using the following algorithms:

* **Randomly:** by randomly removing 50% of elements (Fig. 3@@@-A).
This reflects a "null" biological model of changes in trait-space: the case when observations (e.g. species) are removed regardless of their intrinsic characteristics.
For example, if diversity (i.e. number of species) is reduced by 50% but the trait-space volume remains the same, there is a decoupling between diversity and space occupancy [@ruta2013; @hopkins2013].
Our selected metrics are expected to not be affected by this change.

* **Limit:** by removing all elements with a distance from the centre (mean point) of the space lower or greater than a radius $\rho$ (where \$rho$ is selected such that 50% elements are selected) generating two limit removals: *maximum* and *minimum* (respectively in orange and blue; Fig. 3@@@-B).
This can reflect a strict selection model where observations with trait values below or above a threshold are removed leading to an expansion or a contraction of the trait-space.
Volume metrics are expected to be most affected by this change.

* **Density:** by removing any pairs of point with a distance $D$ from each other where (where $D$ is selected such that 50% elements are selected) generating two density removals: *high* and *low* (respectively in orange and blue; Fig. 3@@@-C).
This can reflect changes within groups in the trait-space due to ecological factors [e.g. competition can generate niche repulsion - lower density; @grant2006].
Density metrics are expected to be most affected by this change.

* **Position:** by removing points similarly as for **Limit** but using the distance from the furthest point from the centre (max point) generating two position removals: *positive* and *negative* (respectively in orange and blue; Fig. 3@@@-D).
This can reflect global changes in trait-space due, for example, to an entire group remaining diverse but occupying a different niche (e.g. in the transition from non-avian dinosaur to avian dinosaurs).
Position metrics are expected to be most affected by this change.

The algorithm to select $\rho$ or $D$ is described in greater detail in in the [Appendix](#Appendix_algorithm_reduce).


<!-- Figure 3: different type of space reduction. Each panel displays two groups (orange and blue) of 50% of the data points each. Each groups are generated using the following algorithm: A - randomly; B - by limit (maximum and minimum limit in respectively orange and blue); C - by density (high and low in respectively orange and blue); and D - by position (positive and negative in respectively orange and blue). Panel E represents a typical display of the reduction results displayed in Table 6@@@: the dots represent the median space occupancy values across all simulations for each scenario of trait space change, the solid and dashed line respectively the 50% and 95% confidence intervals. Results in grey are the random 50% reduction (panel A). Results in blue and orange represent the blue and orange opposite scenarios from panels B, and D. The displayed numerical value are the probability of overlap (Bhattacharrya Coefficient) between the blue and orange distributions and the grey one. -->

![Figure 3: different type of space reduction. Each panel displays two groups (orange and blue) of 50% of the data points each. Each group are generated using the following algorithm: A - randomly; B - by limit (maximum and minimum limit in respectively orange and blue); C - by density (high and low in respectively orange and blue); and D - by position (positive and negative in respectively orange and blue). Panel E represents a typical display of the reduction results displayed in Table 6@@@: the dots represent the median space occupancy values across all simulations for each scenario of trait-space change, the solid and dashed line respectively the 50% and 95% confidence intervals. Results in grey are the random 50% reduction (panel A). Results in blue and orange represent the blue and orange opposite scenarios from panels B, and D. The displayed numerical value is the probability of overlap (Bhattacharrya Coefficient) between the blue and orange distributions and the grey one.](figure/fig_reduce_space-1.png)

To measure the effect of space reduction, distribution and dimensionality on the metric, we first scaled the metric to be relative to the non-reduced space for each dimension distribution or number of dimensions.
To do so, we subtracted the observed occupancy with no space reduction (i.e. base occupancy) to all the occupancy measurements of the reduced spaces and then divided it by the resulting maximum observed occupancy.
This way, our occupancy metrics where scaled between -1 and 1 with a value of 0 indicating no effect of the space reduction and $>0$ and $<0$ respectively indicating an increase or decrease in the occupancy metric value.
We then measured the probability of overlap of the between the non-random removals (limit (max/min), density (high/low) and displacement (positive/negative)) and the random removals using the Bhattacharrya Coefficient [i.e. the probability of overlap between two distributions; @bhattacharyya1943; @guillerme2016].

### Measuring the effect of space and dimensionality on shifting spaces

In some situations, distribution differences and the number of dimensions can have an effect on the metric results.
For example, in a normally distributed space, a decrease in density can often lead to an increase in volume.
This is not necessarily true in log-normal spaces or in uniform spaces for certain metrics (e.g. the convex hull of a lognormal space will be less sensitive to changes in density).
Furthermore, high dimensional spaces (>10) are subject to the "curse of multidimensionality" [@cursedimensionality]: data becomes sparser with increasing number of dimensions, such that the probability of two points A and B overlapping in *n* dimensions is the product of the probability of the two points overlapping on each dimensions($\prod_{i}^{d} P(A = B)_{Di}$).
This probability decreases as a product of the number of dimensions.
Therefore, the "curse" can make the interpretation of high dimensional data counter-intuitive.
For example if a group expands in multiple dimensions (i.e. increase in volume), the actual hypervolume can decrease (Fig. 4@@@ and Tables 6@@@, 7@@@).

We measured the effect of space distribution and dimensionality using an ANOVA ($occupancy \sim distribution$ and $occupancy \sim dimensions$) by using all spaces with 50 dimensions and the uniform and normal spaces with equal variance and no correlation with 3, 15, 50, 100 and 150 dimensions (Table 3@@@) for testing respectively the effect of distribution and dimensions.
The results of the ANOVAs (*p*-values) are reported in Table 6@@@ (see supplementary material for the full ANOVA results).


<!--  -->
<!--  -->
<!--  -->
<!-- EMPIRICAL EXAMPLES -->
<!--  -->
<!--  -->
<!--  -->

## Empirical examples

To address the degree to which the simulations can be applied to real biological problems, we analysed the effect of the different space occupancy metrics on six different empirical studies covering a broad range of fields that employ trait-space analyses (palaeobiology, macroevolution, evo-devo, ecology, etc.).
For each of these six studies we generated trait-spaces from the data published with the papers.
We then divided the trait-spaces into two biologically-relevant groups and tested whether the metrics differentiated the groups in different ways.
Both the grouping and the questions where based on a simplified version of the biological questions addressed in these papers (with no intention to re-analyse the data but to be representative of a sample of the diversity of questions in ecology and evolution).
The procedures to generate the data and the groups varies from one study to the other but is detailed and fully reproducible in the supplementary materials.


study | field | taxonomic Group | traits (data) | trait-space | size | groups (orange/blue in Table 7) | type of question |
------|-------|-----------------|---------------|-------------|------|--------|------------------|
@beck2014 | Palaeontology | Mammalia | discrete morphological phylogenetic data | Ordination of a distance matrix (PCO) | 106*105 | 52 crown vs. 54 stem | Are crown mammals more disparate than stem mammals?|
@wright2017 | Palaeontology | Crinoidea | discrete morphological phylogenetic data | Ordination of a distance matrix (PCO) | 42*41 | 16 before vs. 23 after | Is there a difference in disparity before and after the Ordovician mass extinction?|
@marcy2016 | Evolution | Rodentia | skull 2D landmark coordinates  | Ordination of a Procrustes Superimposition (PCA) | 454*134 | 225 *Megascapheus* vs. 229 *Thomomys* | Are two genera of gopher morphologically distinct? |
@hopkins2016 | Evolution | Trilobita | 3D landmark coordinates | Ordination of a Procrustes Superimposition (PCA) | 46*46 | 36 adults vs. 10 adults | Are juvenile trilobites a subset of adult ones? |
@jones2015 | Ecology | Plantae | Communities species compositions | Ordination of a Jaccard distance matrix (PCO) | 48*47 | 24 aspens vs. 24 grasslands | Is there a difference in species composition between aspens and grasslands? |
@healy2019 | Ecology | Animalia | Life history traits | Ordination (PCA) | 285*6 | 83 ecthotherms vs. 202 endotherms | Do endotherms have more diversified life history strategies than ectotherms? |
Table 5@@@: details of the six empirical trait-spaces.

For each empirical trait-space we bootstrapped each group 500 times [@disprity] and applied the eight space occupancy metric to each pairs of groups.
Note that when space occupancy could not be calculated (e.g. due to the curse of multidimensionality), we collapsed the metric value to 0 (i.e. no space occupancy was captured).
We then compared the means of each groups using the Bhattacharrya Coefficient [i.e. the probability of overlap between two distributions; @bhattacharyya1943; @guillerme2016].
The are displayed in Table 7@@@.

<!--  -->
<!--  -->
<!--  -->
<!-- RESULTS SECTION -->
<!--  -->
<!--  -->
<!--  -->








# Results





## Metric comparisons 


![Figure 4: pairwise correlation between the scaled metrics. Numbers on the upper right corner are the Pearson correlations. The red line are linear regressions (with the confidence intervals in grey).](figure/fig_metric_correlation-1.png)

All the metrics selected were either positively correlated (Pearson correlation of 0.99 for the Average distance from centroid and Sum of variance or 0.97 for the Average nearest neighbour distance and Minimum spanning tree average length; Fig. 4@@@) or somewhat correlated (ranging from 0.66 for the sum of variances and the ellipsoid volume to -0.09 between the average displacement and the average distance from centroid; Fig. 4@@@).
Note that all metrics but the ellipsoid volume were normally (or nearly normally) distributed (Fig. 4@@@).
This is due to the ellipsoid volume being more sensitive to the curse of multidimensionality (many values close to 0).
More comparisons between metrics are available in the supplementary materials.

## Space shifting





Metric      | Volume change  | Density change | Position change | Distribution effect | Dimensions effect |
:-----------|----------------|-----------------|----------------|---------------------|-------------------|
Average distance from centroid | ![](figure/fable_results-1.png) | ![](figure/fable_results-2.png) | ![](figure/fable_results-3.png) | p =  0.449  | p =  0.958 |
Sum of variances | ![](figure/fable_results-4.png) | ![](figure/fable_results-5.png) | ![](figure/fable_results-6.png) | p =  0.274  | p =  0.873 |
Sum of ranges | ![](figure/fable_results-7.png) | ![](figure/fable_results-8.png) | ![](figure/fable_results-9.png) | p =  0 *** | p =  0 ***|
Ellipsoid volume | ![](figure/fable_results-10.png) | ![](figure/fable_results-11.png) | ![](figure/fable_results-12.png) | p =  0 *** | p =  0 ***|
Minimum spanning tree average distance | ![](figure/fable_results-13.png) | ![](figure/fable_results-14.png) | ![](figure/fable_results-15.png) | p =  0.326  | p =  0.435 |
Minimum spanning tree distances evenness | ![](figure/fable_results-16.png) | ![](figure/fable_results-17.png) | ![](figure/fable_results-18.png) | p =  0 *** | p =  0 ***|
Average nearest neighbour distance | ![](figure/fable_results-19.png) | ![](figure/fable_results-20.png) | ![](figure/fable_results-21.png) | p =  0.207  | p =  0.626 |
Average displacements | ![](figure/fable_results-22.png) | ![](figure/fable_results-23.png) | ![](figure/fable_results-24.png) | p =  0 *** | p =  0 ***|
Table 6@@@: Results of the effect of space reduction, space dimension distributions and dimensions number of the different space occupancy metrics. See Fig. 3@@@ for interpretation of the figures. _p_-values for distribution effect and dimensions effect represents respectively the effect of the ANOVAs space occupancy ~ distributions and space occupancy ~ dimensions (Signif. codes: 0 '\*\*\*' 0.001 '\*\*' 0.01 '\*' 0.05 '.' 0.1 '' 1).

As expected (Fig. 2@@@), some different metrics capture different aspects of space occupancy.
In contrast to the clear hypothetical example in Fig. 2@@@, it can be hard to predict the behaviour of each metric when 50% of the observations are removed.
In fact we observe a clear decrease in median metric in less than a third of the space reductions (10/36).

In terms of change in volume (increase or decrease), only the average distance from centroid and the sum of variances seem to capture a clear change in both directions.
Note however that the increase in volume does not correspond to an *actual* increase in volume in the trait-space (i.e. the volume from the blue observations in Fig. 3@@@-B is equivalent to the one in Fig. 3@@@-A).
In terms of change in density (increase or decrease), only the minimum spanning tree average distance and the average nearest neighbour distance seem to capture a clear change in both directions.
In terms of change in position (increase or decrease), only the average displacement metric seems to capture a change a clear change in direction (albeit not in both directions).
This is not surprising however, since the notion of positions becomes more and more complex to appreciate as dimensionality increases (i.e. beyond left/right (1D), up/down (2D) and front/back (3D)).

## Empirical example









Metric     | Beck and Lee 2014  | Wright 2017 | Marcy et al. 2016 | Hopkins and Pearson 2016 | Jones et al. 2015 | Healy et al. 2019  |
:-----------|----------------|-----------------|------------------|----------------|------------------|----------------|
Average distance from centroid | ![](figure/fable_results_empirical-1.png) | ![](figure/fable_results_empirical-9.png) | ![](figure/fable_results_empirical-17.png) | ![](figure/fable_results_empirical-25.png) | ![](figure/fable_results_empirical-33.png) | ![](figure/fable_results_empirical-41.png)|
Sum of variances | ![](figure/fable_results_empirical-2.png) | ![](figure/fable_results_empirical-10.png) | ![](figure/fable_results_empirical-18.png) | ![](figure/fable_results_empirical-26.png) | ![](figure/fable_results_empirical-34.png) | ![](figure/fable_results_empirical-42.png)|
Sum of ranges | ![](figure/fable_results_empirical-3.png) | ![](figure/fable_results_empirical-11.png) | ![](figure/fable_results_empirical-19.png) | ![](figure/fable_results_empirical-27.png) | ![](figure/fable_results_empirical-35.png) | ![](figure/fable_results_empirical-43.png)|
Ellipsoid volume | ![](figure/fable_results_empirical-4.png) | ![](figure/fable_results_empirical-12.png) | ![](figure/fable_results_empirical-20.png) | ![](figure/fable_results_empirical-28.png) | ![](figure/fable_results_empirical-36.png) | ![](figure/fable_results_empirical-44.png)|
Minimum spanning tree average distance | ![](figure/fable_results_empirical-5.png) | ![](figure/fable_results_empirical-13.png) | ![](figure/fable_results_empirical-21.png) | ![](figure/fable_results_empirical-29.png) | ![](figure/fable_results_empirical-37.png) | ![](figure/fable_results_empirical-45.png)|
Minimum spanning tree distances evenness | ![](figure/fable_results_empirical-6.png) | ![](figure/fable_results_empirical-14.png) | ![](figure/fable_results_empirical-22.png) | ![](figure/fable_results_empirical-30.png) | ![](figure/fable_results_empirical-38.png) | ![](figure/fable_results_empirical-46.png)|
Average nearest neighbour distance | ![](figure/fable_results_empirical-7.png) | ![](figure/fable_results_empirical-15.png) | ![](figure/fable_results_empirical-23.png) | ![](figure/fable_results_empirical-31.png) | ![](figure/fable_results_empirical-39.png) | ![](figure/fable_results_empirical-47.png)|
Average displacements | ![](figure/fable_results_empirical-8.png) | ![](figure/fable_results_empirical-16.png) | ![](figure/fable_results_empirical-24.png) | ![](figure/fable_results_empirical-32.png) | ![](figure/fable_results_empirical-40.png) | ![](figure/fable_results_empirical-48.png)|
Table 7@@@: Comparisons of different pairs of groups in different empirical trait-spaces. NAs are used for cases where space occupancy could not be measured due to the curse of multidimensionality. The displayed values are the probability of overlap between both groups (Bhattacharrya Coefficient).

Similarly as for the simulated results, the empirical ones indicate that there is no perfect one-size-fit all metric.
For all eight metrics (expect the ellipsoid volume) we see either one group or the other having a bigger mean than the other and no consistent case where a group has a bigger mean than the other for all the metrics.
For example, in the @beck2014's dataset, there is a clear non-overlap in space occupancy volume using the average distance from centroid or the sum of variances (overlaps of respectively 0.175 and 0.159) but no overlap when measuring the volume using the sum of ranges (overlap of 0.966).
However, for the @hopkins2016's dataset, this pattern is reversed (no clear differences for the average distance from centroid or the sum of variances - overlap of 0.701 and 0.865 respectively) but a clear difference for the sum of ranges (overlap of 0).
Furthermore, for each dataset, the absolute differences between each groups (i.e. mean of the orange group higher or lower than the one of the blue group) is not consistent depending on the metrics.
For example, in @hopkins2016's dataset, the orange group's mean is clearly higher than the blue one when measuring the sum of ranges (overlap of 0) and the inverse is true when measuring the average displacement (overlap of 0).


<!--  -->
<!--  -->
<!--  -->
<!-- DISCUSSION SECTION -->
<!--  -->
<!--  -->
<!--  -->








# Discussion

In this study, we tested twenty-five metrics of trait-space occupancy on simulated and empirical datasets to assess how each metric captures changes in trait-space volume, density and position.
Our results show that the correlation between metrics can vary both within and between metric categories (Fig. 4@@@), highlighting the importance of understanding the metric classification for the interpretation of results.
Furthermore, our simulations show that different metrics capture different types of trait-space change (Table 6@@@), meaning that the use of multiple metrics is important for comprehensive interpretation of trait-space occupancy.
We also show that the choice of metric impacts the interpretation of group differences in empirical datasets (Table 7@@@), again emphasizing that metric choice has a real impact on the interpretation of  specific biological questions


#### Metrics comparisons

Metrics within the same category of trait-space occupancy (volume, density or position) do not have the same level of correlation with each other.
For volume metrics, only the average distance from centroid and the sum of variances are strongly correlated (Pearson correlation of 0.99; Fig. 4@@@) whereas the sum of ranges and the ellipsoid volume are more weekly correlated to them (ranging from 0.07 to 0.40; Fig. 4@@@).
This is due to the sum of ranges and the ellipsoid volume being more affected by the number of dimensions and the type of trait-spaces [Table 6@@@; @cursedimensionality].
This does not preclude using such metrics but raises caution about what they capture in which scenarios [@donohue2013; @Butler2012].
We see the same pattern for the density metrics with the minimum spanning tree average distance and the average nearest neighbour distance being strongly correlated (correlation of 0.97; Table 6@@@), conversely to the minimum spanning tree distances evenness (correlation of respectively 0.18 and 0.20; Table 6@@@) that is also more affected by the number of dimensions and the type trait-space (Table 6@@@).
Some metrics can capture different aspects of trait-space occupancy at the same time: the average distance from centroid (volume proxy) is correlated to the sum of variance (volume proxy) but not to the ellipsoid volume (volume proxy) but still to the average nearest neigbhour distance (density proxy).


Furthermore, the space occupancy values captured by all metrics behave following a normal distribution except for the ellipsoid volume which demonstrates a classic "curse of multidimensionality" with values increasing logarithmically first and then quickly decreasing to 0 [Fig. 4@@@; @cursedimensionality].
Overall, the fact that we have such a range of correlations for normal distributions suggests that each metric can capture different summaries of space occupancy ranging from obvious differences (for metrics not strongly correlated) to subtle ones (for metrics strongly correlated).
Comparisons between more metrics are available in the Supplementary Material 4@@@ and show the same wide range of patterns.

Analysing the properties of metrics in a trait-space prior to using them to answer macroevolutionary or ecological questions can bring valuable insights into what aspect of space occupancy they are capturing.
For example, if some space occupancy metrics captures the same pattern using the sum of variances and the sum of ranges, it is possible that the trait-space have really similar properties (i.e. space with equal normal dimensions).

#### Space shifting

Most metrics capture a normal "null" space shift with no changes in space occupancy on average (i.e. randomly removing 50% of the data - in grey in Table 6@@@).
This is a desirable behaviour for space occupancy metrics since most studies using space occupancy look at potential processes resulting in the observed space occupancy patterns [e.g. competition @brusatte2008, convergence @marcy2016, life history traits @healy2019, etc.].
In this context, random changes in the space occupancy should not be picked up as a pattern to avoid false positive errors.
However, the average nearest neighbour distance and the sum of ranges have a respectively positive and negative "null" median meaning that any decrease of elements will always lead to respectively and increase and a decrease of these metrics even when the process is random (Table 6@@@).
This is not especially a bad property for both metrics (i.e. they can still be used to analysis changes in space occupancy) but it should be kept in mind that even random processes will increase or decrease the metric value which can lead to false positives or negatives. 

Regarding the changes in volume of the trait-spaces, the sum of variances and the average distance from centroid are excellent descriptors, with both maximum and minimum limit changes leading to a clear increase or decrease of the metrics values.
However, as illustrated in the 2D examples in Fig. 3@@@-B, only the minimum limit changes (orange) should actually lead to a decrease of volume of the trait-space; the volume of the maximum limit shifted trait-spaces should have the same volume as the non-modified trait-space but with a hole in its centre ("hollowing out").
Therefore, an increase in "volume" of the trait-space does not necessary always corresponds to an *actual* 3D volume increase but could also reflect a "hollowing" of the trait-space.

Regarding changes in density of the trait-spaces, the average nearest neigbhour distance and the minimum spanning tree average distance consistently detect changes in density with more precision for low density trait-spaces (in blue in Table 6@@@).
However, these metrics seem to be also associated to a "null" increase: a small increase in the metric value could be due to a random change (null) or a small decrease in density (see above).
In general, we can observe some degree of correlation between the changes in density and the changes in volume for most metric picking either signal (i.e. the average nearest neighbour distance picks both changes in density and in volume).
This could be due to the generation of normally distributed spaces where a change in density often leads to a change in volume (i.e. to increase density in a normal distribution, one can simply reduce its tails).
This is not necessary the case with empirical data.

Regarding the changes in position of the trait-space, all but the average displacement metric seems to not be able to distinguish between a random change in trait-space (grey) and a positive or a negative displacement of the trait-space (blue and orange - Table 6@@@).
Furthermore, the average displacement metric does not distinguish between and positive or a negative displacement of the trait-space: in both cases, the metric increases.
This might be due to the inherent complexity of "position" in a trait-space that increases with the number of dimensions.

Regarding the effect of dimensions and trait-space distribution, metrics seems to be either affected by neither of these factors or by both (Table 6@@@).
Again, this should not preclude using metrics that are sensitive to trait-spaces' dimensions and distributions [e.g. the ellispoid volume is a good volume descriptor up until a moderate number of dimensions; @jackson2011; @donohue2013] but should be kept in mind.

#### Empirical examples

Although most differences are fairly consistent within each dataset with one group having a higher space occupancy score than the other for multiple metrics, this difference can be more or less pronounced within each dataset (ranging from no to nearly full overlap - BC $\in(0;0.995)$) and sometimes even reversed.
This indicates that opposite conclusions can be drawn from a dataset depending on which space occupancy metric is considered.
For example, in @wright2017 dataset where the blue group has a greater occupancy metric median value but for the average displacement metric showing a higher median displacement in the red group (Table 7@@@).
These differences in group's space occupancy depending on the metrics are also more pronounced in the empirical datasets where the elements per group is highly different [@hopkins2016; @healy2019].
This also highlights the influence of different groups size on top of the differences in trait-spaces and metrics properties.
Finally, it is worth noting that the ellispoid volume only picks up accurate differences between groups in the @healy2019 dataset which is yet another illustration of the curse of multidimensionality: the @healy2019 dataset has only 6 dimensions compared to all the other ones having more than 41 dimensions [and the @marcy2016 dataset having 136 dimensions for which the ellipsoid volume can't be calculated; Table 7@@@; @cursedimensionality].

### Caveats

While our simulations have been useful to illustrate the behavior of diverse space occupancy metrics, they have several caveats. For example, the trait-spaces are simulated by treating each observation as independent.
This is not often the case in biology since observations might be spatially [@jones2015] or phylogenetically auto-correlated [e.g. @beck2014].
However, we can note that the we get expected results from the empirical data where the observations are more realistically auto-correlated.
Furthermore, the algorithm used to reduce the multi-dimensional spaces might not always accurately reflect how trait-spaces are changed in nature.
They might indeed favour the response of specific metrics, in particular for the changes in density that only modifies the density between pairs of points rather than changing the global density.
This is a specific algorithmic choice was made in order to not confound changes in density along with changes in volume (i.e. a general change in density can be obtain by uniformly increasing/decreasing the volume).
However, the results probably capture the general behaviour of each space occupancy metric since results are consistent between the simulated and emipirical analysis.
Furthermore, the [`moms`](@@@) shiny app allows to test the caveats mentioned above by uploading empirical trait-space and personalised space occupancy metrics.

### Suggestions

We insist that no metric is better than the next one but that researchers should use the most appropriate metrics based on their question, the specific trait-space, and knowledge of the metric’s properties. 
However, the findings of this study might suggest several points:

First, we suggest using multiple metrics to tackle different aspects of the trait-space. 
This is suggestion is in the same logical thinking line that the mean can not be sufficient to describe a distribution (the variance and the type of distribution might be good additional indicators).
Although using multiple metrics is not uncommon in macroevolutionary studies [e.g. @halliday2015] or in ecology [@mammola2019], they often do not cover contrasted aspects of the trait-space (and are often different proxies of the volume of a trait-space).

Second, we suggest selecting a metric (or a series of metrics) that best help answering the biological question a hand.
In fact, if one studies an adaptive radiation in a group of organisms, it is worth thinking what would be the expected null model: would the group's volume increase (radiation in all directions), would it increase in density (niche specialisation) or would it shift in position (radiation into a new set of niches)?

Third, we suggest to not name metrics as the biological aspect they are potentially describing (e.g. disparity or functional dispersion) but rather what they are measuring (e.g. sum of dimensions variance or the average distance from centroids).
We believe this point will allow both a clearer understanding of what is measured (c.f. what is approximated) and an easier understanding between ecology and evolution for which the metrics can be the similar but named differently and used for approximating different types of trait occupancy (Fig. 4@@@).

Multidimensional analysis has been acknowledged to be an essential toolkit modern biology [@adams2019] but can often be counter-intuitive [@cursedimensionality].
It is thus crucial to accurately describe patterns in multidimensional trait-spaces to be able to link them to biological processes of interest.
Mainly, it is important to remember that there are no one-size-fits-all trait-space occupancy metric.
And that what a pattern captured by a specific space occupancy metric is often dependent on the properties of the trait-space and of the particular biological question of interest.
We thus believe that having a clearer understanding of both the properties of the trait-space and the associated space occupancy metrics (e.g. using [`moms`](@@@)) as well as using novel space occupancy metrics to answer specific questions (e.g. the average displacement metric to capture changes in group’s position) will be of great use to study biological processes in a multidimensional world.


# Acknowledgements

We thank Natalie Jones for pointing out to the empirical ecological datasets.
We acknowledge funding from the Australian Research Council DP170103227 and FT180100634 awarded to VW.

# Authors contributions

TG, MNP, AEM and VW designed the project.
TG and AEM collected the empirical dataset.
TG ran the analyses and designed the software.
TG, MNP, AEM and VW wrote the manuscript.

# Data Availability, repeatability and reproducibility

The raw empirical data is available from the original papers @beck2014, @jones2015, @marcy2016, @hopkins2016, @wright2017 and @healy2019.
The subset of the empirical data used in this analysis is available on figshare @@@.
The modified empirical data is available in the package accompanying this manuscript (`data(moms::demo_data)`).
The entirety of this manuscript (including the figures, tables and supplementary material) is repeatable and reproducible by compiling the vignette of the [GitHub `moms R` package](https://github/TGuillerme/moms) (see the `README` file for more information).
The code for the `moms` shiny app open source and available from the [GitHub `moms R` package](https://github/TGuillerme/moms).

# References



# Supplementary material

## Algorithm for selecting the parameters to reduce the space ($radius$, $displacement$, $density$) {#Appendix_algorithm_reduce}

We used a recursive algorithm for selecting the parameter that removes $P$ = 50% elements.

1. Select a random reduction parameters $R$.
2. Remove elements from the space using $R$ resulting in $P'$ removed elements.
If the remaining number of elements is to the required proportion $P$ ; exit the algorithm;
Else go to 3.
3. Get the different between the proportion of removed elements $P'$ and $P$.
If the difference is positive set the increment parameter $R$ to $R = 1.1 \times R$, then go to 2.
Else set $R = 0.9 \times R$, then go to 2.

The algorithm is implemented in the `optimise.parameter` function in [reduce.space_fun.R](@@@).
