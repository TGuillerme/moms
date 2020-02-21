---
title: "MEE-19-10-743 Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces?"
author: "Thomas Guillerme, Mark N. Puttick, Ariel E. Marcy, Vera Weisbecker"
date: "2020-02-12"
output:
  html_document:
    fig_width: 8
    fig_height: 8
---

Dear Editor,

> Mark, Ariel and Vera: this is still a drafty version! Ignore the colours business and the blocks with three @ (@@@) are still not written properly, so if you want to comment them, please don't worry about the tone and the grammar. (the places with a single @ are just the line tags that I'll have to update before resubmission)

@@@please find below our detailed response to the reviewers comments. For clarity, the reviewer's comments are in <font style="color:blue">blue</font> and our response in black with the modified text in the manuscript in *italic*.
We've provided a version of the manuscript where the changes are highlighted in <font style="color:blue">blue</font>.


We believe that some of the parts that the manuscript that the reviewers found unclear were due to the differences of usage of the term "disparity" (which seems to have a disparate meanings throughout the literature depending on the author).
This is why we have deliberately tried to not refer to this term in the manuscript (appart for several specific instances) in order to distance ourselves from the "disparity" meaning*s*, and to try to bridge the gap between evolution and ecology where "disparity" is actually rarely used.


@@@TODO: also update (compile) the supplementaries
@@@Add reference to Bazzi et al Curr. Biol.
@@@Cite Mammalo new paper https://www.biorxiv.org/content/10.1101/2020.01.25.919373v1.full.pdf


# Associate Editor Comments to Author:

* <p style="color:blue">[...]The main problem pointed out by the second reviewer is that the manuscript suffers from a lack of accuracy and clarity in the use of mathematical terminology. I share this concern. I fact I found the manuscript difficult to follow at any level of detail. After reading I was left with little added understanding of what the different measures are supposed to measure or how they perform in these goals. For example, I did not understand the results in the seemingly central Table 5, and like the reviewers I found many mathematical statements to be odd or nonsensical. [...] The topic is certainly appropriate, but a revised manuscript must be substantially improved in mathematical clarity and rigor. The motivation and goals of the measures must be clearly laid out and evaluated. [...]</p>

# Reviewer: 1

<p style="color:blue">In terms of potential modifications to the paper I would actually urge the authors to remove some material rather than add more. Specifically the paper entertains the notion (e.g., L281) that there might be a "magic bullet" metric that captures all three types of measure and that any metric could be applied to capture any of the three types of distribution. This seems a priori to be foolish. For example, sum of ranges could not reasonably be expected to capture density as it does not consider values between the edges (ranges) on any given axis. Similarly, if all points in a distribution were shifted exactly ten units in a specific direction then sum of ranges would remain identical. Thus this is clearly only ever going to be reasonably applied as a volume metric. One could also say something similar for nearest neighbor measures, which logically capture density, but would make no sense as a volume or position metric.</p>

> On line 281, we did actually insist that there is "that there is no perfect one-size-fit all", we have now changed it to "there **can be** no perfect one-size-fit all metric" to highlight that we, like the reviewer agree that finding a "magic bullet" is foolish.

<p style="color:blue">This is especially true of the graphing (i.e., Figure 2E) where the "ideal" scenario will vary depending on the type of measure intended. Currently this makes Table 5 harder to understand than is necessary. Indeed, I would wager that the authors came up with their novel position metric purely because they realised none of the other metrics are logically intended to do this job. I thus suggest a priori defining each metric as a volume, density or position metric and showing only the tests for each in the main text (alongside the ideal plot for a metric that captures that feature well). In other words, sum of ranges would only appear in the volume tests and nearest neighbour only in the density tests. The full results can be moved to the SI, but I think Figure 3 already captures the fact that these metrics measure very different things for the most part (very few show strong positive correlation), as we would logically expect.</p>

> We clarified the Figure 2 to show the "ideal" scenario for any type of metric with two panels showing the best and worst scenarios for any metrics. We did not however separated the Table 5 for each different aspect (size, position, density) since we wanted this table to illustrate how different metrics can capture different aspects at different degrees. For example, the Average distance from centroid (row 1 Table 5) captures changes in size fairly well (column 1) but can also be influenced by changes in density (column 2). Thus changes in the Average distance from centroid are likely to correspond to changes in the size of the trait space but could also be due to changes in density of the trait space (though not likely due to a change in position of the trait space). We added the following to the Table 5 caption to make it clearer:

*This figure illustrates how different metrics can be influenced by different aspects of changes in the trait space. For example, the Average distance from centroid (row 1) captures mainly changes in size (column 1), but also captures changes in density (column 2) but does not capture changes in position (column 3).* lines@ 298-301.


<p style="color:blue">Aside from this, the only potentially fatal flaw in the paper is that it seems to assume features about the spaces that may not be true. For example, that they are Euclidean (distances in the space reflect true distances between points) and isotropic (it is as easy to traverse the space in any direction). E.g., if the latter is not true then distance based measures (nearest neighbor and minimum spanning trees) are confounded. Similarly, if spaces are anisotropic then position based measures may be confounded. </p>

> We've added a disclaimer specifying that the metrics presented here work for Euclidean isotropic spaces:

*Note that these metrics are specific to trait spaces that are Euclidean (distances in the space reflect true distances between points) and isotropic (it is as easy to traverse the space in any direction).* lines@ 195-197.

<p style="color:blue">Additionally, the authors use the term "occupancy" many times, but I do not think they ever really consider occupancy in the truest sense, as this would require knowing how much of the space is truly /occupiable/. For example, where are the true limits of a space (i.e., the "edges")? And are all points inside those limits realistically occupiable? Note that these are simply hard problems, and the authors do already consider other aspects of empirical spaces such as total number of dimensions and distributions of variance across axes. However, I think they could go further in making it clear to readers that these issues must be considered. A nice example of how this could be done was shown by Hillis et al. (2005; Systematic Biology, 54, 471–482). Their Figure 10 shows how ordinations can drastically distort distances. Otherwise, I simply think the authors need to state these issues plainly to help guide readers in their metric choice.</p>

>We thank the reviewer for point out this really interesting aspect and we have added the following disclaimer around the term "occupancy":

*Note that we refer to occupancy here as the general term for where observations are in a trait space.
This definition excludes whether spaces are are truly "occupiable" (e.g. are there limits to the space, are some regions of the space inapplicable, etc.) which might be of importance in particulare spaces (e.g. in tree spaces; @Hillis2005).* lines@ 70-74


<p style="color:blue">(I would also note that the authors currently post several questions that their metrics do not really answer, i.e., L47-48 "are groups overlapping in the trait-space? Are some regions of the trait-space not occupied? How do specific factors influence the occupancy of the trait-space?" These are good questions, but are not what the volume, density and position metrics actually capture. I would suggest rewording, or more explicitly state how these questions can be answered.)</p>

> We reworded the generic questions to correspond to what the size, density and position can actually capture:

*are groups occupying the same amount of trait space?
Do some groups have contain more species than others in the same amount of trait space?
Are some specific factors correlated to different patterns of occupancy of the trait space (e.g. are some factors correlated with more occupancy)?* lines@ 45-47.

<p style="color:blue">Some other thoughts the authors might want to consider (e.g., as additional discussion topics):</p>

> We agree with the reviewers' three points below but due to the restricted amount of words allowed for this manuscript we could unfortunately only add short caveats. Note that these exact three points are discussed in a review paper (still in prep.) where they are discussed in more details.

1. <p style="color:blue">At no point do the authors really discuss visualisation of spaces. E.g., why are these metrics important? Why can't workers simply make bivariate plots and interpet those? (There are good reasons, of course, but I think it would help to make these explicit to emphasise the need for this paper.)</p>

> We've added a note saying that trait space occupancy cannot be studied beyond 2 or 3 dimensions using bi/tri-variate techniques:

*Because of the multidimensional nature of these trait spaces, it is often not possible to study them use bi- or tri-variate techniques.* lines@ 47-49

2. <p style="color:blue">Volume measures generally assume convexity (hypercubes, hyperellipsoids, ranges), but what if the point distributions are concave or have voids (inoccupiable "holes")? This seems like another consideration of what a good metric is that is currently not discussed. Concavities can also be artefactual features of some ordinations (e.g., the "horseshoeing" seen with some spaces).</p>

> We've added the following caveat for the size metrics:

*Size measures do not take into account the distribution of the observations within a group and can be sensible to innocupied "holes" in it (Blonder et al. 2018).* lines@ 104-105

3. <p style="color:blue">What about sampling considerations? I would imagine both density and range would be correlated with sample size, albeit negatively and positively respectively. I don't think the authors are obliged to solve this issue, but it is another consideration of what makes a "good" metric that could be discussed.</p>

> We've added the following caveats for the density metrics:

*Note that density based metrics can be sensitive to sampling*. lines@ 115-116

## Other minor corrections:

* <p style="color:blue">L125 - "Paleogene" (spelling). NB: This is a name and hence there is not a formal UK- (palaeo-) versus US-spelling (paleo-) option here.</p>

> We fixed this typo.

* <p style="color:blue">L127 - "expand" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L152 - I applaud this approach over bombarding readers with information, but a brief note on why would be helpful (and what helped the ones in the main text make "the cut"; are these the most commonly used metrics?).</p>

> We chose these metrics because of their contrasted results and because the where the ones commonly used to represent size and position in the trait-space (expect for the average displacement). We've added a justification in the paper:

*Note that the paper contains the results for only eight metrics which were selected as representative of common metrics covering the size, density and position trait space aspects. However, the results for the additional 17 metrics is available in the supplementary material 4.* lines@ 162-164

* <p style="color:blue">L155-156 - Both the table and contents require more information to understand them. Currently I have to go into the main text to get even a basic idea of what size means, for example.</p>

> We expanded the table caption to:

*Table 2: different simulated space distribution.* Name *of the simulated space in this paper;* dimensions *of the matrix (row\*columns);* distribution(s) *of the data on each dimensions (for the 'Random' space, each dimension is randomly chosen to be Normal, Uniform or Lognormal);* dimension variance *:distribution of the variance between dimensions (when equal, the dimensions have the same variance, otherwise, the variance per dimensions is decreasing either lognormally (multiplicative) or normally (additive);* correlation *between dimensions.* lines 167-172

* <p style="color:blue">L165 - Missing \cite{} in TeX for healy?</p>

> We fixed this typo.

* <p style="color:blue">L167 - Isn't a correlation (i.e., co-linearity) between traits exactly what many ordination techniques use to reduce the dimensionality of the space? I.e., should they be correlated?</p>

> Ordination techniques (e.g. PCA) do indeed produce spaces with uncorrelated dimensions. We added this extra simulation parameters for the cases when trait-spaces where not ordinated and could thus contain some degree of co-linearity. We've specified this in the main text:

*Finally, we added a correlation parameter to take into account the potential correlation between different traits to illustrate the effect of co-linearity between traits (especially in non-ordinated trait spaces).* lines@ 183-185

* <p style="color:blue">L173 - I suspect many of the attributions for these metrics are inaccurate, referring instead to later synthetic works. E.g., sum of variances or ranges certainly goes back to Foote 1992. I think the original authors should get their due here!</p>

> We've updated the attributions of the Sum of ranges and sum of variances to Foote 1992 as well as the Minimum Spanning tree distance to Sedgewick 1990.

* <p style="color:blue">L176 - "their" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L214 - Grammar.</p>

> We fixed this typo.

* <p style="color:blue">L215 - Surely you mean degree of overlap? It seems like you can say absolutely if they do overlap (p=1). Clarification would help.</p>

> We've changed "probability of overlap" to "amount of overlap" throughout the text.

* <p style="color:blue">L216 - This could do with being a bit more informative for what each panel means. Specifically, panel E is probably the most important as it will need to be understood for the results to come to make sense. I.e., what does an ideal plot look like? A worst case scenario?</p>

> We've modified figure 2 by showing a "worst" and "best" scenario (see above).

* <p style="color:blue">L227 - "a decrease in density can often lead to an increase in volume" Surely this depends on the metric. E.g., how could range increase if data are removed? It seems like this is also a consideration for whether a metric is doing a good job. </p>

> We apologise for this typo and meant that a **increase** in density (in a normal space) can lead to a **decrease** of size (not the opposite): when increasing density of point in a normal space, the points further away from the space (which occur at a lower denisty) could be removed which would thus decrease the size. More simply: tightening a space will reduce it's density (again, if the space is normally distributed). We've correct this error. 

* <p style="color:blue">L229 - Again, I think the correct citation for this is an older work.</p>

>We change the reference to Bellman 1957.

* <p style="color:blue">L234 - This seems like a failure of the metric not a hyperdimensionality issue. I.e., shouldn't a good metric capture an expansion regardless of the number of axes involved? Perhaps I am missing something here, but if I specifically think of sum of ranges in a fixed dimension space then increasing the range on any single axis will increase the overall sum.</p>

> The reviewer is totally correct here for the metric "sum of ranges", we here meant that the metric "hypervolume" (i.e. product of ranges) would decrease. We've clarified this in the manuscript:

*if a group expands in multiple dimensions (i.e. increase in size), the actual hypervolume <($\prod_{i}^{d} range_{Di}$) can decrease.* lines@ 262-263

* <p style="color:blue">L245 - "were" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L262 - This Table/Figure is not as easy to interpret as the authors seem to think. I think it would help to show the ideal result for each case as a comparator.</p>

> We improved the table readability by editing Figure 2 as detailed in comment about L216 above.

* <p style="color:blue">L274 - "a change a clear change" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L276 - It should be much easier to identify what the groups are here.</p>

> We've added the groups names for each dataset in the table caption.

* <p style="color:blue">L282 - "except" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L318 - "values" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L342 - "### Caveats" A Markdown subheader without a line break?</p>

> We fixed this typo.

* <p style="color:blue">L346 - "accurately" Missing word? "reflect"?</p>

> We fixed this typo.

* <p style="color:blue">L358 - Missing "a"?</p>

> We fixed this typo.

* <p style="color:blue">L362 - "at" (grammar).</p>

> We fixed this typo.

* <p style="color:blue">L366 - I would actually argue the opposite here. "Disparity" is simply poorly defined, but density and volume have clear meanings. However, specific tests make sense too. In other words, a good use would be something like "We used sum of ranges to capture volume of space occupied".</p>

> We've changed the sentence to reflect the reviewer's opinion that we wholeheartedly agree with:

*Third, we suggest to not name measures as the biological aspect they are describing which can be vague (e.g. "disparity" or "functional dispersion") but rather what they are measuring and why (e.g. "we used sum of ranges to capture the trait space size").* lines@ 405-497 

* <p style="color:blue">SI4, P3 - Plot seems to get cropped by bottom of page.</p>

>We've fixed this display bug.


























# Reviewer: 2

1. <p style="color:blue">Many statistical and mathematical terms are used imprecisely or incorrectly. For instance: </p>

 * <p style="color:blue"> The term "metric" has a particular meaning in statistics and geometry, but the term is used more vaguely in the sense of "measure" here. </p>

 >We changed mentions of metric to measurements to make it clearer we do not mean metric in a geometric sense.

 * <p style="color:blue"> "Volume" is a clear geometric concept, but it is used here for all kinds of formulas. The product of lengths (ranges, standard deviations, inter-quartile distances) in orthogonal directions can indeed be interpreted as a volume, but Procrustes variance (l 61) cannot (this is a sum, not a product of variances). Also the example in Figure 1A is wrong: a sum of ranges is not a volume. Also in Table 3, the first two formulas, "average distance from centroid" (which should read "square root of summed squared distances from centroid") and "sum of variances", are not volumes (and, by the way, they are the same except for the root and the division by n; hence their correlation in the example).</p>

 >We have chosen to change our broad category "volume" to "size" to avoid confusion with the mathematical definition of volume.
 We have renamed the "average distance from centroid" to "average Euclidean distance from centroid".
 Also, the similarity between the average Euclidean distance from centroid is indeed expected but we thought we leave both in the main text of the manuscript since they are commonly used.

 * <p style="color:blue"> Likewise, the term "density" is used in a way that I fail to understand. In statistics, "probability density" is a standard concept that also applies to multivariate data spaces. Sometimes, "density" is used here to describe a variance, e.g. in Fig. 1B, but it is not clear why. Also I don't understand the description in l 103-110.</p>

 >Throughout the manuscript we use density in a more colloquial (or physical) way as suggest by the reviewer where density is an indication of the quantity of observations per units of size (volume, area, etc.): some trait-spaces can have the same size but contain different number of observations.
 In a sense, this can be linked to probability density since a high value (density) in a probability density function of a distribution corresponds to the parts of the distribution with a high number of observations.
 We have tried to make this clearer in our definition of the category of measurement:

 *Density measures an indication of the quantity of observations in the trait space. They can be interpreted as the distribution of the observations within a group in the trait space. Groups with higher density have more observations within it (i.e. more observations per approximation of size) that will tend to be more similar to each other. For example, if group A is greater than group B and both have the same density (observations are equally distant within each group), similar mechanisms could be driving both groups’ trait space occupancy. However, this might suggest that A is older and had more time to achieve more extreme trait combinations under essentially the same process (Endler et al. 2005). Note that density based measures can be sensitive to sampling. Density is less commonly measured compared to size, but it is still used in both ecology (e.g. the minimum spanning tree length; Oksanen et al. 2007) and evolution (e.g. the average pairwise distance; Harmon et al. 2008).* lines@ 109-118

 * <p style="color:blue"> The concept of a "position" of a group in trait space is not clear throughout the manuscript. E.g., in Fig 1C, the median distance from the centroid is considered a "position", but it is just another measure of disparity/variance. And why should "position metrics be harder to interpret in multidimensional spaces" (l 115)? </p>

 > We've changed the position metric in Fig. 1C from "average distance from centroid" to "distances to centre". In this specific case the centre (0,0) of the space and the centroid (-0.23, -0.46) actually differ (though this is not always the case, especially when looking at all the data in a PCA for example). Although we agree that the "distances from centroid" gives an approximation of size, the "distances from centre" gives an approximation of position (i.e. for observations that are further away from the centre). This can be generalised to any measurements like "distances from a fixed point" which will give an idea of the position of a group in trait space. This also links to our comment on line 115 about the difficulty of interpreting position measures in trait space: if two groups have the same position in trait space (say using the average displacement measurement), it just indicates that are equally away from a fixed point (say the centre) but not whether they are in the same position in the trait space (e.g. in a 2D space, groups can be equally distant from the centre left, right up or down - these possible positions will of course increases with each additional dimensions). We've specified this in the main text:

 *For example in a 2D space, two groups can be equally distant from a fixed point but in different parts of the space left, right up or down (with the position possibilities increasing with the number of dimensions).* lines@ 123-125

 * <p style="color:blue"> A data matrix is not yet a "space" (l 80). In addition to a set of elements, a mathematical space requires some relations between the elements, such as a metric or nearness relationship (for assessable discussions of mathematical spaces in biology see, e.g., Stadler et al. 2002 J Theor Biol, Mitteroecker & Huttegger 2009 Biol Theory).</p>

 Here we followed the general mathematical definition of space as a set with some mathematical structure. The exact definition of a mathematical set and structure is way beyond our mathematical comfort zone (and towards logics and philosophy - we specifically followed the [simplification explained here](https://math.stackexchange.com/questions/177937/difference-between-space-and-mathematical-structure)).
 The definition in Mitteroecker & Huttegger 2009 is specific to "morphospaces" whereas the Stadler et al 2002 one is more close to trait spaces as vaguely defined in our manuscript which we have now updated to:

 *In this paper, we define trait spaces as any matrix where rows are observations and columns are traits, where both observations and traits are structurally related (e.g. there is a phylogenetic relation between observations - and traits, etc.).* lines@ 83-85

 * <p style="color:blue"> What is a "random" distribution, as opposed to a normal or uniform distribution (e.g., l 160)?</p>

 >By random we meant a random selection between normal, lognormal and uniform. We've fixed it in the main text:

 *We used a range of distributions (uniform, normal or a random combination of uniform, normal and lognormal) to test the effect of observation distributions on the measurements.* lines@ 175-177

2. <p style="color:blue">I find the statement/conclusion "no one metric describes all of trait-space" somewhat trivial. Also, it is pretty clear that a measure of variance/disparity does not inform about the mean or central tendency; one does not need a simulation for this insight. If one cares about a single measure that captures group differences in both mean and variance (e.g., to assess group overlap), one should use appropriate statistics, such as Mahalanobis distance, Fisher information metric, Bhattacharrya coefficient (which the authors cite in a different context), or one of the many others derived from them.</p>

> We do wholeheartedly agree with this reviewer that the conclusion (and the topic) of this paper is trivial.
This was actually also highlighted by reviewer 1 ("Much of what the authors discuss here could be considered common sense").
However, as reviewer 1 also highlights: "sadly much of the existing literature has failed to take much of this on board".
We of course share the same opinion and this observation was at the origin of this whole paper: providing a basis for "helping workers consider what may be best to use for their own data" (reviewer 1).
Furthermore, we are not only proposing a "trivial" conclusion but we also put some effort into providing a novel tool allowing researchers to make their own decision on which metric to use in their specific case (rather than "blindly" following our suggestions) and propose some specific terms (space occupancy, trait spaces, size, density and position measurements, etc...) that aim to bridge the gap between ecology and evolution that both use multidimensional methods to study similar essential biological questions but with different approaches and a different jargon.
Through this paper we thus propose some basis for both fields to learn from each other and hopefully allowing future fruitful collaborations.


3. <p style="color:blue">The brief discussion of the "curse of dimensionality" is interesting and could be extended, as this is relevant for many modern datasets. However, the statement about the "probability of two points A and B overlapping in n dimensions" is inappropriate, as this probability is zero for continuous variables, regardless of dimension.</p>

> We've removed the bit about the probability of overlap (we should have mentioned "groups" rather than "points") and replaced it by a more general point on the "curse":

*This can have two main unforeseen mathematical consequence: 1) the probability of overlap between two groups decreases as a product of the number of dimensions; and  2) the amount of samples needed to "fill" the spaces increases exponentially [see this interactive illustration by Toph Tucker](https://observablehq.com/@tophtucker/theres-plenty-of-room-in-the-corners).
Furthermore computational time can be increased exponentially rather than linearly with the number of dimensions.* lines@ 257-261

4. <p style="color:blue">I miss a discussion of how the measurement scales of the variables can influence the described statistics. I seems that all the variables are assumed to have an interval scale, for some statistics also a ratio scale. Is this realistic? Also nothing is mentioned about the topology of the multivariate data spaces: Do the spaces need to be metric or even Euclidean? Do the units and scales of the variables all need to be the same?</p>

> See our response to reviewer 1 comment on the trait space properties (lines @@@). Furthermore, we specified that we scale the measurements as to make them comparable since they are relative to each trait space:

*Since the occupancy measures are dependent on their trait space, we scaled and centred them between -1 and 1 to make them comparable.* lines@ 246-247

> We did not mention however whether the units in the trait space need to be Euclidean, scaled, homoscedastic, etc. since this can vary between studies and measurements. In our case most our measures are assuming only a Euclidean isotropic space but not necessarily scaled. The scaling issue of the traits (variables) has more importance in representing the data (e.g. plotting a projection of the trait space) but less in measuring occupancy. This is illustrated in the exaggerated example below:


```r
## A small trait space with a huge scale and distribution difference per axis
space <- cbind(rnorm(10, sd = 1e3), rlnorm(10, sdlog = 1e-3))
## The ellipsoid volume can be calculated using both axis:
dispRity::ellipse.volume(space)
## Operations discussed in this paper on the space are valid
## However, representing without scaling the axis is missleading
par(mfrow = c(1,2))
plot(space, main = "scaled axis")
## Since the real space looks like this
plot(space, xlim = c(-1.5e3, 1.5e3), ylim = c(-1.5e3, 1.5e3),
     main = "unscaled axis")
```

5. <p style="color:blue">The different statistics were evaluated and compared across datasets with different dimensionalities and distributions. While this seems feasible, the computation of p-values by ANOVA is not. This is not a question of type I error as you are not testing against sampling variation. In other words, you treat each dataset as a case drawn independently from the same distribution of datasets. Clearly, this is not what you are doing.</p>

> @@@We somewhat disagree with this reviewer's comment suggesting that the space occupancy measurements are not drawn independently from the same distribution (*sensu* the statistical population) and that we can therefore not estimate the p-value correctly from a simple ANOVA. In fact, because of the centering and scaling of all space occupancy metrics, the metrics are effectively distributed near normally (figure 3) and thus form a coherent population. For example, any sum of variance measured in any simulated space from any parameter forms the "sum of variance population" - the ANOVA then tests whether there is a difference within groups (i.e. the sum of variance measured in a specific simulated space from specific parameters) and between groups (the sum of variance measured in any simulated space). However, we understand the reviewers' concern that these data could be considered as independent but we do not know of any way to report the within/between variation and interpreting the results without using the F-distribution. Maybe the reviewer could help us with this one? Meanwhile, we reported the F-statistic AND the p-value in the table for more clarity.



<!-- 

We've corrected the variance in the ANOVA 

Compare the distribution of disparity scores with a change in dimensions.
The null is that there is no difference between the disparity score and the dimensions.

- We can use a permutation test to test whether the disparities per number of dimensions are different than the overall disparities?
- We can use a series of wilcox tests of one series of disparities vs. all disparities from that parameter n times resulting in 5 p-values?/5



We measured the effect of space distribution and dimensionality using an ANOVA ($occupancy \sim distribution$ and $occupancy \sim dimensions$) by using all spaces with 50 dimensions and the uniform and normal spaces with equal variance and no correlation with 3, 15, 50, 100 and 150 dimensions (Table 2) for testing respectively the effect of distribution and dimensions.
The results of the ANOVAs (*p*-values) are reported in Table 5 (see supplementary material 3 for the full ANOVA result tables). -->


```r
## The data
data <- matrix(c(6, 8, 13, 8, 12, 9, 4, 9, 11, 5, 11, 8, 3, 6, 7, 4, 8, 12 ), ncol = 3, nrow = 6, byrow = TRUE)

## Step 1, calculate the treatments means
means <- apply(data, 2, mean)

## Step 2, calculate the overall mean
overall_mean <- mean(data)

## Step 3, between groups sum of squared differences
between_group_df <- ncol(data) - 1
between_group_ss <- (sum(nrow(data) * (means - overall_mean)^2))/between_group_df

## Step 4, within groups sum of squared differences
within_group_df <- ncol(data) * (nrow(data) - 1)
within_group_ss <- sum(t((t(data)-means)^2))/within_group_df

## Step 5, calculate the F-ratio
F_ratio <- between_group_ss/within_group_ss

# F-ratio thresholf at 0.05 for both degrees of freedom F(2, 15) = 3.68
# Since F_ratio > F(2, 15), the hypothesis can be rejected (all groups are the same)

# The fuck is wrong with that...
```

### Minor comments

* <p style="color:blue">The term "trait space" does not need to be hyphenated.</p>

> We removed the hyphen throughout the manuscript.

* <p style="color:blue">line 11: "a subset of this trait-space" Either a "subset of this set" or a "subspace of this space" </p>

> We changed it to:

*a subspace of this space* line@ 9

* <p style="color:blue">l 39: typo "used"</p>

> We fixed this typo.

* <p style="color:blue"> 175: "eigenvalue" is a single word</p>

> We fixed this typo.

* <p style="color:blue">Table 3:  the formula for "average nearest neighbourhood distance" does not seem correct to me; last line, right column: what is a "centre of trait space"? </p>

> We fixed the average nearest neighbour distance and specified the coordinates of the trait space as the point with coordinates (0, 0, ...).

* <p style="color:blue">Table 5: A p-value cannot be 0.</p>

> We changed our p-value rounding to display p-values rounded to 0 as <1e-3.
















<!-- Methods in Ecology and Evolution




        

Decision Letter (MEE-19-10-743)

From:
    

coordinator@methodsinecologyandevolution.org

To:
    

guillert@tcd.ie

CC:
    

Subject:
    

Methods in Ecology and Evolution - Decision on Manuscript ID MEE-19-10-743

Body:
    

16-Dec-2019

MEE-19-10-743 Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces?

Dear Dr Thomas Guillerme,

I have now received the reviewers' reports and a recommendation from the Associate Editor who handled the review process. Copies of their reports are included below.  Based on their evaluations, I regret to inform you that we are unable to publish your paper in Methods in Ecology and Evolution in its current form.

However, we would be willing to consider a new manuscript which takes into consideration the feedback you have received. I share the AE's and reviewers' comments that such the topic of disparity is potentially of interest to many readers of MEE. But there are many inaccuracies and errors, especially with the technical parts of the work, that will require a total revision before we can even begin to consider this for MEE.

Please read the resubmission instructions at the end of this email before submitting your updated manuscript. The deadline for your resubmission is 15-Mar-2020.

I look forward to your resubmission.

Sincerely,

Dr Lee Hsiang Liow
Senior Editor, Methods in Ecology and Evolution

Reply to:
Mr Chris Grieves
Methods in Ecology and Evolution Editorial Office
coordinator@methodsinecologyandevolution.org


Associate Editor Comments to Author:
Associate Editor
Comments to the Author:
The two reviewers of this manuscript are split with one recommending minor revision and the other rejection with possibility of resubmission. Both reviewers provide many and detailed comments. The main problem pointed out by the second reviewer is that the manuscript suffers from a lack of accuracy and clarity in the use of mathematical terminology. I share this concern. I fact I found the manuscript difficult to follow at any level of detail. After reading I was left with little added understanding of what the different measures are supposed to measure or how they perform in these goals. For example, I did not understand the results in the seemingly central Table 5, and like the reviewers I found many mathematical statements to be odd or nonsensical.  This manuscript is thus not publishable in its present form. To make this publishable in MEE a complete rewrite is required. The topic is certainly appropriate, but a revised manuscript must be substantially improved in mathematical clarity and rigor. The motivation and goals of the measures must be clearly laid out and evaluated. I encourage resubmission only as far as the authors are prepared to do a complete revision and aim for a substantially more rigorous treatment.

Reviewer(s)' Comments to Author:
Reviewer: 1

Comments to the Corresponding Author
This paper concerns the measurement of "disparity" in high-dimensional (usually ordination) spaces. Certainly this is an important topic as biological data is increasingly high-dimensional in nature and tools (i.e., summary statistics) to help capture distributions in such spaces are in high demand. Much of what the authors discuss here could be considered common sense, but sadly much of the existing literature has failed to take much of this on board and hence the term "disparity" has remained ambiguously defined. The authors set out three clearly different ways points can be distributed in a space (volume, density and position), and use a clever way to test for how different metrics capture each one. A large number of metrics (including those already commonly implemented) are tested and overall the paper makes a real contribution to helping workers consider what may be best to use for their own data. I would not say this is a definitve work as I think there are still many open questions, but this is an important step in the right direction and I broadly support publication.

In terms of potential modifications to the paper I would actually urge the authors to remove some material rather than add more. Specifically the paper entertains the notion (e.g., L281) that there might be a "magic bullet" metric that captures all three types of measure and that any metric could be applied to capture any of the three types of distribution. This seems a priori to be foolish. For example, sum of ranges could not reasonably be expected to capture density as it does not consider values between the edges (ranges) on any given axis. Similarly, if all points in a distribution were shifted exactly ten units in a specific direction then sum of ranges would remain identical. Thus this is clearly only ever going to be reasonably applied as a volume metric. One could also say something similar for nearest neighbor measures, which logically capture density, but would make no sense as a volume or position metric. I thus think lumping all metrics into a single "pot" of analyses is both a waste of time and more likely to lead to confusion on the part of the reader. This is especially true of the graphing (i.e., Figure 2E) where the "ideal" scenario will vary depending on the type of measure intended. Currently this makes Table 5 harder to understand than is necessary. Indeed, I would wager that the authors came up with their novel position metric purely because they realised none of the other metrics are logically intended to do this job. I thus suggest a priori defining each metric as a volume, density or position metric and showing only the tests for each in the main text (alongside the ideal plot for a metric that captures that feature well). In other words, sum of ranges would only appear in the volume tests and nearest neighbour only in the density tests. The full results can be moved to the SI, but I think Figure 3 already captures the fact that these metrics measure very different things for the most part (very few show strong positive correlation), as we would logically expect.

Aside from this, the only potentially fatal flaw in the paper is that it seems to assume features about the spaces that may not be true. For example, that they are Euclidean (distances in the space reflect true distances between points) and isotropic (it is as easy to traverse the space in any direction). E.g., if the latter is not true then distance based measures (nearest neighbor and minimum spanning trees) are confounded. Similarly, if spaces are anisotropic then position based measures may be confounded. Additionally, the authors use the term "occupancy" many times, but I do not think they ever really consider occupancy in the truest sense, as this would require knowing how much of the space is truly /occupiable/. For example, where are the true limits of a space (i.e., the "edges")? And are all points inside those limits realistically occupiable? Note that these are simply hard problems, and the authors do already consider other aspects of empirical spaces such as total number of dimensions and distributions of variance across axes. However, I think they could go further in making it clear to readers that these issues must be considered. A nice example of how this could be done was shown by Hillis et al. (2005; Systematic Biology, 54, 471–482). Their Figure 10 shows how ordinations can drastically distort distances. Otherwise, I simply think the authors need to state these issues plainly to help guide readers in their metric choice.

(I would also note that the authors currently post several questions that their metrics do not really answer, i.e., L47-48 "are groups overlapping in the trait-space? Are some regions of the trait-space not occupied? How do specific factors influence the occupancy of the trait-space?" These are good questions, but are not what the volume, density and position metrics actually capture. I would suggest rewording, or more explicitly state how these questions can be answered.)

Some other thoughts the authors might want to consider (e.g., as additional discussion topics):

1. At no point do the authors really discuss visualisation of spaces. E.g., why are these metrics important? Why can't workers simply make bivariate plots and interpet those? (There are good reasons, of course, but I think it would help to make these explicit to emphasise the need for this paper.)
2. Volume measures generally assume convexity (hypercubes, hyperellipsoids, ranges), but what if the point distributions are concave or have voids (inoccupiable "holes")? This seems like another consideration of what a good metric is that is currently not discussed. Concavities can also be artefactual features of some ordinations (e.g., the "horseshoeing" seen with some spaces).
3. What about sampling considerations? I would imagine both density and range would be correlated with sample size, albeit negatively and positively respectively. I don't think the authors are obliged to solve this issue, but it is another consideration of what makes a "good" metric that could be discussed.

Other minor corrections:

L125 - "Paleogene" (spelling). NB: This is a name and hence there is not a formal UK- (palaeo-) versus US-spelling (paleo-) option here.
L127 - "expand" (grammar).
L152 - I applaud this approach over bombarding readers with information, but a brief note on why would be helpful (and what helped the ones in the main text make "the cut"; are these the most commonly used metrics?).
L155-156 - Both the table and contents require more information to understand them. Currently I have to go into the main text to get even a basic idea of what size means, for example.
L165 - Missing \cite{} in TeX for healy?
L167 - Isn't a correlation (i.e., co-linearity) between traits exactly what many ordination techniques use to reduce the dimensionality of the space? I.e., should they be correlated?
L173 - I suspect many of the attributions for these metrics are inaccurate, referring instead to later synthetic works. E.g., sum of variances or ranges certainly goes back to Foote 1992. I think the original authors should get their due here!
L176 - "their" (grammar).
L214 - Grammar.
L215 - Surely you mean degree of overlap? It seems like you can say absolutely if they do overlap (p=1). Clarification would help.
L216 - This could do with being a bit more informative for what each panel means. Specifically, panel E is probably the most important as it will need to be understood for the results to come to make sense. I.e., what does an ideal plot look like? A worst case scenario?
L227 - "a decrease in density can often lead to an increase in volume" Surely this depends on the metric. E.g., how could range increase if data are removed? It seems like this is also a consideration for whether a metric is doing a good job.
L229 - Again, I think the correct citation for this is an older work.
L234 - This seems like a failure of the metric not a hyperdimensionality issue. I.e., shouldn't a good metric capture an expansion regardless of the number of axes involved? Perhaps I am missing something here, but if I specifically think of sum of ranges in a fixed dimension space then increasing the range on any single axis will increase the overall sum.
L245 - "were" (grammar).
L262 - This Table/Figure is not as easy to interpret as the authors seem to think. I think it would help to show the ideal result for each case as a comparator.
L274 - "a change a clear change" (grammar).
L276 - It should be much easier to identify what the groups are here.
L282 - "except" (grammar).
L318 - "values" (grammar).
L332 - This makes perfect sense to me as direction in an ordination space is typically relative or would require some additional context (e.g., towards flatter/higher skulls).
L342 - "### Caveats" A Markdown subheader without a line break?
L346 - "accurately" Missing word? "reflect"?
L358 - Missing "a"?
L362 - "at" (grammar).
L366 - I would actually argue the opposite here. "Disparity" is simply poorly defined, but density and volume have clear meanings. However, specific tests make sense too. In other words, a good use would be something like "We used sum of ranges to capture volume of space occupied".
SI4, P3 - Plot seems to get cropped by bottom of page.


Reviewer: 2

Comments to the Corresponding Author
A number of different statistics to describe phenotype occupation are used in evolutionary biology and ecology, and almost none of them have ever been formally and biologically justified. Hence, the attempt by Guillerme and colleagues to describe, classify, and compare these statistics is important. However, I see several problems in the current manuscript.

1) Many statistical and mathematical terms are used imprecisely or incorrectly. For instance:
- The term "metric" has a particular meaning in statistics and geometry, but the term is used more vaguely in the sense of "measure" here.
- "Volume" is a clear geometric concept, but it is used here for all kinds of formulas. The product of lengths (ranges, standard deviations, inter-quartile distances) in orthogonal directions can indeed be interpreted as a volume, but Procrustes variance (l 61) cannot (this is a sum, not a product of variances). Also the example in Figure 1A is wrong: a sum of ranges is not a volume. Also in Table 3, the first two formulas, "average distance from centroid" (which should read "square root of summed squared distances from centroid") and "sum of variances", are not volumes (and, by the way, they are the same except for the root and the division by n; hence their correlation in the example).
- Likewise, the term "density" is used in a way that I fail to understand. In statistics, "probability density" is a standard concept that also applies to multivariate data spaces. Sometimes, "density" is used here to describe a variance, e.g. in Fig. 1B, but it is not clear why. Also I don't understand the description in l 103-110.
- The concept of a "position" of a group in trait space is not clear throughout the manuscript. E.g., in Fig 1C, the median distance from the centroid is considered a "position", but it is just another measure of disparity/variance. And why should "position metrics be harder to interpret in multidimensional spaces" (l 115)?
- A data matrix is not yet a "space" (l 80). In addition to a set of elements, a mathematical space requires some relations between the elements, such as a metric or nearness relationship (for assessable discussions of mathematical spaces in biology see, e.g., Stadler et al. 2002 J Theor Biol, Mitteroecker & Huttegger 2009 Biol Theory).
- What is a "random" distribution, as opposed to a normal or uniform distribution (e.g., l 160)?

2) I find the statement/conclusion "no one metric describes all of trait-space" somewhat trivial. Also, it is pretty clear that a measure of variance/disparity does not inform about the mean or central tendency; one does not need a simulation for this insight. If one cares about a single measure that captures group differences in both mean and variance (e.g., to assess group overlap), one should use appropriate statistics, such as Mahalanobis distance, Fisher information metric, Bhattacharrya coefficient (which the authors cite in a different context), or one of the many others derived from them.

3) The brief discussion of the "curse of dimensionality" is interesting and could be extended, as this is relevant for many modern datasets. However, the statement about the "probability of two points A and B overlapping in n dimensions" is inappropriate, as this probability is zero for continuous variables, regardless of dimension.

4) I miss a discussion of how the measurement scales of the variables can influence the described statistics. I seems that all the variables are assumed to have an interval scale, for some statistics also a ratio scale. Is this realistic? Also nothing is mentioned about the topology of the multivariate data spaces: Do the spaces need to be metric or even Euclidean? Do the units and scales of the variables all need to be the same?

5) The different statistics were evaluated and compared across datasets with different dimensionalities and distributions. While this seems feasible, the computation of p-values by ANOVA is not. This is not a question of type I error as you are not testing against sampling variation. In other words, you treat each dataset as a case drawn independently from the same distribution of datasets. Clearly, this is not what you are doing.

Some minor issues:

The term "trait space" does not need to be hyphenated.

line 11: "a subset of this trait-space"
Either a "subset of this set" or a "subspace of this space"

l 39: typo "used"

- 175: "eigenvalue" is a single word

Table 3:
- the formula for "average nearest neighbourhood distance" does not seem correct to me.
- last line, right column: what is a "centre of trait space"?

Table 5: A p-value cannot be 0.

+++++
RESUBMISSION INSTRUCTIONS
Please note that resubmitting your manuscript does not guarantee eventual acceptance, and that your resubmission may be subject to re-review before a decision is rendered. Please also ensure that your altered manuscript still conforms to our word limit of 6000-7000 for research articles, or 3000 for applications.

Once you have made the suggested changes, go to https://mc.manuscriptcentral.com/mee-besjournals and login to your Author Centre. Click on "Manuscripts with Decisions," and then click on "Create a Resubmission" located next to the manuscript number. Then, follow the steps for resubmitting your manuscript.

Because we are trying to facilitate timely publication of manuscripts submitted to Methods in Ecology and Evolution, your new manuscript should be uploaded within 12 weeks. The deadline for your resubmission is 15-Mar-2020. If it is not possible for you to submit your manuscript by that date, please get in touch with the editorial office, otherwise we will consider your paper as a completely new submission.

Date Sent:
    

16-Dec-2019
 

 
© Clarivate Analytics |  © ScholarOne, Inc., 2020. All Rights Reserved.
 -->