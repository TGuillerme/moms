# MEE-19-10-743 Shifting spaces: which disparity or dissimilarity metrics best summarise occupancy in multidimensional spaces?

Dear Dr Thomas Guillerme,

I have now received the reviewers' reports and a recommendation from the Associate Editor who handled the review process. Copies of their reports are included below. Based on their evaluations, I regret to inform you that we are unable to publish your paper in Methods in Ecology and Evolution in its current form.

However, we would be willing to consider a new manuscript which takes into consideration the feedback you have received. I share the AE's and reviewers' comments that such the topic of disparity is potentially of interest to many readers of MEE. But there are many inaccuracies and errors, especially with the technical parts of the work, that will require a total revision before we can even begin to consider this for MEE.

Please read the resubmission instructions at the end of this email before submitting your updated manuscript. The deadline for your resubmission is 15-Mar-2020.

I look forward to your resubmission.

Sincerely,

Dr Lee Hsiang Liow
Senior Editor, Methods in Ecology and Evolution


# Associate Editor Comments to Author:

* The two reviewers of this manuscript are split with one recommending minor revision and the other rejection with possibility of resubmission. Both reviewers provide many and detailed comments. The main problem pointed out by the second reviewer is that the manuscript suffers from a lack of accuracy and clarity in the use of mathematical terminology. I share this concern. I fact I found the manuscript difficult to follow at any level of detail. After reading I was left with little added understanding of what the different measures are supposed to measure or how they perform in these goals. For example, I did not understand the results in the seemingly central Table 5, and like the reviewers I found many mathematical statements to be odd or nonsensical.  This manuscript is thus not publishable in its present form. To make this publishable in MEE a complete rewrite is required. The topic is certainly appropriate, but a revised manuscript must be substantially improved in mathematical clarity and rigor. The motivation and goals of the measures must be clearly laid out and evaluated. I encourage resubmission only as far as the authors are prepared to do a complete revision and aim for a substantially more rigorous treatment.

# Reviewer: 1

This paper concerns the measurement of "disparity" in high-dimensional (usually ordination) spaces. Certainly this is an important topic as biological data is increasingly high-dimensional in nature and tools (i.e., summary statistics) to help capture distributions in such spaces are in high demand. Much of what the authors discuss here could be considered common sense, but sadly much of the existing literature has failed to take much of this on board and hence the term "disparity" has remained ambiguously defined. The authors set out three clearly different ways points can be distributed in a space (volume, density and position), and use a clever way to test for how different metrics capture each one. A large number of metrics (including those already commonly implemented) are tested and overall the paper makes a real contribution to helping workers consider what may be best to use for their own data. I would not say this is a definitve work as I think there are still many open questions, but this is an important step in the right direction and I broadly support publication.

In terms of potential modifications to the paper I would actually urge the authors to remove some material rather than add more. Specifically the paper entertains the notion (e.g., L281) that there might be a "magic bullet" metric that captures all three types of measure and that any metric could be applied to capture any of the three types of distribution. This seems a priori to be foolish. For example, sum of ranges could not reasonably be expected to capture density as it does not consider values between the edges (ranges) on any given axis. Similarly, if all points in a distribution were shifted exactly ten units in a specific direction then sum of ranges would remain identical. Thus this is clearly only ever going to be reasonably applied as a volume metric. One could also say something similar for nearest neighbor measures, which logically capture density, but would make no sense as a volume or position metric. I thus think lumping all metrics into a single "pot" of analyses is both a waste of time and more likely to lead to confusion on the part of the reader. This is especially true of the graphing (i.e., Figure 2E) where the "ideal" scenario will vary depending on the type of measure intended. Currently this makes Table 5 harder to understand than is necessary. Indeed, I would wager that the authors came up with their novel position metric purely because they realised none of the other metrics are logically intended to do this job. I thus suggest a priori defining each metric as a volume, density or position metric and showing only the tests for each in the main text (alongside the ideal plot for a metric that captures that feature well). In other words, sum of ranges would only appear in the volume tests and nearest neighbour only in the density tests. The full results can be moved to the SI, but I think Figure 3 already captures the fact that these metrics measure very different things for the most part (very few show strong positive correlation), as we would logically expect.

Aside from this, the only potentially fatal flaw in the paper is that it seems to assume features about the spaces that may not be true. For example, that they are Euclidean (distances in the space reflect true distances between points) and isotropic (it is as easy to traverse the space in any direction). E.g., if the latter is not true then distance based measures (nearest neighbor and minimum spanning trees) are confounded. Similarly, if spaces are anisotropic then position based measures may be confounded. Additionally, the authors use the term "occupancy" many times, but I do not think they ever really consider occupancy in the truest sense, as this would require knowing how much of the space is truly /occupiable/. For example, where are the true limits of a space (i.e., the "edges")? And are all points inside those limits realistically occupiable? Note that these are simply hard problems, and the authors do already consider other aspects of empirical spaces such as total number of dimensions and distributions of variance across axes. However, I think they could go further in making it clear to readers that these issues must be considered. A nice example of how this could be done was shown by Hillis et al. (2005; Systematic Biology, 54, 471–482). Their Figure 10 shows how ordinations can drastically distort distances. Otherwise, I simply think the authors need to state these issues plainly to help guide readers in their metric choice.

(I would also note that the authors currently post several questions that their metrics do not really answer, i.e., L47-48 "are groups overlapping in the trait-space? Are some regions of the trait-space not occupied? How do specific factors influence the occupancy of the trait-space?" These are good questions, but are not what the volume, density and position metrics actually capture. I would suggest rewording, or more explicitly state how these questions can be answered.)

Some other thoughts the authors might want to consider (e.g., as additional discussion topics):

1. At no point do the authors really discuss visualisation of spaces. E.g., why are these metrics important? Why can't workers simply make bivariate plots and interpet those? (There are good reasons, of course, but I think it would help to make these explicit to emphasise the need for this paper.)
2. Volume measures generally assume convexity (hypercubes, hyperellipsoids, ranges), but what if the point distributions are concave or have voids (inoccupiable "holes")? This seems like another consideration of what a good metric is that is currently not discussed. Concavities can also be artefactual features of some ordinations (e.g., the "horseshoeing" seen with some spaces).
3. What about sampling considerations? I would imagine both density and range would be correlated with sample size, albeit negatively and positively respectively. I don't think the authors are obliged to solve this issue, but it is another consideration of what makes a "good" metric that could be discussed.

Other minor corrections:

* L125 - "Paleogene" (spelling). NB: This is a name and hence there is not a formal UK- (palaeo-) versus US-spelling (paleo-) option here.
* L127 - "expand" (grammar).
* L152 - I applaud this approach over bombarding readers with information, but a brief note on why would be helpful (and what helped the ones in the main text make "the cut"; are these the most commonly used metrics?).
* L155-156 - Both the table and contents require more information to understand them. Currently I have to go into the main text to get even a basic idea of what size means, for example.
* L165 - Missing \cite{} in TeX for healy?
* L167 - Isn't a correlation (i.e., co-linearity) between traits exactly what many ordination techniques use to reduce the dimensionality of the space? I.e., should they be correlated?
* L173 - I suspect many of the attributions for these metrics are inaccurate, referring instead to later synthetic works. E.g., sum of variances or ranges certainly goes back to Foote 1992. I think the original authors should get their due here!
* L176 - "their" (grammar).
* L214 - Grammar.
* L215 - Surely you mean degree of overlap? It seems like you can say absolutely if they do overlap (p=1). Clarification would help.
* L216 - This could do with being a bit more informative for what each panel means. Specifically, panel E is probably the most important as it will need to be understood for the results to come to make sense. I.e., what does an ideal plot look like? A worst case scenario?
* L227 - "a decrease in density can often lead to an increase in volume" Surely this depends on the metric. E.g., how could range increase if data are removed? It seems like this is also a consideration for whether a metric is doing a good job. 
* L229 - Again, I think the correct citation for this is an older work.
* L234 - This seems like a failure of the metric not a hyperdimensionality issue. I.e., shouldn't a good metric capture an expansion regardless of the number of axes involved? Perhaps I am missing something here, but if I specifically think of sum of ranges in a fixed dimension space then increasing the range on any single axis will increase the overall sum.
* L245 - "were" (grammar).
* L262 - This Table/Figure is not as easy to interpret as the authors seem to think. I think it would help to show the ideal result for each case as a comparator.
* L274 - "a change a clear change" (grammar).
* L276 - It should be much easier to identify what the groups are here.
* L282 - "except" (grammar).
* L318 - "values" (grammar).
* L332 - This makes perfect sense to me as direction in an ordination space is typically relative or would require some additional context (e.g., towards flatter/higher skulls).
* L342 - "### Caveats" A Markdown subheader without a line break?
* L346 - "accurately" Missing word? "reflect"?
* L358 - Missing "a"?
* L362 - "at" (grammar).
* L366 - I would actually argue the opposite here. "Disparity" is simply poorly defined, but density and volume have clear meanings. However, specific tests make sense too. In other words, a good use would be something like "We used sum of ranges to capture volume of space occupied".
* SI4, P3 - Plot seems to get cropped by bottom of page.


# Reviewer: 2

A number of different statistics to describe phenotype occupation are used in evolutionary biology and ecology, and almost none of them have ever been formally and biologically justified. Hence, the attempt by Guillerme and colleagues to describe, classify, and compare these statistics is important. However, I see several problems in the current manuscript.

## 1) Many statistical and mathematical terms are used imprecisely or incorrectly. For instance:
- The term "metric" has a particular meaning in statistics and geometry, but the term is used more vaguely in the sense of "measure" here.
- "Volume" is a clear geometric concept, but it is used here for all kinds of formulas. The product of lengths (ranges, standard deviations, inter-quartile distances) in orthogonal directions can indeed be interpreted as a volume, but Procrustes variance (l 61) cannot (this is a sum, not a product of variances). Also the example in Figure 1A is wrong: a sum of ranges is not a volume. Also in Table 3, the first two formulas, "average distance from centroid" (which should read "square root of summed squared distances from centroid") and "sum of variances", are not volumes (and, by the way, they are the same except for the root and the division by n; hence their correlation in the example).
- Likewise, the term "density" is used in a way that I fail to understand. In statistics, "probability density" is a standard concept that also applies to multivariate data spaces. Sometimes, "density" is used here to describe a variance, e.g. in Fig. 1B, but it is not clear why. Also I don't understand the description in l 103-110.
- The concept of a "position" of a group in trait space is not clear throughout the manuscript. E.g., in Fig 1C, the median distance from the centroid is considered a "position", but it is just another measure of disparity/variance. And why should "position metrics be harder to interpret in multidimensional spaces" (l 115)? 
- A data matrix is not yet a "space" (l 80). In addition to a set of elements, a mathematical space requires some relations between the elements, such as a metric or nearness relationship (for assessable discussions of mathematical spaces in biology see, e.g., Stadler et al. 2002 J Theor Biol, Mitteroecker & Huttegger 2009 Biol Theory).
- What is a "random" distribution, as opposed to a normal or uniform distribution (e.g., l 160)?

## 2) I find the statement/conclusion "no one metric describes all of trait-space" somewhat trivial.
Also, it is pretty clear that a measure of variance/disparity does not inform about the mean or central tendency; one does not need a simulation for this insight. If one cares about a single measure that captures group differences in both mean and variance (e.g., to assess group overlap), one should use appropriate statistics, such as Mahalanobis distance, Fisher information metric, Bhattacharrya coefficient (which the authors cite in a different context), or one of the many others derived from them.

## 3) The brief discussion of the "curse of dimensionality" is interesting and could be extended, as this is relevant for many modern datasets.
However, the statement about the "probability of two points A and B overlapping in n dimensions" is inappropriate, as this probability is zero for continuous variables, regardless of dimension.

## 4) I miss a discussion of how the measurement scales of the variables can influence the described statistics.
I seems that all the variables are assumed to have an interval scale, for some statistics also a ratio scale. Is this realistic? Also nothing is mentioned about the topology of the multivariate data spaces: Do the spaces need to be metric or even Euclidean? Do the units and scales of the variables all need to be the same?

## 5) The different statistics were evaluated and compared across datasets with different dimensionalities and distributions.
While this seems feasible, the computation of p-values by ANOVA is not. This is not a question of type I error as you are not testing against sampling variation. In other words, you treat each dataset as a case drawn independently from the same distribution of datasets. Clearly, this is not what you are doing.


* The term "trait space" does not need to be hyphenated.
* line 11: "a subset of this trait-space"
Either a "subset of this set" or a "subspace of this space" 
* l 39: typo "used"

*  175: "eigenvalue" is a single word

* Table 3:  the formula for "average nearest neighbourhood distance" does not seem correct to me; last line, right column: what is a "centre of trait space"? 

* Table 5: A p-value cannot be 0.