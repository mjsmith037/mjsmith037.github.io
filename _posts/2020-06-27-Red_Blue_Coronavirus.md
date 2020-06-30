---
layout: post
title: "Interpreting graphs: the red-blue coronavirus divide"
author: "Matthew J. Michalska-Smith"
date: "2020-06-28"
categories: current_events basic_statistics interpreting_graphs
---

I recently saw this graph floating about the internet, and it has some interesting properties that I thought might warrant a new type of post, where I dive into interpreting a particular figure and see where it takes me.

<!--more-->

<img style="display:block;margin:auto;" src="/Images/blog_figures/red_blue_coronavirus/red_blue_original.jpeg"/>

The graph we are looking at today<sup><label for="figsource" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="figsource" class="margin-toggle"/><span
class="sidenote">Unfortunately the associated article for the figure is <a
href="https://www.washingtonpost.com/politics/2020/06/17/coronavirus-has-come-trump-country/">behind
a paywall</a>, but I've read some of the <a
href="https://twitter.com/busbyj2/status/1273683231108464643">Twitter discussion</a></span> has two
panels, showing similar trends. On the left is a plot of the percentage of new cases of COVID-19
(vertical axis) occurring in either "blue states" (i.e. states that voted for Hillary Clinton in the
last presidential election) or "red states" (i.e. those states that voted for Donald Trump). These
percentages are shown as they varies through time (horizontal axis). On the right, we see a similar
trend for "blue" and "red" counties.

In both cases, we see that the blue line is dropping while, the red line is climbing. For the states panel, we even see a crossing of the lines -- there are more new cases in red states than there are in blue states each day over the past week or so.

The responses I have seen fall into a few main camps<sup><label for="partisan" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="partisan" class="margin-toggle"/><span
class="sidenote">I'll let you intuit how these responses correlate with political preference as
well</span>:

1. This is due to differences between republican and democrat policies/beliefs/behaviors in response to the pandemic
2. This is a [red herring](https://en.wikipedia.org/wiki/Red_herring), and distorts the picture that there have been many more cases in blue states than red states
3. This is a result of demography, including population movement and density

And, of course, it bears stressing that these explanations are not mutually exclusive.

There are some important issues to keep in mind when looking at this plot. First, recall that the
vertical axis is not the number of new cases, but rather the percentage of new cases belonging to
each of these two groups. This means we have no information on whether the absolute number of new
cases is going up, down, or staying steady overall, **or within either group**. Second, the data are
here divided into groups of unequal size: there are more states and counties that voted for Trump
than for Clinton in the last election<label for="countyparty"
class="margin-toggle">&#8853;</label><input type="checkbox" class="margin-toggle"
id="countyparty"/><span class="marginnote"><a
href="/Images/blog_figures/red_blue_coronavirus/county_densities.png"><img
src="/Images/blog_figures/red_blue_coronavirus/county_densities.png" alt="A plot showing the
distribution of county population densities by 2016 presidential candidate preferece"/></a></span>.
Note that nearly 85% of counties voted for Trump (as opposed to Clinton) in 2016, but that the
counties that voted for Clinton were disproportionately populous. Though the log-scaling of the
horizontal axis in the figure to the right can be deceiving here, consider that of the top 100 most
dense counties, 89 of them went blue. The issue of unequal sample size is particularly important in
statistics when it correlates with other variables that might contribute to explaining the pattern
of interest. In this case, we note that political party preference seems to correlate with
population density.

## Forming an expectation

A key step in interrogating new data is to come up with an idea of what you expect the data to look
like<sup><label for="expect" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="expect" class="margin-toggle"/><span class="sidenote">Ideally with some logical
justification</span>. If politics had no bearing on COVID-19, what would we expect to see? If each
person has an equal likelihood of getting sick, regardless of their political orientation, then the
new daily cases would be distributed across the county as people are: more populous places would
have more cases each day, and less populous places would have fewer. Here I have recreated the
counties portion of the original figure using publicly accessible data<sup><label for="focus"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="focus"
class="margin-toggle"/><span class="sidenote">Data sources provided at the end of this post. To
simplify the message, I'll focus on the county level data for this post</span>.

![](/Images/blog_figures/red_blue_coronavirus/counties_redo.png)

But I have added two lines corresponding to our expectation of no difference. **If there were no
difference in disease risk between counties that voted for Trump and those that voted for Clinton,
we would expect the new cases to converge to these lines**, which indicate the proportion of the
population that lives in red and blue counties.

Another way to measure the effect of one variable on another (*e.g.*, the uneven distribution of
people in space on disease prevalence), is to remove the expected effect through normalization.
Here, we can try to remove the effect of differing population sizes by converting the counts of new
cases daily to the number of new cases *per capita* daily. As mentioned in [previous
posts](Comparing_COVID-19_Responses), this is done through dividing the number of new cases in each
county by the population of that county. In this way, we make counties more comparable by removing a
known difference.

![](/Images/blog_figures/red_blue_coronavirus/counties_percapita.png)

When normalized in this way, we expect that (in the absence of further variables of importance) the
curves should converge to the proportion of all couties that are red/blue. Note that this
convergence is not complete: there are more per-capita-cases than expected in blue counties and
fewer than expected in red counties -- the opposite of what we would have expected from the original
plot. This suggests our normalization did not account for all of the differences in cases between
blue and red counties<sup><label for="sneakpeek" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="sneakpeek" class="margin-toggle"/><span
class="sidenote">Keep your eyes peeled for an upcoming post looking at the best normalization for
COVID data (spoiler alert: it involves density)</span>. We don't know from this analysis, however,
if that difference is something inherently political (*i.e.* relating to policy differences) or some
further demographic difference.

## So what is going on here?

If the story is <s>just</s> mostly to do with population, why hasn't it always been this way? In
other words, why does the plot start out with almost all of the new cases in blue counties? My
explanation here is a bit [hand-waving](https://en.wikipedia.org/wiki/Hand-waving), as I don't have
data to suggest this is the actual course of events, but I would posit that what we are seeing is
the effects of a slow diffusion of the disease out of the major cities (where it was first
introduced due to a higher rate of international immigration) and into the surrounding countryside.
In fact, this interpretation holds if you further break up the data, for instance into high, medium,
and low density counties.

![](/Images/blog_figures/red_blue_coronavirus/counties_by_density.png)

## Take note!

Importantly, none of the above graphics suggest that the daily number of new cases is declining.
These are all discussions about the relative rates in red/sparse and blue/dense counties. Indeed,
the number of new cases being reported each day is rapidly increasing in both red and blue counties,
and it is up to each of us to do our part to limit disease spread, especially to those most
vulnerable in our communities, through mask-wearing and social distancing when possible.

 ![](/Images/blog_figures/red_blue_coronavirus/total_newcases.png)

### Data Sources:

[COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19/)

[MIT Election Data and Science Lab, 2018, "County Presidential Election Returns 2000-2016", Harvard Dataverse, V6, UNF:6:ZZe1xuZ5H2l4NUiSRcRf8Q](https://doi.org/10.7910/DVN/VOQCHQ)

[US census data on county population and land area](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-detail.html)
