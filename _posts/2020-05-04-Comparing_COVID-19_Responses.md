---
layout: post
title: "Comparing COVID-19 Responses"
author: "Matthew J. Michalska-Smith"
date: "2020-05-04"
categories: current_events basic_statistics
---

There has been a lot of talk about different countries' responses to the ongoing COVID-19 pandemic,
but how can we tell which responses are better than others? Even if one country has fewer
cases/deaths/*etc.* than another, does that mean their approach is transferable?

<!--more-->

When comparing stats<sup><label for="lastpost" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="lastpost" class="margin-toggle"/><span
class="sidenote">like those mentioned [in the last post](Resources_for_Tracking_the_COVID-19_Epidemic)</span> between countries, there are several considerations that need to be made.

First, one must be sure the numbers are normalized by population. A reasonable expectation is that,
ideally, this would be normalizing by population **density** rather than just population, but doing
so for countries is complicated by its highly heterogeneous nature. Namely, there are very dense
(*e.g.* New York City at more than 26 000 people/mile<sup>2</sup>) and very sparse (*e.g.* the state
of Wyoming with only 6 people/mile<sup>2</sup>) areas within the same country. Because of this,
calculating population density by dividing total population by total land area often leads to a
number that is markedly different from all but a vanishingly small proportion of the actual
landscape. Since we can't easily normalize by density, most analyses/visualizations I have seen
instead settle for normalizing by population size.

Second, the fatality rate depends on variables such as age demographics and the distribution of
co-morbidities<sup><label for="comorb" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="comorb" class="margin-toggle"/><span class="sidenote">other illnesses and
conditions that affect the severity of one's infection with COVID-19</span>. These are also highly
heterogeneous, but additionally are very rarely reported on due in part to health privacy concerns.
In general, we might be able to have country-wide summary statistics for some co-morbidities in some
countries, but lack the widespread consistency needed to make broad comparisons. As a result, these
numbers are very rarely included in the analyses and visualizations you will find online.

Third, testing intensity differs across localities, and testing is strongly correlated with the
number of cases detected<label for="testfig" class="margin-toggle">&#8853;</label><input
type="checkbox" class="margin-toggle" id="testfig"/><span class="marginnote"><a
href="/Images/blog_figures/cases_by_tests.png"><img src="/Images/blog_figures/cases_by_tests.png" alt="A plot relating testing
to number of cases"/></a></span> and death rates. Put simply, the more tests you perform, the more
cases you will detect<sup><label for="testing" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="testing" class="margin-toggle"/><span
class="sidenote">though the percentage of positive tests out of tests performed will usually
decrease</span>. This is driven by insufficient testing, but is particularly difficult from a
statistical perspective, because we are not testing randomly. Instead, most countries are testing
individuals who come to the hospital and present with symptoms. This means we are grossly
underestimating asymptomatic cases and also inflating our calculations of deaths per confirmed case
("case fatality") by essentially substituting symptomatic cases<sup><label for="testissue"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="testissue"
class="margin-toggle"/><span class="sidenote">and more specifically only individuals who have
sufficient symptoms to seek medical care</span> for the total population prevalence. As testing gets
better, case fatality will become a more meaningful metric and a more useful point of comparison
between countries.

Fourth, COVID-19 was introduced to countries at different times. This means that one country might
be in a period of exponential growth, while another country is winding down. To compare overall
disease trajectories, it is thus necessary to normalize this timeline. The usual way to do this is
to set day "0" to be the day each country reached some threshold value for the metric of interest.
For example, the day each country reached 1 death per 1 million people<sup><label for="threshold"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="threshold"
class="margin-toggle"/><span class="sidenote">put another way, the day on which the number of deaths
divided by the population was greater than or equal to 0.000 001</span>.

Finally, there are issues of how cases are defined and how deaths are assigned. Moreover, this
definition has changed both between countries<sup><label for="casedef1" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="casedef1" class="margin-toggle"/><span
class="sidenote">[Onder, Graziano, Giovanni Rezza, and Silvio Brusaferro. "Case-fatality rate and
characteristics of patients dying in relation to COVID-19 in Italy." *JAMA*
(2020)](https://jamanetwork.com/journals/jama/fullarticle/2763667)</span>  and over time within the
same country<sup><label for="casedef2" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="casedef2" class="margin-toggle"/><span class="sidenote">[Tsang, Tim K., et al.
"Effect of changing case definitions for COVID-19 on the epidemic curve and transmission parameters
in mainland China: a modelling study." *The Lancet Public Health*
(2020)](https://www.sciencedirect.com/science/article/pii/S246826672030089X)</span> over the course
of this pandemic. Fortunately, there seems to be convergence on these definitions, so this is
becoming less of a problem (especially for comparing developed nations) as time goes on.

## So what are we supposed to do?

All of the above complexity leads one to a somewhat bleak perspective. No matter which two countries
are compared, there will always be a "just-so" story that can be concocted to invalidate the
comparison. Yet such comparisons are essential to learning from this experience and improving
responses in the event of another wave and/or the next pandemic.

So we make the best comparisons we can, normalizing for as many of the differences as possible. This
means:

1. Comparing *per capita*<sup><label for="percap" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="percap" class="margin-toggle"/><span
class="sidenote">the number of cases/deaths/*etc.* per individual in the population</span> values
rather than raw numbers, incorporating population density and demographics when possible.

2. Use numbers of deaths over number of cases (at least until testing reaches a sufficient level for
case fatality rates to be reliable). While there are also issues with counting deaths related to
COVID-19 it suffers from less variability than does number of cases and is less biased by testing
rates. Moreover, this is the most important number from a public health perspective. The number of
cases is rather irrelevant in the absence of severe health outcomes.

3. Be sure to align countries epidemic curves before comparing countries so that contries are compared at similar points along their respective epidemic trajectories.

All of these considerations are taken into account in the figure popularized by [The Financial Times](https://www.ft.com/coronavirus-latest) and used by my favorite COVID-tracking site
[91-DIVOC](http://91-divoc.com/pages/covid-visualization/)<sup><label for="voxvid"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="voxvid"
class="margin-toggle"/><span class="sidenote">check out [this great video](https://www.youtube.com/watch?v=O-3Mlj3MQ_Q) explaining these three points</span>.

## A case study: Sweden

Sweden has been in the US [news a lot
lately](https://news.google.com/search?q=sweden%20COVID-19&hl=en-US&gl=US&ceid=US%3Aen) as an
outlier in their governmental response to COVID-19. While the evaluation of their strategy is
rapidly becoming a partisan enterprise, lets see what the data show about their response, trying to
keep as much nuance in the discussion as possible. To help account for both demographic and
environmental confounders<sup><label for="confound" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="confound" class="margin-toggle"/><span
class="sidenote">variables that are correlated with the measures we care about, making it difficult
to say which is (are) the "true" cause of the change we see</span>, we will specifically compare
Sweden against it's immediate neighbors: Norway, Finland, and Denmark.

[Let's use these data for comparison.](https://91-divoc.com/pages/covid-visualization/?chart=countries-normalized&highlight=Sweden&show=highlight-only&trendline=default&y=fixed&scale=log&data=deaths&extra=Norway,Finland,Denmark#countries-normalized)

As of 4 May, Sweden has almost 260 deaths per 1 million people, followed by Denmark with 83, Finland with 42, and Norway with 39. Normalizing by the date of COVID-19 introduction, we can step back to "Day 37" since 1 death / million population (this is the latest date with all four countries represented). This corresponds to 4 May for Finland, 26 April for Sweden and Denmark, and 25 April for Norway. At this point, the death rates are Sweden: 212, Denmark: 72, Finland: 42, and Norway: 37. Put simply, Sweden has nearly three times the death rate of its next nearest neighbor and is the 10<sup>th</sup> highest death rate of all countries reporting at that time<sup><label for="microstates"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="microstates"
class="margin-toggle"/><span class="sidenote">8<sup>th</sup> if we exclude [San Marino](https://en.wikipedia.org/wiki/San_Marino) and [Andorra](https://en.wikipedia.org/wiki/Andorra), whose combined land area measures approximately 200 mi<sup>2</sup> -- [roughly the amount of parking space in LA County](http://www.betterinstitutions.com/blog/2016/1/2/map-a-parking-lot-with-all-of-la-countys-186-million-parking-spaces) -- and whose populations sum to just over 100 000</span>. This is despite [having less than 1.25 times as many confirmed cases as the next highest Nordic country](https://91-divoc.com/pages/covid-visualization/?chart=countries-normalized&highlight=Sweden&show=highlight-only&trendline=default&y=fixed&scale=log&data=cases&extra=Norway,Finland,Denmark#countries-normalized).

### A caveat

It is important to note that current death rates are not the only metric to consider when comparing
countries. Indeed the numbers we really want to compare are the true case fatality and the final
death rates, after the pandemic has passed<sup><label for="endemic" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="endemic" class="margin-toggle"/><span
class="sidenote">or at least moved into more of an endemic role, where we have seasonal outbreaks
like with influenza</span>. Perhaps Sweden will suffer less of a "second-wave" because of its less
restrictive lock-down now. It is hard to say whether or not they have greater population exposure,
and thus more herd immunity<sup><label for="herdimmune" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="herdimmune" class="margin-toggle"/><span
class="sidenote">a situation in which disease spread is inhibited by a large fraction of the
population already being immune</span>, because of insufficient testing. Herd immunity relies on a
large proportion of the population contracting the disease at some point, but the difference in
prevalence is not so great between Sweden and other European countries according to the data
reported so far.

One of the main reasons for governmental intervention in response to the pandemic has been to
"flatten the curve" and prevent an overwhelming of the health care system. Since none of these
countries have had their health systems overwhelmed, it remains a worthwhile
question why Sweden has such a higher death rate at this time.

<!--

link: [text](url "label [optional]"

footnote: <sup><label for="One" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="One"
class="margin-toggle" /><span class="sidenote">footnote text</span>

-->
