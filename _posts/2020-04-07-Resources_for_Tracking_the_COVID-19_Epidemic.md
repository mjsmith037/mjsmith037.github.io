---
layout: post
title: "Resources for Tracking the COVID-19 Epidemic"
author: "Matthew J. Michalska-Smith"
date: "2020-04-09"
categories: data_visualization current_events
---

<span class="sidenote">this post was inspired by [this recent blogpost](https://towardsdatascience.com/whats-wrong-with-covid-19-data-visualizations-and-how-to-fix-it-3cdc9adc774d)</span>

It is easy to succumb to number overload when faced with the incredible volume of data available
detailing the spread of the SARS-CoV-2 virus around the globe. As of the writing of this post, more
than 1.5 million individuals have been confirmed to have contracted COVID-19 and nearly 90
thousand individuals have died. As the pandemic has progressed, [dozens of
dashboards](https://github.com/CSSEGISandData/COVID-19/issues/576)<sup><label for="byod"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="byod"
class="margin-toggle" /><span class="sidenote">and even
[tutorials](https://www.freecodecamp.org/news/how-to-create-a-coronavirus-covid-19-dashboard-map-app-in-react-with-gatsby-and-leaflet/)
on how to make your own</span> have been created to track the climbing numbers each day. With so
many to choose from, how does one pick a trustworthy source? and what are the most important numbers
to consider anyway?

<!--more-->

## Trustworthy sources

Fortunately, in this age of open data, there are a number of trustworthy sources of data on the
spread of COVID-19. It seems like most dashboards draw from the [Johns Hopkins (JHU) data
feed](https://github.com/CSSEGISandData/COVID-19)<sup><label for="jhu" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="jhu" class="margin-toggle" /><span
class="sidenote">of course, JHU also has their own (popular)
[dashboard](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)</span>,
though I found the European Centre for Disease Prevention and Control to also have an [easy to
access data
stream](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)
(complete with example R code!).

## The numbers being reported

### Confirmed cases

The number of patients who have received a positive test.

This metric has some issues. First, it conflates two underlying processes: detection and prevalence.
While most people look to this number as a proxy for prevalence, this approach is made difficult by
the growing recognition of asymptomatic<sup><label for="asymp" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="asymp" class="margin-toggle" /><span
class="sidenote">people who do not show any visible sign of being infected, but can nevertheless
infect others</span> carriers in the population. Such people are unlikely to be tested, meaning
counts are almost certainly underestimates of the true number of cases.

Second, this number ignores the issues of test [sensitivity and
specificity](https://en.wikipedia.org/wiki/Sensitivity_and_specificity). Put simply, it ignores the
problem where (due to test failure) some of the people who test negative actually do have the
disease and some of the people who test positive are actually healthy. This is because [we just
don't know](https://www.statnews.com/2020/03/31/covid-19-overcoming-testing-challenges/) what these
numbers are yet, though the focus seems to be on [maximizing
specificity](https://www.fda.gov/media/135662/download)<sup><label for="fp" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="fp" class="margin-toggle" /><span
class="sidenote">minimizing the number of people who test positive, but are actually healthy</span>.
This compounds the asymptomatic issue, making these numbers doubly underestimates.

Finally, this number is [monotonically
increasing](https://en.wikipedia.org/wiki/Monotonic_function)<sup><label for="mono"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="mono"
class="margin-toggle" /><span class="sidenote">the number can only go up over time</span>. This
means that what we actually care about is the rate of change in this number, rather than the
absolute value *per se*. More on this later.

### Active cases

The number of confirmed cases that have not yet died or recovered.

Though obviously relying on "confirmed cases," and thus inheriting many of the associated issues mentioned above. This is an interesting metric insofar as it is the only one on this list that is not monotonic. This tracks the number of known infectious individuals in the population ata given time, and is expected to follow a typical epidemiological curve.

### Deaths

The number of people whose "cause of death" is COVID-19.

I find this to be the most important number for a couple of reasons. First, it is the most
dependable number—while there are some cases of people dying from co-morbidities which were
exacerbated by SARS-CoV-2 or simply not being tested, the majority of people dying from COVID are
identified and there are virtually no false positives.

### Recovered

The number of people who survived known infections with SARS-CoV-2.

This number is obviously just the difference of confirmed cases and deaths, but can be useful in
considering the proportion of the population with immunity to further infection. This proportion
becomes particularly important when thinking about [herd
immunity](https://en.wikipedia.org/wiki/Herd_immunity)<sup><label for="herd" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="herd" class="margin-toggle" /><span
class="sidenote">when there are enough immune individuals in a population to inhibit further
epidemics</span>.

## My top three dashboards

### 1. [91-DIVOC](http://91-divoc.com/pages/covid-visualization/)

This is my favorite visualization for understanding the spread of COVID-19. I look at each of the
four main plots each day, and they have recently added the [ability to keep track of county level
data as well](http://91-divoc.com/pages/covid-by-your-locations/) (which I use to keep tabs on
friends and family across the country). On the main page, there are four plots, grouped into two
classes. The first two look at absolute numbers at the country and US state levels, respectively.
And the second two look at numbers normalized by population size, again for countries and US states.

There are two key elements to these plots that make them more informative than most. First, the
plots horizontal axes are set by the number of days since a threshold level of cases was reached.
This is really important, as it allows comparison across localities that were exposed to the virus
at different times. Second, the plots vertical axis is in log scaling. Because these numbers are
growing exponentially, a log-scaled axis makes the lines close to linear and provides an intuitive
understanding of the [doubling time](https://en.wikipedia.org/wiki/Doubling_time)<sup><label
for="double" class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="double"
class="margin-toggle" /><span class="sidenote">the amount of time necessary for there to be twice
the current number of cases/deaths</span> each location is experiencing.

In the first two plots, I tend to focus on the "New Cases/Day" and "New Deaths/Day" metrics. By
looking only at the change in cases (deaths) from the previous day, we break the monoticity problem
mentioned above. This also gets a little closer to the perspective of the inspiration piece
mentioned at the top. Basically, I am looking for these curves to level off and begin to decline.
This would signify that we are on the tail side of the epidemic curve and on our way toward reducing
the total number of active cases.

In the second two, I focus on the "Active Cases" and "Deaths" metrics. Here, I am looking to compare
curves across localities, by normalizing both the timeframe (scaling to days since a threshold was
reached) and the number of cases (dividing by the population size), we put each locality on the same
footing. Thus, we can see that (as of today) the US looks a lot more like Italy than the UK in terms
of cases, but  pretty similar to Sweden in terms of number of deaths.

### 2. [University of Virginia](https://nssac.bii.virginia.edu/covid-19/dashboard/)

Similar to the popular JHU dashboard mentioned above, I like this one more mainly due to the
functionality of the chart on the left side, where you can both switch the counts to daily (see
above), and filter it to look at only the metric of interest (neither of which can be done on the
JHU site).

### 3. [A countervailing perspective](https://movement-wildlife.shinyapps.io/APP-2/)

**UPDATE (4 May 2020) : this site appears to no longer be functioning properly, so I have since stopped using it as a reference.**

This is probably one you haven't heard about. It is a relatively simple phenomonological<sup><label
for="phenom" class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="phenom"
class="margin-toggle" /><span class="sidenote">a model that only cares about prediction, not
underlying mechanisms</span> model that uses parameters from countries in the decline phase to
inform the shape of the epidemiological curve for countries earlier in their outbreaks. Put simply,
it is hard to know when the number of cases per day will begin to decline, so this model uses the
numbers from places where that is already happening to make a more educated guess.

I like this model because it is providing a perspective that is very different from the prevailing
media projections. I don't know which will end up being more accurate, but I find that looking at
this model helps keep me grounded.

<!--

link: [text](url "label [optional]"

footnote: <sup><label for="One" class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="One" class="margin-toggle" /><span class="sidenote">footnote text</span>

-->
