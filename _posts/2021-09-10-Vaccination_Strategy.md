---
layout: post
title: "Why vaccination strategy is not as simple as it seems"
author: "Matthew J. Michalska-Smith"
date: "2021-09-10"
categories: current_events methods
---

When vaccination just started getting underway in the U.S., many people were confused and angered at
the fact that many localities were pursuing vaccination strategies that didn't just boil down to
vaccinating the most vulnerable. On the other hand, there was a significant movement to vaccinate
front-line workers that often cited equity and racial justice as motivations. What didn't seem to
make the news as often is the mathematics that make this a reasonable alternative to consider.

<!--more-->

The strategy of vaccinating those most likely to suffer adverse outcomes from COVID-19 is intuitive:
we first protect those most in need of protecting. Of course, when we phrase it this way, we see a
slight chink in the logical armor---while those with pre-existing conditions and the elderly are
[definitely more likely](https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-with-medical-conditions.html)
to suffer adverse outcomes<sup><label for="outcomes" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="outcomes" class="margin-toggle" /><span
class="sidenote">Note that I am explicitly not limiting this to death. [As noted
previously](../20/The_Case_Against_Herd_Immunity), there are a host of other reasons to want to
avoid getting COVID</span>, these consequences are also dependent on their risk of being exposed to
the virus.

## Key concept: diseases spread along contact networks

Key to understanding why there might be considerations beyond the health status of each individual
is the realization that we are all interconnected through a web of interpersonal interactions. In
disease ecology, we call this a "contact network." Directly communicated diseases such as
COVID<sup><label for="fomites" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="fomites" class="margin-toggle" /><span class="sidenote">
[Most](https://pubs.acs.org/doi/abs/10.1021/acs.estlett.0c00966)
[recent](https://journals.asm.org/doi/full/10.1128/mSphere.01070-20)
[research](https://www.nature.com/articles/s41598-021-95479-5) suggests that spread through virus
particles persisting on shared surfaces is rare for COVID, [especially outside of hospital
settings](https://link.springer.com/article/10.1007/s10654-021-00784-y) </span> rely on the innate
sociality of humans to spread from one host to another. What actually counts as a "contact" will
depend on the properties of the pathogen in question: we all have heard the "at least 15 minutes of
less than 6 feet proximity" rule-of-thumb for COVID, but for influenza, it might take more time or
shorter distances, while the delta variant of COVID might be able to spread further or faster.
Importantly, **these are all based on probability distributions** and unlucky individuals
might get infected over much longer distances or just passing by an infectious individual.
Similarly, someone not getting infected after spending a day with an asymptomatic carrier does not
disprove the general trend.

Here is a conceptual diagram<sup><label for="concept" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="concept" class="margin-toggle" /><span
class="sidenote">That is, figure not meant to represent actual data, but rather to
help us think about a problem</span> of what these guidelines are trying to capture:

![](/Images/blog_figures/vaccination_strategy/risk_distribution.png)

Note that there is some probability of being infected in any given moment that depends on the
distance you are from an infectious person. Similarly, the longer you are near that person, the more
likely you are to get infected yourself. The guideline is a way of simplifying the message that, for
example, you have a 1 in 3 chance of being infected by the time you have spent 15 minutes within 6
feet of an infectious person (the upper-left region of the heatmap inside the blue curve).

<label for="netfig" class="margin-toggle">&#8853;</label><input type="checkbox"
class="margin-toggle" id="netfig"/><span class="marginnote" style="text-align:right;"><a
href="/Images/blog_figures/vaccination_strategy/network.png"><img
src="/Images/blog_figures/vaccination_strategy/network.png"/></a><br>A simple contact network</span>

When we consider every interaction between every pair of individuals in a population (possibly even
varying through time), we end up with data that can be visualized as a network, where the "nodes"
are individuals, and the "links" are interactions that have the potential to spread disease. Because
diseases spread along this network of interactions, we can begin to see how vaccinating certain
individuals (such as the blue nurse at the center of our plotted network) can have a
disproportionate impact on the spread of disease. This effect can be further amplified when a
person's job involves a risky behavior in addition to a large number of interpersonal interactions
(consider, for example, dentists, whose work involves increase exposure to aerosolized particles
*and* interacting with many people in close contact and for extended periods of time).

## An alternative to vaccinating by health status

So, how do we operationalize this network perspective? Put simply, a network view of vaccination
suggests we should additionally consider both **exposure risk** (the risk of being infected) and
**spread risk** (the risk of passing the pathogen along to others).

### Countervailing consideration 1: exposure risk

Thinking about differential risk of exposure across the population is complicated because it depends
on many interacting variables, such as the likelihood of a person interacting with a sick person (note
this depends both on the person's interpersonal interactions as well as the prevalence of disease in
the population), the personal behavior of both people involved in the interaction, and even
environmental features such as airflow. A person who largely stays home and minimizes their contact
with others has a low risk of being infected by others, whereas a dentist (as mentioned above) has
a much higher risk.

### Countervailing consideration 2: spread risk

A second, related, consideration is how each individual contributes to the overall risk of a disease
spreading through a population. For this, we have to consider how each person fits within the wider
social contact network along which communicable diseases spread. In many ways, this consideration
overlaps with exposure risk: someone with few interpersonal interactions is unlikely to contribute
to further disease spread even if they do happen to get sick. Yet, there are also situations where
these become decoupled. A waiter, delivery person, or teacher, on the other hand, has both a higher
likelihood of being infected **and of passing that infection along to others** as a result of their
position within the contact network.

## But which is the better course?

Thus far, I've focused on explaining an alternative to the health-status approach to vaccination,
using contact network structure as a conceptual framework, but, with the obvious caveat that this is
not a [dichotomous](https://en.wikipedia.org/wiki/Dichotomy)<sup><label for="dichotomous"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="dichotomous"
class="margin-toggle" /><span class="sidenote"> It is not a choice between two exclusive options
(like True/False, Black/White, etc.); there are hybrids and alternatives not discussed here </span>
choice, how should we be administering vaccinations in response to COVID-19?

A recent study<sup><label for="science" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="science" class="margin-toggle" /><span class="sidenote">[Bubar, et al. *Science*
2021](https://www.science.org/doi/full/10.1126/science.abe6959)</span> looking into this question
found that the approach of vaccinating the more vulnerable is better if your goal is to minimize
deaths<sup><label for="cases" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="cases" class="margin-toggle" /><span class="sidenote">In contrast, targeting
younger people (who both make up a larger fraction of the population and are more likely to interact
with others) has a greater effect on reducing the overall case-burden</span>.

<label for="figinterp" class="margin-toggle sidenote-number"></label><input type="checkbox"
class="margin-toggle" id="figinterp"/><span class="marginnote">Note that the largest reduction in
deaths (highest curve on the right) corresponds to preferentially vaccinating the elderly.</span>

<figure class="fullwidth">
  <img style="max-width:750px; width:100%;" src="/Images/blog_figures/vaccination_strategy/modified_fig.png"/>
  <figcaption>Modified from Figure 1 of <a href="https://www.science.org/doi/full/10.1126/science.abe6959">Bubar, et al. 2021</a>.</figcaption>
</figure>

Some subset of readers are at this point exclaiming that "of course! this was obviously going to be
the result," and are probably wondering why they've wasted their time reading this whole
post.<sup><label for="thanks" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="thanks" class="margin-toggle" /><span class="sidenote">THANK YOU!, by the way <i
class="fal fa-smile-beam"></i></span> It is important to remember that this didn't have to be the
case, and indeed might not be the case for other diseases. Intuitions are not always correct, and we
need to do the research to be confident in our choices, especially when there are lives on the line.
