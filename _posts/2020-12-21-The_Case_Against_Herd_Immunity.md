---
layout: post
title: 'The case against "herd immunity" through infection'
author: "Matthew J. Michalska-Smith"
date: "2020-12-21"
categories: current_events
usemathjax: true
---

As COVID-19 cases continue to spike and governments implement stronger and stronger restrictions, I
thought it might be worth visiting an argument that I have encountered on and off over the past
year. In its most basic form, the argument goes "why not simply let the pandemic run its course
while protecting the vulnerable as best we can, but not worrying overly much (indeed, perhaps even
encouraging) those young and healthy individuals to contract the virus in a push toward herd
immunity?"
<!--more-->
At first glance, depending on which political sphere you receive your news from, that
sentence might fill you with either righteousness or abject terror, but I have been struck
by the lack of nuance in the way this argument is covered and wanted to dive a little deeper here.
First, however, we have to take care of some jargon...

### What is "herd immunity"

The term herd immunity initially gained popularity in the context of vaccination. It was observed
that one did not have to vaccinate every individual in the population to curb disease spread, but
rather, the rate of the spread of disease appeared to be non-linearly related to the percentage of
the population that was still susceptible. Put more simply, the number of people saved from a
disease is not proportional to the number of people vaccinated: at low vaccination levels, fewer
people are saved than are vaccinated because no vaccine is 100% effected; at high vaccination rates,
even more people are saved because the pathogen has trouble finding new viable hosts. The key
takeaway here is that eradication of a disease is possible even without vaccinating everybody.

Importantly, the percentage that is actually needed to set a disease on the path to extinction
depends on two factors:

1. the social network structure in the population, and
2. how easily the disease in question is passed from person to person

Traditionally, scientists have ignored the former, assuming a well-mixed<sup><label for="mixed"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="mixed"
class="margin-toggle" /><span class="sidenote">This is just a fancy way of saying that each
infectious individual has the same chance to infect any susceptible individual in the
population</span> population. In such a simplified scenario, there is also a nice relationship
between the reproductive number $$\mathcal{R}_0$$<sup><label for="r0" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="r0" class="margin-toggle" /><span
class="sidenote">Recall that this is the expected number of new infections resulting from an initial
infectious individual in an otherwise completely susceptible population</span> and the percentage
needed for herd immunity $$p$$:

<div class="mathblock">$$p = 1 / \mathcal{R}_0 .$$</div>

In other words, for a disease like COVID-19 with an $$\mathcal{R}_0 \approx 3$$, we would need to
have at least 67% of the population immune before the pandemic would die out. 

Unfortunately, our population is not well-mixed, and this can affect the proportion needed for herd
immunity in complicated ways, depending both upon the structure of the network and upon which
individual is chosen as the initial case. Consider some extreme examples: if the initially infected
person only interacts with one other person, a well-placed vaccine could prevent any further
infection. If the contact network formed a star, on the other hand, then vaccinations do nothing
beyond protecting the individual actually being vaccinated.

## Why should I<sup>\*</sup> care?

\* when I am a young, healthy individual

Many of the critiques I hear about current shutdown policies are based on a conception that there is
very little risk involved when infected with COVID-19, so long as you are young and healthy (*i.e.*
you don't have a pre-existing condition that makes you more vulnerable to the disease), but this
confounds the risk of **death** with the risk of **undesirable outcomes**. It is true that the death
rate for COVID-19 is much lower than we initially thought (more on this later), but death is not the
only outcome worth avoiding. Here are some other reasons why it might be better to avoid getting
sick, if you can help it.

### risk of transmission to others

One of the reasons it is so important to reduce your risk of infection is because your choices have
effects far beyond yourself. Even if you manage to avoid the above risks, every additional
infectious individual in the population adds additional risk of further infections, including to
those less fortunate than yourself. Importantly, many of the conditions correlated with worse
outcomes from COVID-19 are not obvious when looking at another person, meaning there is no way for
you to ensure you do not spread the disease to someone who might be more likely to die should they
be infected. Even if the first person you infect is not at risk, we are all intimately connected
with one another through (surprisingly short) chains of interactions—just think of the
popular-science concept of "[Six Degrees of
Separation](https://en.wikipedia.org/wiki/Six_degrees_of_separation)." This close connection between
individual actions and population-level consequences is the key justification for vaccination
requirements in schools across the United States<sup><label for="malone" class="margin-toggle
sidenote-number"></label></sup><input type="checkbox" id="malone" class="margin-toggle" /><span
class="sidenote">[Malone & Hinman
2003](https://www.cdc.gov/vaccines/imz-managers/guides-pubs/downloads/vacc_mandates_chptr13.pdf)</span>.

### risk of side effects and sequelae

While about 98% of all patients with COVID-19 will not die during the course of their illness, it is
important to note that death is not the only adverse outcome. SARS-CoV-2 attacks multiple organ
systems throughout a host individual, affecting the lungs, liver, kidneys, heart, and brain. There
is increasing evidence that even "mild" cases of COVID-19 can leave patients with serious and
lasting damage<sup><label for="scimag" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="miscscimagrisk" class="margin-toggle" /><span class="sidenote">For a more
comprehensive piece on this, check out [this one from
*Science*](https://www.sciencemag.org/news/2020/07/brain-fog-heart-damage-covid-19-s-lingering-problems-alarm-scientists)
and [this site compiling testimonials](https://www.post-covid.org.uk/get-support/) from individuals
recovering from COVID-19</span>. For instance, [a study of 538 survivors of
COVID-19](https://www.clinicalmicrobiologyandinfection.com/article/S1198-743X(20)30575-9/fulltext),
revealed that nearly half of them had persistent symptoms (usually respiratory) three months after
being discharged from the hospital. Some of the side effects can be quite severe, including
[strokes](https://link.springer.com/article/10.1007/s12975-020-00818-9), [neurological
disorder(s)](https://academic.oup.com/brain/article/143/10/3104/5868408), [kidney
disease](https://link.springer.com/article/10.1007/s40620-020-00851-9), and [psychiatric
disorder(s)](https://www.sciencedirect.com/science/article/pii/S0278584620303626?via%3Dihub) (such as
anxiety and depression), just to name a few.

Finally, though children have made up a small proportion of the total number of confirmed cases of
COVID-19, those that do contract the disease have a small<sup><label for="miscrisk"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="miscrisk"
class="margin-toggle" /><span class="sidenote">Estimated to be as high as 0.1-0.5%</span> risk of
developing a complex vasculitis which is being called [Multi-system Inflammatory Syndrome in
Children
(MIS-C)](https://journals.lww.com/pidj/Fulltext/2020/11000/A_Systematic_Review_of_Multisystem_Inflammatory.3.aspx).

Finally, there is the possibility of side effects that will only reveal themselves years
afterward<sup><label for="sequelae" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="sequelae" class="margin-toggle" /><span class="sidenote">sometimes called
[sequelae](https://www.merriam-webster.com/dictionary/sequela), or "late-effects" in medical
literature</span>. This may sound like science fiction, but is actually documented for a number of
common diseases. For instance, [about 1 in 10,000 individuals that have measles as children will
develop a neurodegenerative disorder 7-10 years after seeming to have recovered from their
illness](https://www.cdc.gov/measles/symptoms/complications.html). And individuals who have had
polio can sometimes see their [symptoms return more than 30 years (!) after recovering from their
initial
infection](https://www.mayoclinic.org/diseases-conditions/post-polio-syndrome/symptoms-causes/syc-20355669).
Because these can be so varied and occur so long after someone is first exposed to the pathogen, we
really have no idea whether or not there will be syndromes associated with past COVID infection at
this point.

### risk of disease mutation

This one is a bit more abstract, but bear with me. Every time a virus replicates, there is a chance
that it will make a mistake in the copy and produce a new mutation. Most of these mutations will
either have no effect or be detrimental to the virus in some way<sup><label for="mutations"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="mutations"
class="margin-toggle" /><span class="sidenote">for instance, making it less able to infect new
cells</span>, but there is some chance each time this happens that the mutation will actually make
the virus worse (from our perspective). There is some chance that this newly mutated virus is
*better* able to infect new hosts, or that it is more likely to kill those that it infects.
Moreover, some mutations, such as increased spread rate, are even favored by evolution, increasing
the likelihood that they stick around should they arise.

Because mutation rates are fairly constant between individual virus particles, the more viruses
there are in the environment (*i.e.* the more people there are infected with COVID-19), the more
potential there is for novel mutations to arise. Thus, it is beneficial for fewer people to get
infected overall, but, in particular, it is beneficial for the population size of the
virus<sup><label for="viruspop" class="margin-toggle sidenote-number"></label></sup><input
type="checkbox" id="viruspop" class="margin-toggle" /><span class="sidenote">here I mean the total
number of virus particles present in the world</span> to be small when we are implementing a
vaccination campaign. This is because a smaller population size means that those rare variants of
the virus that are able to evade the vaccine are less likely to exist in the population to continue
spreading the disease after widespread vaccination.

### risk of death

Finally, I can't make a post like this without noting that there **is** some (small) risk that even
the healthy and young might die from COVID-19. I do not want to exaggerate this risk—it is less than
that for influenza for individuals under 65
<!-- <sup><label for="coviddeath" class="margin-toggle sidenote-number"></label></sup> -->
—but it is not 0, and when we consider that there are 275 million
individuals in this age range in the US, even this small risk could tally to more than 250,000 dead.

<!-- | Age (years) | COVID-19<sup>a</sup> | Influenza<sup>b</sup> | Ratio<sup>c</sup> |
|------:|-------:|-------:|-----:|
| 0-4   | 0.003% | 0.007% | 0.43 |
| 5-17  | 0.001% | 0.003% | 0.33 |
| 18-49 | 0.045% | 0.209% | 0.22 |
| 50-64 | 0.327% | 0.615% | 0.53 |
| 65+   | 3.312% | 0.831% | 3.99 | -->
<label for='coviddeath' class='margin-toggle'> &#8853;</label><input type='checkbox' id='coviddeath' class='margin-toggle'/><span class='marginnote'>
<!-- <input type="checkbox" id="coviddeath" class="margin-toggle" /><span class="marginnote"> -->
<sup>a</sup><a href="https://www.nature.com/articles/s41586-020-2918-0">O'Driscoll *et al*. 2020</a><br>
<sup>b</sup><a href="https://www.cdc.gov/flu/about/burden/2018-2019.html">Numbers from the 2018-19 US influenza season</a><br>
<sup>c</sup> <i>i.e.</i> given that you are infected, you are $$x$$ times more likely to die from COVID than from flu
</span>

<div class="table-wrapper" style="margin: 1em;">
<table>
    <thead>
        <tr>
            <th>Age (years)</th>
            <th>COVID-19<sup>a</sup></th>
            <th>Influenza<sup>b</sup></th>
            <th>Ratio<sup>c</sup></th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>0-4</td>
            <td>0.003%</td>
            <td>0.007%</td>
            <td>0.43</td>
        </tr>
        <tr>
            <td>5-17</td>
            <td>0.001%</td>
            <td>0.003%</td>
            <td>0.33</td>
        </tr>
        <tr>
            <td>18-49</td>
            <td>0.045%</td>
            <td>0.209%</td>
            <td>0.22</td>
        </tr>
        <tr>
            <td>50-64</td>
            <td>0.327%</td>
            <td>0.615%</td>
            <td>0.53</td>
        </tr>
        <tr>
            <td>65+</td>
            <td>3.312%</td>
            <td>0.831%</td>
            <td>3.99</td>
        </tr>
    </tbody>
</table>
</div>

It is also worth pointing out that death rates are not static, and while we usually think about
changes being for the better (improved therapeutics, better management, *etc*.), it can also change
for the worse depending on the number of cases circulating at a given time. [As hospital beds fill
up, mortality rises](https://link.springer.com/article/10.1007%2Fs11606-016-3936-3) because we are
no longer able to provide the best possible care to all patients that need it<sup><label
for="scarcity" class="margin-toggle sidenote-number"></label></sup><input type="checkbox"
id="scarcity" class="margin-toggle" /><span class="sidenote">Note that this decline in care quality
actually happens *before* a give resource is completely used up, as doctors begin to be both
overworked and possibly rationing the supplies that are left</span>.

## Final thoughts

This is not all to say that we should all lock ourselves in our homes until a vaccine is ready for
mass distribution—such a course would not be without its own costs, both financially and in the
psychiatric wellbeing of the individuals involved—but what I **do** mean to say is that the decision
of whether or not to lock down is far from trivial. Neither is is a simple case of "letting people
make their own choices." This individualistic mindset does not work in matters of public health:
vaccines (and face-masks) only work if enough people participate<sup><label for="nonlinear"
class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="nonlinear"
class="margin-toggle" /><span class="sidenote">because of the underlying non-linearity of disease
processes</span>.

We all have to do our part to reduce the spread of COVID-19, for those most vulnerable in our
population, but also for ourselves.
