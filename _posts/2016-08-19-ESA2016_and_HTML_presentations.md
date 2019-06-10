---
layout: post
title: "ESA2016 & HTML presentations"
author: "Matthew J. Michalska-Smith"
date: "2016-08-19"
categories: site_update presentation methods
---

# Welcome to the site!

Last week, I attended the [101st Annual Meeting of the Ecological Society of
America](http://esa.org/ftlauderdale/ "ESA2016") in Ft. Lauderdale, FL. While
there I was able to meet up with several colleagues I hadn't seen in a long time
and see many interesting talks. I also gave a talk of my own, where I presented
some recently submitted work on distinguishing ecological categorizations or
roles (e.g. parasites vs predators) using a purely statistical consideration of
the network structure. For this, we utilized (and enhanced) the group model
which was introduced to Ecology by my adviser ([Stefano
Allesina](http://allesinalab.uchicago.edu/)) and committee member ([Mercedes
Pascual](http://pondside.uchicago.edu/ecol-evol/people/pascual.html)) [back in
2009](http://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2009.01321.x/abstract
"Food web models: a plea for groups").

<!--more-->

For my last few presentations, I have been experimenting with HTML slides using
[reveal.js](https://github.com/hakimel/reveal.js/). I really like how these
slides look (when done properly), but there are some additional complications
that arise (when compared to using
[Beamer](https://en.wikipedia.org/wiki/Beamer_(LaTeX))), most of which are due
to changing display resolutions. In my previous presentations, I had not
experienced the consequences of this, mainly because I was using figures with
approximately square dimensions and modern computers/projectors with high
resolutions.For my talk at ESA this year, I violated all of these and ended up
spending a lot of time and stress trying to optimize my slides for the
resolution of the projector, with the end result of still having the edges of my
slides cut off and the image blurry. To be fair, it was not clear how much of
this would have still been the case with a PDF, but the hours put into trying to
account for any possibility and then still not having the presentation go as I'd
planned left me feeling a little dejected and reconsidering my decision to use
HTML slides.

Maybe it was time to go back to Beamer.

But then I thought through the elements that got me excited about HTML slides in
the first place and decided to give it at least one more go.

HTML and LaTeX are both of the [WYSIWYM](https://en.wikipedia.org/wiki/WYSIWYM
"what you see is what you mean") school of document preparation, both look
professional out of the box<sup><label for="One" class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="One" class="margin-toggle" /><span class="sidenote">Note that HTML *per se* does not look good without some effort, but when using a package of css and javascript, such as reveal.js, it is more comparable.</span>, and both are pretty easy to do
simple presentations in. The biggest advantage of Beamer presentations is that
they easily compile into PDFs which are supremely portable, with consistent
(identical, really) display on any device or resolution. While HTML slides
(there are several options out there to make HTML slides, but I am only familiar
with reveal.js) have the capability to export as PDFs, I have not been
successful in getting this to work for anything but very simple slide
constructions<sup><label for="Two" class="margin-toggle sidenote-number"></label></sup><input type="checkbox" id="One" class="margin-toggle" /><span class="sidenote">An alternative to the built-in PDF rendering of reveal.js is a third party program called [DeckTape](https://github.com/astefanutti/decktape). In my experience, this produced better results (more specifically, it worked, while the other did not), but still failed on the more complicated slides.</span>.

So what is it about HTML that keeps me coming back?

## it looks better

This is clearly a subjective point, but I think I can saw with certainty that
there are more mechanisms in place (from slide transitions, to parallax
backgrounds, to sub-slides, to a slide-space geometry in
[impress.js](https://github.com/impress/impress.js/) or
[jimpress.js](http://jmpressjs.github.io/jmpress.js/#/home), to any animation
you can program with a little proficiency in
[javascript](https://www.javascript.com/) or the even simpler
[jQuery](https://jquery.com/)) to add aesthetics to your slides.

## media embedding

This can also be done with Beamer, but it is quite a bit easier with HTML,
especially if you are presenting online where you can simply link to a YouTube
video that will play automatically in your slide without having to open another
window. Likewise, you can link out to another website and return to the same
slide by simply clicking the back button.

## interactive presentations

I haven't utilized this functionality at all, but with jQuery, it is possible to
make your presentation interactive in any way you want, be it a game of jeopardy
(with slide linking navigation), a multiple choice quiz (with answer checking),
or a notes section for people to keep notes which can then be exported as a text
file.
<!-- https://thiscouldbebetter.wordpress.com/2012/12/18/loading-editing-and-saving-a-text-file-in-html5-using-javascrip/ -->

## adaptive display

This is a pretty big divergence from PDFs, representing, in a sense, an
alternative approach to the problem of multiple devices/display resolutions.
While PDF solves the problem with consistency, HTML with the help of
[bootstrap](http://getbootstrap.com/) solves it with adaptive response. If you
want to see what I mean (and are on a large screen), try changing the size of
the window you are reading this in -- see how the font size stays the same, but
the wrapping changes? this is adaptive display at work. More sophisticated
changes are possible as well, such as how the orientation of the pictures and
descriptions changes in the projects tab of this site or how when the window
gets too small (or you are using a mobile device) the menu switches to a
drop-down rather than a header. You can make your slides adapt in this way too,
such that mobile and desktop/projector viewers each get an experience optimized
for their screens.

## accompanying notes

Finally, and perhaps most importantly for me, it is possible to incorporate
additional notes which can be used in a "speaker view" or made visible to
increase comprehension of slides outside of the context of the talk.

This is especially appealing to me as I struggle with balancing slides that
works well for a presentation but still stand alone on a website. You can check
out my solution in the [slides from this past ESA](/talks/ESA_08.2016/){:target="_blank"} and let me know what you think.

P.S. The addition of this blog and a soon-to come more comprehensive "talks"
page constitute the largest enhancements to the site since it was first
made available. As I continue to improve the site, I appreciate any feedback on
usability or desired additional features. Just shoot me an
[email](mailto:mjsmith@uchicago.edu) with your suggestion or comment.
