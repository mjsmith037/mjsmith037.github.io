"""<div id="slide_position">
  <span class="blue"><i class="fas fa-check-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i></span>
  <i style="padding-left:0.25vw;" class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i>
  <i style="padding-left:0.25vw;" class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i>
  <i style="padding-left:0.25vw;" class="fas fa-circle"></i><i class="fas fa-circle"></i><i class="fas fa-circle"></i>
  <i style="padding-left:0.25vw;" class="fas fa-circle"></i><i class="fas fa-circle"></i>
</div>"""

import re

## read in the files
with open("../index.html", "r") as htmlFile:
    htmlText = htmlFile.read()
## find all the sections
re.sub(r"(<section>\n\s+?<h4.+h4>)(.*?</section>)", r"\1\ntest\2", htmlText, re.DOTALL)
## build the slide progression html

## put it in each of the sections

## write out the new file
