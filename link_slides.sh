#!/bin/bash

# this script creates symbolic links to all reveal.js index.html
# files in the _includes directory file structure of jekyll


# first the reveal.js presentations
for dd in $(ls -d talks/*/)
do
    # first make sure all the subdirectories are in place
    mkdir -p _includes/$dd
    # now copy over the files
    ln ${dd}index.html _includes/$dd
done

# also copy over any beamer presentations
ln talks/*.pdf _includes/talks/
