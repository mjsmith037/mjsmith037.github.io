library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)

NSTEPS <- 10000
NCHAINS <- 50
FNAME <- "Unsorted-10.txt"

S <- nrow(read.table(FNAME))
for (rseed in 1:20) {
    for (ngroups in 3) {
        system(paste("./FindGroups_DC", S, FNAME, rseed, NSTEPS, NCHAINS, ngroups, 0))
        system(paste("./FindGroups_DC", S, FNAME, rseed, NSTEPS, NCHAINS, ngroups, 1, 1, 1))
    }
}

outfiles <- list.files(pattern="beta-NA")
bestoutfile <- outfiles %>%
  str_extract_all("\\w+?-[\\d\\.]+", simplify=TRUE) %>%
  as.data.frame() %>%
  mutate_each(funs(as.numeric(gsub("\\w+-", "", .)))) %>%
  mutate(fname = outfiles) %>%
  arrange(V5, V3) %>%
  .$fname %>% .[1]

outfiles_DC <- list.files(pattern="beta-1")
bestoutfile_DC <- outfiles_DC %>%
  str_extract_all("\\w+?-[\\d\\.]+", simplify=TRUE) %>%
  as.data.frame() %>%
  mutate_each(funs(as.numeric(gsub("\\w+-", "", .)))) %>%
  mutate(fname = outfiles_DC) %>%
  arrange(V7, V3) %>%
  .$fname %>% .[1]
