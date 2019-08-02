library(magrittr)
library(tidygraph)
library(ggraph)
library(tidyverse)
library(broom)
library(xtable)

source("~/Research/CompetetiveTradeoff/Code/tidy_tests.R")

OVERLAP_LINK_CUTOFF <- 0.75
theme_set(theme_bw())
my_cols <- c("Bacteria -> Bacteria"="#006989",
             "Bacteria -> Fungi"="#6a526b",
             "Fungi -> Bacteria"="#6a526b",
             "Fungi -> Fungi"="#9b1d20")

raw_network_data <- read_csv("~/Research/CompetetiveTradeoff/Data/complete_competetive_index.csv",
                             col_types="ccccccccdddddd") %>%
  filter(leaf_x == leaf_y) %>%
  mutate(isolate_x = str_c(isolate_x, type_x, sep="."),
         isolate_y = str_c(isolate_y, type_y, sep=".")) %>%
  do(bind_rows(transmute(., treatment = treatment_x,
                         leaf         = leaf_x,
                         row_type     = type_x,
                         row_isolate  = isolate_x,
                         col_type     = type_y,
                         col_isolate  = isolate_y,
                         overlap      = x_on_y_pw),
               transmute(., treatment = treatment_x,
                         leaf         = leaf_x,
                         row_type     = type_y,
                         row_isolate  = isolate_y,
                         col_type     = type_x,
                         col_isolate  = isolate_x,
                         overlap      = y_on_x_pw))) %>%
  mutate(row_type = str_extract_all(row_isolate, "\\w+$") %>% str_replace_all(c("bacto"="Bacteria",
                                                                                "fungi"="Fungi")),
         col_type = str_extract_all(col_isolate, "\\w+$") %>% str_replace_all(c("bacto"="Bacteria",
                                                                                "fungi"="Fungi")),
         type=str_c(row_type, " -> ", col_type),
         treatment=str_replace_all(treatment, c("C"="Control", "N"="NPK Supplemented")))

ggplot(raw_network_data) +
  aes(x=overlap, fill=type, colour=type) +
  geom_histogram(bins=50) +
  geom_vline(aes(xintercept=OVERLAP_LINK_CUTOFF), size=0.75) +
  facet_grid(treatment~type) +
  scale_colour_manual(values=my_cols) + scale_fill_manual(values=my_cols) +
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  xlab("Niche Overlap") + ylab("Count") +
  theme(legend.position="none")

ggsave("../Figures/NO-histogram.svg", width=8.67, height=4.5)
