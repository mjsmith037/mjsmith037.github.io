library(magrittr)
library(tidygraph)
library(ggraph)
library(tidyverse)
library(broom)
library(kableExtra)

tidy_p_value <- function(pvals) {
  ifelse(pvals < 0.001, "< 0.001", format(pvals, digits=0, nsmall=3))
}

tidy_t_test_long <- function(dat, grouping, variable) {
  factor_levels <- unique(dat[[grouping]])
  t.test(filter(dat, !!sym(grouping) == factor_levels[1]) %>% .[[variable]],
         filter(dat, !!sym(grouping) == factor_levels[2]) %>% .[[variable]]) %>%
    tidy() %>%
    select(estimate1, estimate2, p.value) %>%
    set_names(c(factor_levels, "p.value"))
}

tidy_ks_test_long <- function(dat, grouping, variable) {
  factor_levels <- unique(dat[[grouping]])
  ks.test(filter(dat, !!sym(grouping) == factor_levels[1]) %>% .[[variable]],
          filter(dat, !!sym(grouping) == factor_levels[2]) %>% .[[variable]]) %>%
    tidy() %>%
    mutate(p.value = tidy_p_value(p.value))
}

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

p <- ggplot(raw_network_data) +
  aes(x=overlap, fill=type, colour=type) +
  geom_histogram(bins=50) +
  facet_grid(treatment~type) +
  scale_colour_manual(values=my_cols) + scale_fill_manual(values=my_cols) +
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1")) +
  xlab("Niche Overlap") + ylab("Count") +
  theme(legend.position="none")

ggsave(p, "../Figures/NO-histogram_no_line.svg", width=8.67, height=4.5)
ggsave(p + geom_vline(aes(xintercept=OVERLAP_LINK_CUTOFF), size=0.75), "../Figures/NO-histogram.svg", width=8.67, height=4.5)

full_join(
  raw_network_data %>% group_by(type) %>% do(tidy_t_test_long(., "treatment", "overlap")),
  raw_network_data %>% group_by(type) %>% do(tidy_ks_test_long(., "treatment", "overlap")),
  by="type"
) %>%
  ungroup() %>%
  select(-statistic, -alternative, -method) %>%
  rename_all(. %>% str_replace_all(c("\\.x$"="_t_test", "\\.y$"="_ks_test"))) %>%
  mutate(type = str_replace_all(type, "->", "$\\\\rightarrow$")) %>%
  mutate_if(is.numeric, function(x) str_c("$", format(x, digits=3, nsmall=3), "$")) %>%
  set_names(c("Interaction Type", "NPK Supplemented", "Control", "T-test p-value", "KS-test p-value")) %>%
  kable(row.names=FALSE, escape=FALSE)

