library(tidygraph)
library(ggraph)
library(tidyverse)
library(xtable)
library(magrittr)

tidy_p_value <- function(pvals) {
  ifelse(pvals < 0.001, "< 0.001", format(pvals, digits=0, nsmall=3))
}

tidy_lm <- function(dat, formula) {
  lm(formula, data=dat) %>%
  {bind_cols(tidy(.) %>% mutate(estimate = str_c(str_c(formula)[2], " ~ ", round(estimate[1], 3),
                                                 "$ \\cdot $", str_c(formula)[3], ifelse(sign(estimate[2]) == 1, " + ", " - "), round(abs(estimate[2]), 3))) %>%
               filter(term != "(Intercept)") %>% select(term, estimate, p.value),
             glance(.) %>% select(adj.r.squared))} %>%
    mutate(p.value = tidy_p_value(p.value)) %>%
    select(-term)
}

tidy_aov <- function(dat, formula) {
  aov(formula, data=dat) %>%
    {bind_rows(tidy(.) %>% select(term, df, sumsq, meansq, p.value) %>% mutate(p.value = tidy_p_value(p.value),
                                                                               sumsq = round(sumsq, 3),
                                                                               meansq = round(meansq, 3)),
               glance(.) %>% select(adj.r.squared) %>% transmute(term = "Adjusted $R^2$", df=round(adj.r.squared, 5)))}
}

OVERLAP_LINK_CUTOFF <- 0.75

theme_set(theme_bw())
my_cols <- c("Bacteria"         = "#006989",
             "Fungi"            = "#9b1d20",
             "Mixed"            = "#6a526b",
             "Any"              = "black",
             "Control"          = "#575a3f",
             "NPK Supplemented" = "#d7471e")

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
  mutate(link = as.integer(overlap > OVERLAP_LINK_CUTOFF),
         row_type = str_extract_all(row_isolate, "\\w+$") %>% str_replace_all(c("bacto"="Bacteria",
                                                                                "fungi"="Fungi")),
         col_type = str_extract_all(col_isolate, "\\w+$") %>% str_replace_all(c("bacto"="Bacteria",
                                                                                "fungi"="Fungi")),
         interaction_type=str_c(row_type, " -> ", col_type),
         treatment=str_replace_all(treatment, c("C"="Control", "N"="NPK Supplemented")))

degree_dist_by_treatment_by_kingdom <- bind_rows(raw_network_data %>%
                                                   group_by(row_isolate, row_type, leaf, treatment, interaction_type) %>%
                                                   summarise(degree = sum(overlap)) %>%
                                                   mutate(direction = "out") %>%
                                                   rename(name = row_isolate, type = row_type),
                                                 raw_network_data %>%
                                                   group_by(col_isolate, col_type, leaf, treatment, interaction_type) %>%
                                                   summarise(degree = sum(overlap)) %>%
                                                   mutate(direction = "in") %>%
                                                   rename(name = col_isolate, type = col_type)) %>%
  ungroup() %>%
  filter(leaf != "C5") %>% ## Remove outlying network
  mutate(tmp = ifelse(interaction_type %>% str_split(" -> ") %>% invoke_map(equals, .), "within", "between")) %>%
  select(name, type, direction, treatment, tmp, degree) %>%
  spread(tmp, degree, fill=0)

ggplot(degree_dist_by_treatment_by_kingdom) +
  aes(x=within, y=between, colour=type) +
  geom_abline(colour="darkgray") +
  geom_point() +
  xlab("Degree between members of the same kingdom") +
  ylab("Degree between members of disparate kingdoms") +
  geom_smooth(method="lm", se=FALSE) +
  facet_grid(direction~treatment) +
  scale_colour_manual(values=my_cols) +
  theme(legend.position="none")
ggsave("../Figures/in-by-out-by-kingdom-degree-distributions.svg", width=7, height=7)

degree_dist_by_treatment_by_kingdom %>%
  group_by(treatment, direction) %>%
  do(tidy_lm(., between~within)) %>%
  ungroup() %>%
  mutate_if(is.numeric, format, digits=0, nsmall=3) %>%
  set_names(c("Treatment", "Direction", "Formula", "p-value", "Adjusted $R^2$")) %>%
  kable(row.names=FALSE, escape=FALSE) %>%
  collapse_rows(1)

degree_dist_by_treatment_by_kingdom %>%
  mutate(ratio = between/within) %>%
  filter(is.finite(ratio)) %>%
  do(tidy_aov(., ratio~treatment*direction)) %>%
  ungroup() %>%
  mutate_all(replace_na, "") %>%
  set_names(c("Term", "df", "sum squared error", "mean squared error", "p-value")) %>%
  kable(row.names=FALSE, escape=FALSE)
