library(tidygraph)
library(ggraph)
library(broom)
library(tidyverse)
library(kableExtra)

tidy_p_value <- function(pvals) {
  ifelse(pvals < 0.001, "< 0.001", format(pvals, digits=0, nsmall=3))
}

tidy_lm <- function(dat, formula) {
  lm(formula, data=dat) %>%
  {bind_cols(tidy(.) %>% mutate(estimate = str_c(str_c(formula)[2], " ~ ", round(estimate[1], 3),
                                                 " &middot; ", str_c(formula)[3], ifelse(sign(estimate[2]) == 1, " + ", " - "), round(abs(estimate[2]), 3))) %>%
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
             "Any"              = "black")

raw_network_data <- read_csv("~/Research/CompetetiveTradeoff/Data/complete_competetive_index.csv",
                             col_types="ccccccccdddddd") %>%
  filter(leaf_x == leaf_y) %>%
  filter(leaf_x != "C5") %>%
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

degree_in_by_out <- bind_rows(raw_network_data %>%
                                group_by(row_isolate, leaf, treatment, interaction_type) %>%
                                summarise(degree = sum(overlap)) %>%
                                mutate(direction = "outdegree") %>%
                                rename(isolate = row_isolate) %>%
                                mutate(type = str_split_fixed(interaction_type, " -> ", 2)[,1],
                                       partner = str_split_fixed(interaction_type, " -> ", 2)[,2]),
                              raw_network_data %>%
                                group_by(col_isolate, leaf, treatment, interaction_type) %>%
                                summarise(degree = sum(overlap)) %>%
                                mutate(direction = "indegree") %>%
                                rename(isolate = col_isolate) %>%
                                mutate(type = str_split_fixed(interaction_type, " -> ", 2)[,2],
                                       partner = str_split_fixed(interaction_type, " -> ", 2)[,1])) %>%
  ungroup() %>%
  spread(direction, degree, fill=0) %>%
  {bind_rows(group_by(., isolate, leaf, treatment, type, partner) %>%
               summarise(indegree=sum(indegree), outdegree=sum(outdegree)),
             group_by(., isolate, leaf, treatment, type) %>%
               summarise(indegree=sum(indegree), outdegree=sum(outdegree)))} %>%
  ungroup() %>%
  mutate(col = case_when(is.na(partner)  ~ "Any",
                         type == partner ~ type,
                         type != partner ~ "Mixed"))

ggplot(degree_in_by_out %>% filter(col == "Any")) +
  aes(y=indegree, x=outdegree, colour=col) +
  geom_point() +
  # geom_jitter(width=0.33, heigh=0.33) +
  geom_line(stat="smooth", method="lm", size=1) +
  facet_grid(treatment~type) +
  scale_colour_manual(values=my_cols) +
  coord_cartesian(xlim=c(0, 20), ylim=c(0, 20)) +
  theme(legend.position="none")
ggsave("../Figures/in-by-out-degree-distributions.svg", width=7, height=7)

ggplot(degree_in_by_out) +
  aes(y=indegree, x=outdegree, colour=col, alpha=col) +
  geom_point() +
  # geom_jitter(width=0.33, heigh=0.33) +
  geom_line(stat="smooth", method="lm", size=1) +
  facet_grid(treatment~type) +
  scale_colour_manual(values=my_cols) +
  scale_alpha_manual(values=c("Bacteria" = 1, "Fungi" = 1, "Mixed" = 1, "Any" = 0.25)) +
  coord_cartesian(xlim=c(0, 20), ylim=c(0, 20)) +
  theme(legend.position="none")
ggsave("../Figures/in-by-out-degree-distributions_subdivided.svg", width=7, height=7)

degree_in_by_out %>%
  mutate(interaction_type = ifelse(is.na(partner), str_c(type, " -> Any"), str_c(type, " -> ", partner))) %>%
  group_by(treatment, interaction_type) %>%
  do(tidy_lm(., indegree~outdegree)) %>%
  # mutate(`p-value` = case_when(`p-value` == "$< 0.001$" ~ )) %>%
  ungroup() %>%
  mutate_if(is.numeric, format, digits=0, nsmall=3) %>%
  mutate(interaction_type = str_replace_all(interaction_type, "->", " &rarr; ")) %>%
  set_names(c("Treatment", "Interaction Type", "Formula", "p-value", "Adjusted $R^2$")) %>%
  kable(row.names=FALSE, escape=FALSE) %>%
  collapse_rows(1)

degree_in_by_out %>%
  filter(!is.na(partner)) %>%
  mutate(interaction_type = str_c(type, " -> ", partner),
         ratio = indegree/outdegree) %>%
  filter(is.finite(ratio)) %>%
  do(tidy_aov(., ratio~treatment*interaction_type)) %>%
  ungroup() %>%
  mutate_all(replace_na, "") %>%
  set_names(c("Term", "df", "sum squared error", "mean squared error", "p-value")) %>%
  kable(row.names=FALSE, escape=FALSE)
