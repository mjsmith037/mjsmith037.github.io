library(tidygraph)
library(ggraph)
library(tidyverse)
library(kableExtra)

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

degree_in_by_out <- bind_rows(raw_network_data %>%
                                group_by(row_isolate, leaf, treatment, interaction_type) %>%
                                summarise(degree = sum(link)) %>%
                                mutate(direction = "outdegree") %>%
                                rename(isolate = row_isolate) %>%
                                mutate(type = str_split_fixed(interaction_type, " -> ", 2)[,1],
                                       partner = str_split_fixed(interaction_type, " -> ", 2)[,2]),
                              raw_network_data %>%
                                group_by(col_isolate, leaf, treatment, interaction_type) %>%
                                summarise(degree = sum(link)) %>%
                                mutate(direction = "indegree") %>%
                                rename(isolate = col_isolate) %>%
                                mutate(type = str_split_fixed(interaction_type, " -> ", 2)[,2],
                                       partner = str_split_fixed(interaction_type, " -> ", 2)[,1])) %>%
  ungroup() %>%
  spread(direction, degree, fill=0) %>%
  bind_rows(group_by(., isolate, leaf, treatment, type, partner) %>%
              summarise(indegree=sum(indegree), outdegree=sum(outdegree)),
            group_by(., isolate, leaf, treatment, type) %>%
              summarise(indegree=sum(indegree), outdegree=sum(outdegree))) %>%
  ungroup() %>%
  mutate(col = case_when(is.na(partner)  ~ "Any",
                         type == partner ~ type,
                         type != partner ~ "Mixed"))

ggplot(degree_in_by_out) +
  aes(y=indegree, x=outdegree, colour=col) +
  geom_jitter(width=0.33, height=0.33) +
  geom_smooth(method="lm", se=FALSE) +
  facet_grid(treatment~type) +
  scale_colour_manual(values=my_cols) +
  theme(legend.position="none")

ggsave("../Figures/in-by-out-degree-distributions.svg", width=7, height=7)

degree_in_by_out %>%
  mutate(interaction_type = ifelse(is.na(interaction_type), str_c(type, " -> Any"), interaction_type)) %>%
  group_by(treatment, interaction_type) %>%
  do(tidy_lm(., indegree~outdegree)) %>%
  # mutate(`p-value` = case_when(`p-value` == "$< 0.001$" ~ )) %>%
  ungroup() %>%
  mutate(interaction_type = str_replace_all(interaction_type, "->", "$\\\\rightarrow$")) %>%
  mutate_if(is.numeric, function(x) str_c("$", format(x, digits=0, nsmall=3), "$")) %>%
  set_names(c("Treatment", "Interaction Type", "Term", "Estimate", "p-value", "Adjusted $R^2$")) %>%
  kable(format="latex", row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lllrrr", "")), escape=FALSE) %>%
  collapse_rows(1)

degree_in_by_out %>%
  mutate(interaction_type = ifelse(is.na(interaction_type), str_c(type, " -> Any"), interaction_type),
         ratio = indegree/outdegree) %>%
  filter(is.finite(ratio)) %>%
  do(tidy_aov(., ratio~treatment*interaction_type)) %>%
  ungroup() %>%
  # set_names(c("Term", "p-value", "Adjusted $R^2$")) %>%
  xtable() %>%
  print(booktabs=TRUE, format="latex", include.rownames=FALSE, sanitize.text.function=function(x) return(x))
