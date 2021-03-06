library(igraph)
library(tidygraph)
library(ggraph)
library(magrittr)
library(tidyverse)

OVERLAP_LINK_CUTOFF <- 0.75
set.seed(0)

theme_set(theme_bw())
my_cols <- c("Bacteria"="#006989",
             "bacto"="#006989",
             "bacto->bacto"="#006989",
             "bacto->fungi"="#6a526b",
             "fungi->bacto"="#6a526b",
             "fungi->fungi"="#9b1d20",
             "Fungi"="#9b1d20",
             "fungi"="#9b1d20")

raw_network_data <- read_csv("~/Research/CompetetiveTradeoff/Data/complete_competetive_index.csv",
                             col_types="ccccccccdddddd") %>%
  filter(leaf_x == leaf_y) %>%
  mutate(isolate_x = str_c(isolate_x, type_x, sep="."),
         isolate_y = str_c(isolate_y, type_y, sep=".")) %>%
  do(bind_rows(transmute(., treatment = treatment_x, leaf        = leaf_x,
                         row_type     = type_x,      row_isolate = isolate_x,
                         col_type     = type_y,      col_isolate = isolate_y,
                         overlap      = x_on_y_pw),
               transmute(., treatment = treatment_x, leaf        = leaf_x,
                         row_type     = type_y,      row_isolate = isolate_y,
                         col_type     = type_x,      col_isolate = isolate_x,
                         overlap      = y_on_x_pw))) %>%
  # filter(overlap > OVERLAP_LINK_CUTOFF) %>%
  mutate(treatment = str_replace_all(treatment, c("C"="Control", "N"="NPK Supplemented")))

network_data <- tbl_graph(
  nodes = raw_network_data %>%
    do(bind_rows(transmute(., isolate=row_isolate, type=row_type, leaf, treatment),
                 transmute(., isolate=col_isolate, type=col_type, leaf, treatment))) %>%
    distinct(),
  edges = raw_network_data %>%
    select(row_isolate, col_isolate, overlap, leaf, treatment) %>%
    mutate(type=str_c(str_extract(row_isolate, "\\w+$"), "->", str_extract(col_isolate, "\\w+$"))))

normalize <- . %>% subtract(min(.)) %>% divide_by(max(.))

## all together layout
layout_matrix <- network_data %>%
  layout_with_fr(weights=.$overlap) %>%
  as_tibble(.name_repair="minimal") %>%
  set_names(c("x","y")) %>%
  mutate(leaf = network_data %>% activate(nodes) %>% as_tibble() %>% use_series(leaf)) %>%
  group_by(leaf) %>%
  mutate(x = normalize(x),
         y = normalize(y)) %>%
  ungroup() %>%
  select(-leaf)

# ggraph(network_data %>% filter(leaf == "C1") %>% activate("edges") %>% filter(type == "fungi->bacto" | type == "bacto->fungi"), layout="manual",
#        node.positions=layout_matrix[which(network_data %>% activate(nodes) %>% as_tibble() %>%
#                                             use_series(leaf) %>% equals("C1")),]) +
#   geom_edge_link(aes(edge_colour=type), start_cap=circle(15, 'pt'), end_cap=circle(15, 'pt'),
#                  edge_width=0.66, arrow=arrow(angle=30, length=unit(5, "pt"), type="closed")) +
#   geom_node_text(aes(colour=type), label="disease", family="Font Awesome 5 Pro Solid", size=8) +
#   scale_colour_manual(values=my_cols) +
#   scale_edge_colour_manual(values=my_cols) +
#   theme(axis.text=element_blank(),
#         axis.title=element_blank(),
#         axis.ticks=element_blank(),
#         panel.grid=element_blank(),
#         panel.border=element_blank(),
#         legend.position="none")

ggraph(network_data %>% filter(treatment == "Control"), layout="manual",
       node.positions=layout_matrix[which(network_data %>% activate(nodes) %>% as_tibble() %>%
                                            use_series(treatment) %>% equals("Control")),]) +
  geom_edge_link(aes(edge_colour=type), start_cap=circle(3, 'pt'), end_cap=circle(3, 'pt'),
                 edge_alpha=0.5,
                 edge_width=0.25, arrow=arrow(angle=30, length=unit(2, "pt"), type="closed")) +
  geom_node_text(aes(colour=type), label="disease", family="Font Awesome 5 Pro Solid", size=2) +
  facet_nodes(~str_extract(leaf, "\\d"), nrow=3, scales="free") +
  scale_colour_manual(values=my_cols) +
  scale_edge_colour_manual(values=my_cols) +
  scale_x_continuous(expand=expand_scale(0.12, 0)) +
  scale_y_continuous(expand=expand_scale(0.12, 0)) +
  theme(axis.text=element_blank(), axis.title=element_blank(), axis.ticks=element_blank(),
        panel.grid=element_blank(), panel.border=element_blank(), panel.background=element_blank(),
        plot.background=element_blank(),
        strip.background=element_blank(), strip.text=element_blank(),
        legend.position="none")
ggsave("../Figures/control_networks.png", width=3, height=4.5)
# ggsave("../Figures/control_networks_weighted.png", width=3, height=4.5)

ggraph(network_data %>% filter(treatment == "Control") %>% mutate(alph=ifelse(leaf == "C5", 0.5, 1)) %>% activate("edges") %>% mutate(alph=ifelse(leaf == "C5", 0.5, 1)), layout="manual",
#        node.positions=layout_matrix[which(network_data %>% activate(nodes) %>% as_tibble() %>%
#                                             use_series(treatment) %>% equals("Control")),]) +
#   geom_edge_link(aes(edge_colour=type, edge_alpha=0.5 * alph), start_cap=circle(3, 'pt'), end_cap=circle(3, 'pt'),
#                  edge_width=0.25, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
#   geom_node_text(aes(colour=type, alpha=alph), label="disease", family="Font Awesome 5 Pro Solid", size=2) +
#   facet_nodes(~str_extract(leaf, "\\d"), nrow=3, scales="free") +
#   scale_colour_manual(values=my_cols) +
#   scale_edge_colour_manual(values=my_cols) +
#   scale_alpha_identity() + scale_edge_alpha_identity() +
#   scale_x_continuous(expand=expand_scale(0.12, 0)) +
#   scale_y_continuous(expand=expand_scale(0.12, 0)) +
#   theme(axis.text=element_blank(), axis.title=element_blank(), axis.ticks=element_blank(),
#         panel.grid=element_blank(), panel.border=element_blank(), panel.background=element_blank(),
#         plot.background=element_blank(),
#         strip.background=element_blank(), strip.text=element_blank(),
#         legend.position="none")
# ggsave("../Figures/control_networks_weighted_fade_C5.png", width=3, height=4.5)

ggraph(network_data %>% filter(treatment == "NPK Supplemented"), layout="manual",
       node.positions=layout_matrix[which(network_data %>% activate(nodes) %>% as_tibble() %>%
                                            use_series(treatment) %>% equals("NPK Supplemented")),]) +
  geom_edge_link(aes(edge_colour=type), start_cap=circle(3, 'pt'), end_cap=circle(3, 'pt'),
                 edge_alpha=0.5,
                 edge_width=0.25, arrow=arrow(angle=30, length=unit(2, "pt"), type="closed")) +
  geom_node_text(aes(colour=type), label="disease", family="Font Awesome 5 Pro Solid", size=2) +
  facet_nodes(~str_extract(leaf, "\\d"), nrow=3, scales="free") +
  scale_colour_manual(values=my_cols) +
  scale_edge_colour_manual(values=my_cols) +
  scale_x_continuous(expand=expand_scale(0.12, 0)) +
  scale_y_continuous(expand=expand_scale(0.12, 0)) +
  theme(axis.text=element_blank(), axis.title=element_blank(), axis.ticks=element_blank(),
        panel.grid=element_blank(), panel.border=element_blank(), panel.background=element_blank(),
        plot.background=element_blank(),
        strip.background=element_blank(), strip.text=element_blank(),
        legend.position="none")
ggsave("../Figures/npk_networks.png", width=3, height=4.5)
# ggsave("../Figures/npk_networks_weighted.png", width=3, height=4.5)
