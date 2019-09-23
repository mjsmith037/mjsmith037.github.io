library(tidyverse)
library(magrittr)
library(tidygraph)
library(ggraph)
library(gganimate)

set.seed(100)
gg <- play_erdos_renyi(7, p=0.33) %>%
  activate(edges) %>% filter(str_c(from, to) %>% is_in(str_c(to, from)) %>% not()) %>%
  activate(nodes) %>%
  mutate(name = c("one", "two", "three", "four", "five", "six", "seven"))
layout_matrix <- gg %>% graphlayouts::layout_with_stress() %>% set_colnames(c("x", "y"))

ggraph(gg, layout=layout_matrix) +
  geom_edge_link(end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_alpha_identity() + scale_edge_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes.png", width=5, height=5, bg="transparent")

ggraph(gg, layout=layout_matrix) +
  geom_edge_link(end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(size=10, aes(label=1:7), family="Alegreya") +
  scale_alpha_identity() + scale_edge_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_numbers.png", width=5, height=5, bg="transparent")

ggraph(gg %>% activate(nodes) %>% mutate(alph = ifelse(name %in% c("six", "three"), 1, 0)),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_alpha_identity() + scale_edge_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_zero.png", width=5, height=5, bg="transparent")

ggraph(gg %>% activate(edges) %>% mutate(alph = ifelse(from == 6 & to == 3, 1, 0)),
       layout=layout_matrix) +
  geom_edge_link(aes(alpha=alph), end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  scale_edge_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_edges_zero.png", width=5, height=5, bg="transparent")


ggraph(gg %>% activate(nodes) %>% mutate(alph = ifelse(name %in% c("six", "three"), 0, 1)),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_alpha_identity() + scale_edge_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_one.png", width=5, height=5, bg="transparent")

ggraph(gg %>% activate(edges) %>% mutate(alph = ifelse(from == 6 & to == 3, 0, 1)),
       layout=layout_matrix) +
  geom_edge_link(aes(alpha=alph), end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  scale_edge_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_edges_one.png", width=5, height=5, bg="transparent")


adj_mat <- gg %>% as.igraph() %>% igraph::as_adjacency_matrix(sparse=FALSE)
diag(adj_mat) <- -rowSums(adj_mat)
{cat("\\begin{bmatrix}\n")
  tmp <- apply(adj_mat, 1, . %>% str_c(" delta", collapse=" & ")) %>%
    str_replace_all(c("1 delta" = "delta", "0 delta" = "0")) %>%
    str_replace_all("delta", "\\\\delta") %>%
    lapply(. %>% str_c("  ", ., "\\\\\n") %>% cat())
  cat("\\end{bmatrix}\n")}




################################################################################################################################

ggraph(gg %>%
         activate(nodes) %>%
         mutate(col=str_replace_all(name, c("one"="#000000", "two"="#f74700", "three"="#000000", "four"="#000000", "five"="#000000", "six"="#000000", "seven"="#000000")),
                alph=str_replace_all(name, c("one"="0", "two"="1", "three"="0", "four"="0", "five"="0", "six"="0", "seven"="0"))),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(colour=col, alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_colour_identity() + scale_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_two.png", width=5, height=5, bg="transparent")

ggraph(gg %>%
         activate(nodes) %>%
         mutate(col=str_replace_all(name, c("one"="#000000", "two"="#f74700", "three"="#000000", "four"="#000000", "five"="#000000", "six"="#f74700", "seven"="#000000")),
                alph=str_replace_all(name, c("one"="0", "two"="1", "three"="0", "four"="0", "five"="0", "six"="1", "seven"="0"))),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(colour=col, alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_colour_identity() + scale_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_three.png", width=5, height=5, bg="transparent")

ggraph(gg %>%
         activate(nodes) %>%
         mutate(col=str_replace_all(name, c("one"="#000000", "two"="#f74700", "three"="#000000", "four"="#000000", "five"="#000000", "six"="#f74700", "seven"="#f74700")),
                alph=str_replace_all(name, c("one"="0", "two"="1", "three"="0", "four"="0", "five"="0", "six"="1", "seven"="1"))),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(colour=col, alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_colour_identity() + scale_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_four.png", width=5, height=5, bg="transparent")

ggraph(gg %>%
         activate(nodes) %>%
         mutate(col=str_replace_all(name, c("one"="#000000", "two"="#f74700", "three"="#f74700", "four"="#000000", "five"="#000000", "six"="#f74700", "seven"="#f74700")),
                alph=str_replace_all(name, c("one"="0", "two"="1", "three"="1", "four"="0", "five"="0", "six"="1", "seven"="1"))),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(colour=col, alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_colour_identity() + scale_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_five.png", width=5, height=5, bg="transparent")

ggraph(gg %>%
         activate(nodes) %>%
         mutate(col=str_replace_all(name, c("one"="#f74700", "two"="#f74700", "three"="#f74700", "four"="#000000", "five"="#000000", "six"="#f74700", "seven"="#f74700")),
                alph=str_replace_all(name, c("one"="1", "two"="1", "three"="1", "four"="0", "five"="0", "six"="1", "seven"="1"))),
       layout=layout_matrix) +
  geom_edge_link(alpha=0, end_cap=circle(15, "pt"), start_cap=circle(15, "pt"),
                 edge_width=0.33, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
  geom_node_text(aes(colour=col, alpha=alph), size=5, label="farm", family="Font Awesome 5 Pro Solid") +
  scale_colour_identity() + scale_alpha_identity() +
  theme(legend.position="none",
        panel.border=element_blank(),
        panel.background=element_blank(),
        plot.background=element_blank())
ggsave(filename="sample_network_nodes_six.png", width=5, height=5, bg="transparent")
