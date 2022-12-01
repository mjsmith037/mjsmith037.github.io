library(tidygraph)
library(ggraph)
library(magrittr)
library(tidyverse)

# my_cols <- c("#e3944f", "#297373", "#9a281d")
# 
# set.seed(2)
# 
# graph <- play_smallworld(2, 20, 1, 0.1) %N>%
#   filter(!node_is_isolated())
# 
# layout <- igraph::layout_nicely(graph) 
# 
# # summary(layout)
# 
# ggraph(graph, layout) +
#   geom_edge_link(alpha=0.1) +
#   geom_node_point(size=4) +
#   scale_edge_alpha_identity() +
#   coord_equal(xlim=c(-10, 10), ylim=c(-5.7, 5.7), clip="off") +
#   theme(panel.background=element_blank())
# ggsave("../figures/complex_network_background.svg", width=16, height=9)
# 
# graph %>%
#   mutate(color=rep("#000000", n()) %>% inset(which.min(abs(layout[,1]) + abs(layout[,2])), "#9a281d")) %>% 
#   ggraph(layout) +
#   geom_edge_link(alpha=0.1) +
#   geom_node_point(aes(colour=color), size=4) +
#   coord_equal(xlim=c(-10, 10), ylim=c(-5.7, 5.7), clip="off") +
#   scale_colour_identity() +
#   scale_edge_alpha_identity() +
#   theme(panel.background=element_blank(),
#         legend.position="none")
# ggsave("../figures/complex_network_background_one.svg", width=16, height=9)
# 
# graph %>%
#   mutate(color=rep("#000000", n()) %>%
#            inset(which.min(abs(layout[,1]) + abs(layout[,2])), "#9a281d")) %E>% 
#   mutate(alpha=ifelse(edge_is_incident(which(.N()$color != "#000000")), 1, 0.1),
#          colour=ifelse(edge_is_incident(which(.N()$color != "#000000")), "#9a281d", "#000000")) %>% 
#   ggraph(layout) +
#   geom_edge_link(aes(alpha=alpha, colour=colour)) +
#   geom_node_point(aes(colour=color), size=4) +
#   coord_equal(xlim=c(-10, 10), ylim=c(-5.7, 5.7), clip="off") +
#   scale_colour_identity() + scale_edge_colour_identity() +
#   scale_edge_alpha_identity() +
#   theme(panel.background=element_blank(),
#         legend.position="none")
# ggsave("../figures/complex_network_background_two.svg", width=16, height=9)
# 
# graph %>%
#   mutate(color=rep("#000000", n()) %>%
#            inset(which.min(abs(layout[,1]) + abs(layout[,2])), "#9a281d")) %E>% 
#   mutate(alpha=ifelse(edge_is_incident(which(.N()$color != "#000000")), 1, 0.1),
#          colour=ifelse(edge_is_incident(which(.N()$color != "#000000")), "#9a281d", "#000000")) %N>%
#   mutate(color=color %>%
#            inset(node_is_adjacent(which(color != "#000000")), "#9a281d")) %>%
#   ggraph(layout) +
#   geom_edge_link(aes(alpha=alpha, colour=colour)) +
#   geom_node_point(aes(colour=color), size=4) +
#   coord_equal(xlim=c(-10, 10), ylim=c(-5.7, 5.7), clip="off") +
#   scale_colour_identity() + scale_edge_colour_identity() +
#   scale_edge_alpha_identity() +
#   theme(panel.background=element_blank(),
#         legend.position="none")
# ggsave("../figures/complex_network_background_three.svg", width=16, height=9)
# 
# graph %>%
#   mutate(color=rep("#000000", n()) %>%
#            inset(which.min(abs(layout[,1]) + abs(layout[,2])), "#9a281d"),
#          color=color %>%
#            inset(node_is_adjacent(which(color != "#000000")), "#9a281d")) %E>% 
#   mutate(alpha=ifelse(edge_is_incident(which(.N()$color != "#000000")), 1, 0.1),
#          colour=ifelse(edge_is_incident(which(.N()$color != "#000000")), "#9a281d", "#000000")) %>% 
#   ggraph(layout) +
#   geom_edge_link(aes(alpha=alpha, colour=colour)) +
#   geom_node_point(aes(colour=color), size=4) +
#   coord_equal(xlim=c(-10, 10), ylim=c(-5.7, 5.7), clip="off") +
#   scale_colour_identity() + scale_edge_colour_identity() +
#   scale_edge_alpha_identity() +
#   theme(panel.background=element_blank(),
#         legend.position="none")
# ggsave("../figures/complex_network_background_four.svg", width=16, height=9)

library(migraph)
set.seed(5)
graph <- play_smallworld(2, 20, 1, 0.1) %N>%
  filter(!node_is_isolated())
layout <- igraph::layout_nicely(graph) 
ggraph(graph %>% mutate(id=1:n()), layout) +
  geom_edge_link(edge_width=0.5, alpha=0.33) +
  # geom_node_label(aes(label=id), size=3) +
  geom_node_point(size=3) +
  coord_cartesian(xlim=range(layout[,1]), ylim=range(layout[,2]), expand=FALSE, clip="off") +
  theme(panel.background=element_blank(),
        plot.background=element_blank())
ggsave("../figures/network_changes_one_node.svg", width=12, height=8)
focal_node_id <- 50
graph %N>%
  mutate(changed = "Same",
         changed = changed %>% inset(focal_node_id, "Identity")) %>% 
  {ggraph(filter(., changed != "Same"), layout[as_tibble(.) %>% pull(changed) %>% equals("Same") %>% not() %>% which(),,drop=FALSE]) +
      geom_node_point(aes(colour=changed), size=4) +
      coord_cartesian(xlim=range(layout[,1]), ylim=range(layout[,2]), expand=FALSE, clip="off") +
      scale_colour_manual(values=c("Changed"=my_cols[3], "Identity"=my_cols[1], "Same"="black")) +
      theme(panel.background=element_blank(),
            plot.background=element_blank(),
            legend.position="none")}
ggsave("../figures/network_changes_one_node_focus.svg", width=12, height=8)
old <- graph %N>%
  mutate(!!!(node_triad_census(graph) %>% as_tibble())) %>% 
  mutate(id = 1:n(),
         degree = centrality_degree(),
         closeness = centrality_closeness(),
         betweenness = centrality_betweenness()) %>%
  as_tibble()
new <- graph %N>%
  mutate(id = 1:n()) %>% 
  slice(-focal_node_id)
new %<>% 
  mutate(!!!(node_triad_census(new) %>% as_tibble())) %>%
  mutate(degree = centrality_degree(),
         closeness = centrality_closeness(),
         betweenness = centrality_betweenness()) %>% 
  as_tibble()
graph %N>%
  mutate(changed = "Same",
         changed = changed %>%
           inset(full_join(old, new, by="id") %>% 
                   filter(`degree.x` != `degree.y`) %>% 
                   pull(id), "Changed") %>%
           inset(focal_node_id, "Identity")) %>% 
  {ggraph(filter(., changed != "Same"), layout[as_tibble(.) %>% pull(changed) %>% equals("Same") %>% not() %>% which(),]) +
      geom_node_point(aes(colour=changed), size=4) +
      coord_cartesian(xlim=range(layout[,1]), ylim=range(layout[,2]), expand=FALSE, clip="off") +
      scale_colour_manual(values=c("Changed"=my_cols[3], "Identity"=my_cols[1], "Same"="black")) +
      annotate(geom="text", x=-14, y=15, size=14, hjust=0, family="Alegreya Sans", label="Degree") +
      theme(panel.background=element_blank(),
            plot.background=element_blank(),
            legend.position="none")}
ggsave("../figures/network_changes_one_node_degree.svg", width=12, height=8)
graph %N>%
  mutate(changed = "Same",
         changed = changed %>%
           inset(full_join(old, new, by="id") %>% 
                   filter(`102.x` != `102.y`) %>% 
                   pull(id), "Changed") %>%
           inset(focal_node_id, "Identity")) %>% 
  {ggraph(filter(., changed != "Same"), layout[as_tibble(.) %>% pull(changed) %>% equals("Same") %>% not() %>% which(),]) +
      geom_node_point(aes(colour=changed), size=4) +
      coord_cartesian(xlim=range(layout[,1]), ylim=range(layout[,2]), expand=FALSE, clip="off") +
      scale_colour_manual(values=c("Changed"=my_cols[3], "Identity"=my_cols[1], "Same"="black")) +
      annotate(geom="text", x=-14, y=15, size=14, hjust=0, family="Alegreya Sans", label="A <=> B    C") +
      theme(panel.background=element_blank(),
            plot.background=element_blank(),
            legend.position="none")}
ggsave("../figures/network_changes_one_node_T102.svg", width=12, height=8)
graph %N>%
  mutate(changed = "Same",
         changed = changed %>%
           inset(full_join(old, new, by="id") %>% 
                   filter(`betweenness.x` != `betweenness.y`) %>% 
                   pull(id), "Changed") %>%
           inset(focal_node_id, "Identity")) %>% 
  {ggraph(filter(., changed != "Same"), layout[as_tibble(.) %>% pull(changed) %>% equals("Same") %>% not() %>% which(),]) +
      geom_node_point(aes(colour=changed), size=4) +
      coord_cartesian(xlim=range(layout[,1]), ylim=range(layout[,2]), expand=FALSE, clip="off") +
      scale_colour_manual(values=c("Changed"=my_cols[3], "Identity"=my_cols[1], "Same"="black")) +
      annotate(geom="text", x=-14, y=15, size=14, hjust=0, family="Alegreya Sans", label="Betweenness") +
      theme(panel.background=element_blank(),
            plot.background=element_blank(),
            legend.position="none")}
ggsave("../figures/network_changes_one_node_betweenness.svg", width=12, height=8)



