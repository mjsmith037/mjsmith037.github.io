library(tidygraph)
library(tidyverse)

showtext_auto()
font_add(family="Alegreya Sans",
         regular="~/.local/share/fonts/AlegreyaSans-Regular.otf")

set.seed(1)

theme_set(theme_bw())

#### example random graphs #####################################################

my_cols <- c('"Random"'="#297373", '"Scale-free"'="#e3944f", '"Small-world"'="#9a281d")
# my_cols <- c("Erdős-Rényi"="#297373", "Barabási-Albert"="#e3944f", "Watts-Strogatz"="#9a281d")

set.seed(1)
n_populations <- 100
example_ba <- play_barabasi_albert(n_populations, 2, 2, directed=FALSE) %>% mutate(net_type = "ba") %>%
  activate(edges) %>% mutate(net_type = "ba")
example_er <- play_erdos_renyi(n=n_populations, m=200, directed=FALSE) %>% mutate(net_type = "er") %>%
  activate(edges) %>% mutate(net_type = "er")
example_ws <- play_smallworld(n_dim=2, dim_size=10, order=1, p_rewire=0.3) %>% mutate(net_type = "ws") %>%
  activate(edges) %>% mutate(net_type = "ws")

layout_matrix <- rbind(example_ba %>% as.igraph() %>% igraph::layout_nicely(),
                       example_er %>% as.igraph() %>% igraph::layout_nicely(),
                       example_ws %>% as.igraph() %>% igraph::layout_nicely()) %>%
  set_colnames(letters[24:25]) %>%
  as_tibble()

bind_graphs(example_ba, example_er, example_ws) %>%
  mutate(net_type = factor(net_type, levels=c("ba", "er", "ws"),
                           labels=c('"Scale-free"', '"Random"', '"Small-world"'))) %N>%
  # labels=c("Barabási-Albert", "Erdős-Rényi", "Watts-Strogatz"))) %N>%
  mutate(net_type = factor(net_type, levels=c("ba", "er", "ws"),
                           labels=c('"Scale-free"', '"Random"', '"Small-world"'))) %>%
  # labels=c("Barabási-Albert", "Erdős-Rényi", "Watts-Strogatz"))) %>%
  {ggraph(., layout=layout_matrix) +
      geom_edge_link(aes(colour=net_type),
                     start_cap=circle(5, "pt"),
                     end_cap=circle(5, "pt"),
                     edge_width=0.5,
                     edge_alpha=0.5) +
      geom_node_point(aes(colour=net_type), size=2) +
      facet_nodes(~net_type, scales="free", nrow=1) +
      scale_colour_manual(values=my_cols) +
      scale_edge_colour_manual(values=my_cols) +
      theme_minimal() +
      theme(text=element_text(size=20, family="Alegreya Sans"),
            axis.text=element_blank(),
            axis.title=element_blank(),
            plot.background=element_blank(),
            panel.grid=element_blank(),
            panel.spacing.x=unit(0.5, "in"),
            panel.background=element_blank(),
            legend.position="none")}
ggsave("../figures/summary_networks.svg", width=10, height=4)