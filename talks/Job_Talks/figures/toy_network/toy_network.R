library(tidyverse)
library(magrittr)
library(tidygraph)
library(ggraph)
library(gganimate)
library(magrittr)
library(tidygraph)
library(docstring)
library(tidyverse)
library(assertthat)

source("../../../Job_Talks/code/simulate_disease_on_network_.R")

set.seed(0)
gg <- play_smallworld(2, 15, 1, 0.15) %>%
  activate(edges) %>% filter(str_c(from, to) %>% is_in(str_c(to, from)) %>% not())
layout_matrix <- gg %>% graphlayouts::layout_with_stress() %>% set_colnames(c("x", "y"))

#### network structure examples ################################################
# ggraph(gg, layout=layout_matrix) +
#   geom_edge_link(end_cap=circle(5, "pt"), start_cap=circle(5, "pt"), edge_width=0.33) +
#   #, arrow=arrow(angle=30, length=unit(3, "pt"), type="closed")) +
#   geom_node_point(size=3) +
#   theme(legend.position="none",
#         panel.border=element_blank(),
#         panel.background=element_blank(),
#         plot.background=element_blank())
# ggsave(filename="net_struct_base.svg", width=8, height=5, bg="transparent")

#### epidemic outcome examples #################################################

set.seed(0)
maxtime <- 100
dis_sims <- lapply(1:10, function(rr) {
  run_disease_network_simulation(maxtime, gg, "SIS", beta=0.2, gamma=0.1)
}) %>% bind_rows(.id="rep")
p <- ggplot(dis_sims) +
  aes(x=time, y=n, colour=status) +
  geom_line(aes(group=str_c(rep, status)), alpha=0.33) +
  geom_line(size=1.5, data=dis_sims %>%
              group_by(rep) %>%
              filter(max(n[status == "I"]) >= 5) %>%
              group_by(time, status) %>% summarise(n = mean(n))) +
  ylab("Number of nodes") +
  scale_x_continuous(name="Time", expand=expansion(mult=c(.05, 0))) +
  scale_colour_manual(name=NULL, values=c("#297373", "#9a281d")) +
  theme_bw() +
  theme(text=element_text(size=14, family="Alegreya Sans"),
        legend.position="bottom",
        plot.margin=margin(5.5, 10, 5.5, 5.5))
ggsave("../../../Job_Talks/figures/DNS_Sim_Lines.svg", width=6, height=4)

tmp <- dis_sims %>%
  group_by(rep) %>%
  filter(max(n[status == "I"]) >= 5) %>%
  ungroup() %>%
  filter(status == "I", time > 0.25 * maxtime) %>%
  pull() %>% quantile(c(0.025, 0.975))
p + 
  annotate(geom="polygon",
           x=c(0.25*maxtime, 0.25*maxtime, Inf, Inf),
           y=c(tmp[1], tmp[2], tmp[2], tmp[1]),
           fill="#000000", alpha=0.1)
ggsave("../../../Job_Talks/figures/DNS_Sim_Lines_cv.svg", width=6, height=4)

p + 
  annotate(geom="polygon",
           x=c(0.25*maxtime, 0.25*maxtime, -Inf, -Inf),
           y=c(-Inf, Inf, Inf, -Inf),
           fill="#FFFFFF", alpha=0.75) +
  geom_hline(aes(yintercept=yint), size=1.5, linetype="dashed",
             data=dis_sims %>%
               group_by(rep) %>%
               filter(max(n[status == "I"]) >= 5) %>%
               group_by(time, status) %>% summarise(n = mean(n)) %>%
               ungroup() %>%
               filter(status == "I", time > 0.25 * maxtime) %>%
               summarise(yint=mean(n)))
ggsave("../../../Job_Talks/figures/DNS_Sim_Lines_mean.svg", width=6, height=4)

p + 
  geom_hline(aes(yintercept=yint), size=1.5, linetype="dashed",
             data=dis_sims %>%
               group_by(rep) %>%
               filter(max(n[status == "I"]) >= 5) %>%
               group_by(time, status) %>% summarise(n = mean(n)) %>%
               ungroup() %>%
               filter(status == "I", time > 0.25 * maxtime) %>%
               summarise(yint=max(n)))
ggsave("../../../Job_Talks/figures/DNS_Sim_Lines_max.svg", width=6, height=4)

p + 
  geom_vline(aes(xintercept=xint), size=1.5, linetype="dashed",
             data=dis_sims %>%
               group_by(rep) %>%
               filter(max(n[status == "I"]) >= 5) %>%
               group_by(time, status) %>% summarise(n = mean(n)) %>%
               ungroup() %>%
               filter(status == "I") %>%
               filter(n == max(n)) %>%
               summarise(xint=min(time)))
ggsave("../../../Job_Talks/figures/DNS_Sim_Lines_ttp.svg", width=6, height=4)

node_data <- dis_sim %N>%
  as_tibble() %>%
  mutate(node_id = 1:n()) %>%
  bind_cols(layout_matrix) %>%
  select(!number_infectious_links)
edge_data <- dis_sim %E>%
  as_tibble() %>%
  select(!pot_inf_link) %>%
  unite(from, to, col="edge_id", sep="_", remove=FALSE) %>%
  pivot_longer(c(from, to), names_to="end", values_to="node_id") %>%
  left_join(select(node_data, c(node_id, x, y)), by=c("node_id")) %>%
  pivot_longer(c(x, y), names_to="coordinate", values_to="value") %>%
  # create xend and yend when at the "to" end, for geom_segment use later
  mutate(col = str_c(coordinate, if_else(end == "to", "end", ""))) %>%
  select(edge_id, col, value) %>%
  arrange(edge_id) %>%
  pivot_wider(names_from=col, values_from=value)
node_data %<>%
  pivot_longer(!c(node_id, x, y), names_to="time", values_to="status") %>%
  mutate(time = as.integer(time))

anim <- ggplot() +
  geom_segment(data=edge_data, size=1, aes(x=x, xend=xend, y=y, yend=yend, group=edge_id), alpha=0.67) +
  geom_point(data=node_data, aes(x=x, y=y, colour=status, group=node_id), size=3) +
  transition_time(time) +
  scale_colour_manual(values=c("#0b6884", "#bc4b51", "#7b678e")) +
  theme_graph() +
  theme(legend.position="bottom",
        legend.title=element_blank())
anim_save(filename="dis_spread_on_network.gif", animation=anim, end_pause=10,
          width=4, height=4, units="in", res=200)
