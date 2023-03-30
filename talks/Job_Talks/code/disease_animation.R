# setwd("C:/Users/mjsmi/Downloads")

library(magick)
library(gganimate)
library(magrittr)
library(tidygraph, warn.conflicts=FALSE)
library(docstring, warn.conflicts=FALSE)
library(tidyverse)
library(ggraph)
library(parallel)
library(showtext)
showtext_auto()
font_add_google("Alegreya Sans")
font_add_google("Lato")

myprod <- function(x) prod(x[x != 0])

library(netrankr)
assignInNamespace("aggregate_positions", function(tau_x, type = "sum") {
  if (!inherits(tau_x, "Matrix") & !is.matrix(tau_x)) {
    stop("tau_x must be a matrix")
  }
  
  if (type == "sum") {
    return(rowSums(tau_x))
  } else if (type == "prod") {
    return(apply(tau_x, 1, myprod))
  } else if (type == "mean") {
    return(rowMeans(tau_x))
  } else if (type == "max") {
    return(apply(tau_x, 1, max))
  } else if (type == "min") {
    return(apply(tau_x, 1, min))
  } else if (type == "invsum") {
    return(rowSums(tau_x)^-1)
  } else if (type == "self") {
    diag(tau_x)
  } else {
    stop(paste("type =", type, "is not supported. See function details for options."))
  }
},ns="netrankr")

run_disease_network_simulation <- function(timesteps, contact_network, model_structure,
                                           sigma=0, gamma, xi=0, return_net=FALSE) {
  # parse model structure
  model_states <- str_split(model_structure, "")[[1]]
  if (head(model_states, 1) == tail(model_states, 1)) model_states %<>% head(-1)
  
  # initialize with one infected node (chosen at random)
  if (is.matrix(contact_network)) contact_network %<>% as_tbl_graph()
  contact_network %<>%
    activate(nodes) %>%
    mutate("0" = sample(c("I", rep("S", nrow(as_tibble(.)) - 1))) %>%
             factor(levels=model_states))
  
  # progress through the discrete time epidemic simulation
  for (timestep in 1:timesteps) {
    contact_network %<>%
      activate(edges) %>%
      # identify which links have the potential to produce new infectious individuals
      # i.e. which links connect an infectious individual with a susceptible one?
      mutate(pot_inf_link = edge_is_between(.N()[str_c(timestep - 1)] == "S",
                                            .N()[str_c(timestep - 1)] == "I"),
             weight = 1 - (beta * pot_inf_link)) %>%
      activate(nodes) %>%
      # the probability of a state-change this timestep is determined by the model parameters:
      mutate(prob_change_status = case_when(
        !!sym(str_c(timestep - 1)) == "S" ~
          # depends on how many potentially infecting connections each individual has
          1 - centrality_manual(relation="weights", aggregation="prod"), # infection rate
        !!sym(str_c(timestep - 1)) == "E" ~ sigma,      # rate of disease progression
        !!sym(str_c(timestep - 1)) == "I" ~ gamma,      # recovery rate
        !!sym(str_c(timestep - 1)) == "R" ~ xi,         # rate of immunity loss
        # if there is not a rate parameter for this state, then don't change
        TRUE ~ 0),
        # create a new column for this timestep, updating statuses when neccessary
        !!str_c(timestep) :=
          model_states[!!sym(str_c(timestep - 1)) %>%
                         as.integer() %>%
                         add(rbernoulli(n(), prob_change_status)) %>%
                         # in some models, we loop around to the beginning again
                         {ifelse(. > length(model_states), 1, .)}] %>%
          factor(levels=model_states))
  }
  
  # convert the output to a more manageable format
  timeseries <- contact_network %N>%
    # remove unnecessary columns
    select(-prob_change_status) %>%
    # remove the network data
    as_tibble() %>%
    # wide -> long format
    pivot_longer(names_to="time", values_to="status", everything()) %>%
    # clean up types
    mutate(time = as.integer(time),
           status = factor(status, levels=model_states)) %>%
    # summarise number of each class at each timestep
    count(status, time) %>%
    # add 0's for missing classes at each timestep
    complete(status, nesting(time), fill=list(n=0))
  
  if (return_net) return(contact_network)
  
  # return the cleaned output
  return(timeseries)
}


set.seed(0)
net1 <- play_smallworld(2, 15, 1, 0.05) %E>% mutate(beta = 0.2)
layout <- igraph::layout_nicely(net1)
net2 <- net1 %E>% mutate(beta = ifelse(from == 169 | to == 169, 0.05, 0.2))

set.seed(0)
dis_sims <- bind_rows(
  mclapply(1:100, mc.cores=7, function(rr) {
    run_disease_network_simulation(200, net1, "SIS", gamma=0.1) %>%
      mutate(net = "net1",
             rep = rr)
  }) %>% bind_rows(),
  mclapply(1:100, mc.cores=7, function(rr) {
    run_disease_network_simulation(200, net2, "SIS", gamma=0.1) %>%
      mutate(net = "net2",
             rep = rr)
  }) %>% bind_rows()
)

# ggplot(dis_sims) +
#   aes(x=time, y=n, colour=status) +
#   geom_line(aes(group=str_c(rep, status)), alpha=0.1) +
#   geom_line(linewidth=2, data=dis_sims %>%
#               group_by(net, rep) %>%
#               filter(max(n[status=="I"]) >= 5) %>%
#               group_by(net, time, status) %>%
#               summarise(n = mean(n))) +
#   facet_wrap(~net) +
#   scale_colour_manual(values=c("#0b6884", "#bc4b51", "#7b678e")) +
#   theme_bw() +
#   theme(legend.position="none")
# ggsave("../figures/trajectory_difference_quarter_strength_interaction.svg", width=6, height=4)

dis_sims %>%
  filter(time > 0.5*max(time), status == "I") %>%
  group_by(net, rep) %>%
  filter(max(n) >= 5) %>%
  summarise(mean_I = mean(n)) %>%
  mutate(net=recode(net, "net1"="original", "net2"="modified")) %>%
  ggplot() +
  aes(x=mean_I, fill=net, colour=net) +
  geom_density(alpha=0.5) +
  # geom_histogram(alpha=0.5, position="Identity", bins=100) +
  xlab("Mean Prevalence") + ylab("Frequency") +
  scale_fill_manual(values=c("#50b99a", "#ffc847"),
                    aesthetics=c("colour", "fill")) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=20, family="Alegreya Sans"))
ggsave("../figures/mean_prev_difference_half_strength_interaction.svg", width=6, height=4)

dis_sims %>%
  filter(time > 0.5*max(time), status == "I") %>%
  group_by(net, rep) %>%
  filter(max(n) >= 5) %>%
  summarise(mean_I = mean(n), .groups="drop") %>%
  pivot_wider(names_from=net, values_from=mean_I) %>%
  mutate(diff=net1 - net2) %>%
  {ggplot(.) +
      aes(x=diff) +
      geom_histogram(bins=30) +
      geom_vline(aes(xintercept=xint), colour="#bc4b51", linewidth=2, linetype="dashed",
                 data=summarise(., xint=mean(diff, na.rm=TRUE))) +
      xlab("Difference in mean prevalence") + ylab("Frequency") +
      theme_bw() +
      theme(text=element_text(size=20, family="Alegreya Sans"))}
ggsave("../figures/paired_differences_half_strength_interaction.svg", width=6, height=4)

set.seed(1)
sim1 <- run_disease_network_simulation(200, net1, "SIS", gamma=0.1)
set.seed(1)
sim2 <- run_disease_network_simulation(200, net2, "SIS", gamma=0.1)

ggplot(tibble(time=sim1$time[sim1$status=="I"], delta=sim1$n[sim1$status=="I"] - sim2$n[sim2$status=="I"])) +
  aes(x=time, y=delta) +
  geom_line(size=1.5) +
  ylab("Original - Masker") +
  theme_bw() +
  theme(text=element_text(size=20, family="Alegreya Sans"))
ggsave("../figures/delta.svg", width=6, height=4)

set.seed(1)
sim1 <- run_disease_network_simulation(200, net1, "SIS", gamma=0.1, return_net=TRUE)
set.seed(1)
sim2 <- run_disease_network_simulation(200, net2, "SIS", gamma=0.1, return_net=TRUE)

disease_anim <- function(dis_sim, mycols, layout_matrix) {
  node_data <- dis_sim %N>%
    as_tibble() %>%
    mutate(node_id = 1:n()) %>%
    bind_cols(layout_matrix) %>%
    select(!prob_change_status)
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
    scale_colour_manual(values=mycols) +
    theme_graph() +
    theme(text=element_text(size=20, family="Alegreya Sans"),
          legend.position="bottom",
          legend.title=element_blank())
  return(anim)
}

line_anim <- function(dis_sim, mycols) {
  dat <- dis_sim %N>%
    select(-prob_change_status) %>%
    as_tibble() %>%
    pivot_longer(names_to="time", values_to="status", everything()) %>%
    mutate(time = as.integer(time)) %>%
    count(status, time) %>%
    complete(status, nesting(time), fill=list(n=0))
  
  anim <- ggplot(dat) +
    aes(x=time, y=n, colour=status) +
    geom_line(size=1.5) +
    transition_reveal(time) +
    scale_colour_manual(values=mycols) +
    theme_bw() +
    theme(text=element_text(size=20, family="Alegreya Sans"),
          legend.position="none")
  return(anim)
}

layout_matrix <- sim1 %>% igraph::layout_nicely() %>% set_colnames(c("x", "y"))
net_anim1 <- disease_anim(sim1, c("#0b6884", "#bc4b51"), layout_matrix)
net_anim2 <- disease_anim(sim2, c("#7b678e", "#ff5e5b"), layout_matrix)
line_anim1 <- line_anim(sim1, c("#0b6884", "#bc4b51"))
line_anim2 <- line_anim(sim2, c("#7b678e", "#ff5e5b"))

net_gif1 <- animate(net_anim1, width=600, height=400, duration=10, fps=10)
net_gif2 <- animate(net_anim2, width=600, height=400, duration=10, fps=10)
line_gif1 <- animate(line_anim1, width=600, height=400, duration=10, fps=10)
line_gif2 <- animate(line_anim2, width=600, height=400, duration=10, fps=10)

anim_save("net_gif1.gif", net_gif1)
anim_save("net_gif2.gif", net_gif2)
anim_save("line_gif1.gif", line_gif1)
anim_save("line_gif2.gif", line_gif2)

net_mgif1 <- image_read("net_gif1.gif")
net_mgif2 <- image_read("net_gif2.gif")
line_mgif1 <- image_read("line_gif1.gif")
line_mgif2 <- image_read("line_gif2.gif")

length(net_mgif1); length(net_mgif2); length(line_mgif1); length(line_mgif2)

new_gif1 <- image_append(c(net_mgif1[1], line_mgif1[1]), stack=FALSE)
new_gif2 <- image_append(c(net_mgif2[1], line_mgif2[1]), stack=FALSE)
for(i in 2:100){
  combined1 <- image_append(c(net_mgif1[i], line_mgif1[i]), stack=FALSE)
  combined2 <- image_append(c(net_mgif2[i], line_mgif2[i]), stack=FALSE)
  new_gif1 <- c(new_gif1, combined1)
  new_gif2 <- c(new_gif2, combined2)
}

full_gif <- image_append(c(new_gif1[1], new_gif2[1]), stack=TRUE)
for(i in 2:100){
  combined <- image_append(c(new_gif1[i], new_gif2[i]), stack=TRUE)
  full_gif <- c(full_gif, combined)
}

anim_save(filename="../figures/paired_simulations.gif",
          detail=5, animation=full_gif, end_pause=10)

## static figure for BWF
layout <- igraph::layout_nicely(net1) %>% set_colnames(c("x", "y"))
node_data <- net1 %N>%
  as_tibble() %>%
  mutate(node_id = 1:n()) %>%
  bind_cols(layout) %>%
  mutate(col = ifelse(node_id == 169, "#9a281d", "#000000"))
edge_data <- net1 %E>%
  as_tibble() %>%
  mutate(col = ifelse(from == 169 | to == 169, "#9a281d", "#000000")) %>% 
  unite(from, to, col="edge_id", sep="_", remove=FALSE) %>%
  pivot_longer(c(from, to), names_to="end", values_to="node_id") %>%
  left_join(select(node_data, c(node_id, x, y)), by=c("node_id")) %>%
  pivot_longer(c(x, y), names_to="coordinate", values_to="value") %>%
  # create xend and yend when at the "to" end, for geom_segment use later
  mutate(dir = str_c(coordinate, if_else(end == "to", "end", ""))) %>%
  select(edge_id, dir, col, value) %>%
  arrange(edge_id) %>%
  pivot_wider(names_from=dir, values_from=value)
netplot <- ggplot() +
  geom_segment(data=edge_data, size=1.5, aes(x=x, xend=xend, y=y, yend=yend, group=edge_id, colour=col), alpha=0.67) +
  geom_point(data=node_data, aes(x=x, y=y, group=node_id, colour=col), size=4) +
  scale_colour_identity() +
  theme_graph()

lineplot <- bind_rows(
  sim1 %N>%
    select(-prob_change_status) %>%
    as_tibble() %>%
    pivot_longer(names_to="time", values_to="status", everything()) %>%
    mutate(time = as.integer(time)) %>%
    count(status, time) %>%
    complete(status, nesting(time), fill=list(n=0)) %>% 
    filter(status == "I", time > 50) %>%
    mutate(status = "Original"),
  sim2 %N>%
    select(-prob_change_status) %>%
    as_tibble() %>%
    pivot_longer(names_to="time", values_to="status", everything()) %>%
    mutate(time = as.integer(time)) %>%
    count(status, time) %>%
    complete(status, nesting(time), fill=list(n=0)) %>% 
    filter(status == "I", time > 50) %>%
    mutate(status = "Masker")
  ) %>% 
  ggplot() +
  aes(x=time, y=n, colour=status) +
  geom_line(size=1.5) +
  # scale_colour_manual(values=c("#297373", "#e3944f", "#9a281d")) +
  scale_colour_manual(values=c("#297373", "#9a281d")) +
  ylab("Number of people infectious") + ylab("Time") +
  theme_bw() +
  theme(text=element_text(size=20, family="Alegreya Sans"),
        legend.title=element_blank())

deltaplot <- ggplot(tibble(time=sim1$time[sim1$status=="I"], delta=sim1$n[sim1$status=="I"] - sim2$n[sim2$status=="I"])) +
  aes(x=time, y=delta) +
  geom_line(size=1.5) +
  ylab("Original - Masker") +
  theme_bw() +
  theme(text=element_text(size=20, family="Alegreya Sans"))
ggsave("../figures/delta.svg", width=6, height=4)


library(patchwork)
netplot + lineplot +
