library(fontawesome)
library(shiny)
library(magrittr)
library(tidygraph)
library(tidyverse)
library(docstring)
library(ggraph)
library(ggraph)
library(gganimate)
library(magick)
library(parallel)

source("../code/generate_contact_network.R")
source("../code/simulate_disease_on_network.R")

set.seed(1)

my_cols <- c(
  "S"  = "#0b6884", "Susceptible"                 = "#0b6884",
  "E"  = "#50b99a", "Exposed"                     = "#50b99a",
  "Ia" = "#ff5e5b", "Asymptomatically Infectious" = "#ff5e5b",
  "Is" = "#ffc847", "Symptomatically Infectious"  = "#ffc847",
  "R"  = "#7b678e", "Recovered"                   = "#7b678e",
  "D"  = "#bc4b51", "Dead"                        = "#bc4b51",
  "Vulnerable"     = "#ff5e5b",
  "Not Vulnerable" = "#0b6884",
  "Housemates"          = "#0b6884", "household"         = "#0b6884",
  "Extended Housemates" = "#bc4b51", "between_household" = "#bc4b51",
  "Classmates"          = "#50b99a", "classmate"         = "#50b99a",
  "Coworkers"           = "#7b678e", "coworker"          = "#7b678e",
  "Bar"                 = "#ff5e5b", "bar"               = "#ff5e5b"
)

setwd("../figures/")

theme_set(theme_bw(base_size=12))

# graph plotting takes a while
full_net <- generate_contact_network(100,
                                     classmate_transmission_rate=function(n) rep(0.005, n),
                                     coworker_transmission_rate=function(n) rep(0.005, n))
net <- full_net %E>% filter(type != "background")
layout <- igraph::layout_nicely(net)

ggraph(net, layout=layout) +
  geom_edge_link(edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  theme_graph(background=NULL) +
  theme(legend.position="none")
ggsave("net.svg", width=7, height=6, bg="transparent")


ggraph(net %>% filter(type == "household"), layout="nicely") +
  geom_edge_link(aes(colour=type),
                 edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  scale_edge_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide=FALSE) +
  guides(edge_colour=guide_legend(override.aes=list(edge_width=1.5, edge_alpha=1))) +
  theme_graph(background=NULL) +
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("edges_zero.svg", width=7, height=6, bg="transparent")

ggraph(net %>% filter(type != "coworker"), layout="nicely") +
  geom_edge_link(aes(colour=type),
                 edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  scale_edge_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide=FALSE) +
  guides(edge_colour=guide_legend(override.aes=list(edge_width=1.5, edge_alpha=1))) +
  theme_graph(background=NULL) +
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("edges_one.svg", width=7, height=6, bg="transparent")

ggraph(net, layout=layout) +
  geom_edge_link(aes(colour=type),
                 edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  scale_edge_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide=FALSE) +
  guides(edge_colour=guide_legend(override.aes=list(edge_width=1.5, edge_alpha=1))) +
  theme_graph(background=NULL) +
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("edges.svg", width=7, height=6, bg="transparent")

ggraph(net, layout=layout) +
  geom_edge_link(edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3, aes(colour=ifelse(vulnerable, "Vulnerable", "Not Vulnerable"))) +
  scale_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide=FALSE) +
  theme_graph(background=NULL) +
  theme(legend.position="bottom",
        legend.title=element_blank())
ggsave("nodes.svg", width=7, height=6, bg="transparent")

make_composite_disease_gif <- function(time_max=200, filename="dynamics_1.gif") {

  output <- run_SARSCoV2_simulation(time_max, full_net, beta_values=c(household=0.33,
                                                                      background=0.25/244/10,
                                                                      classmate=0.005,
                                                                      coworker=0.005)) %>%
    select(-starts_with("number_infectious_"), -household_id, -age_class, -vulnerable) %>%
    as_tibble() %>%
    pivot_longer(names_to="time", values_to="status", matches("\\d+")) %>%
    mutate(time = as.integer(time),
           status = factor(status, levels=c("S", "E", "Ia", "Is", "R", "D"),
                           labels=c("Susceptible", "Exposed", "Asymptomatically Infectious",
                                    "Symptomatically Infectious", "Recovered", "Dead")))

  layout_matrix <- layout[rep(1:nrow(layout), times=time_max), ]

  net_plot <- ggraph(tbl_graph(nodes=output %>% arrange(time),
                               edges=net %E>% as_tibble() %>% filter(type != "background")),
                     layout=layout_matrix) +
    geom_edge_link(edge_alpha=0.67, edge_width=0.5,
                   end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
    geom_node_point(aes(colour=status), size=3) +
    scale_colour_manual(values=my_cols) +
    # scale_edge_alpha_manual(values=c(family=1, classmate=0.25, coworker=0.25)) +
    transition_manual(time) +
    theme_graph(background=NULL) +
    theme(legend.position="none")
  net_gif <- animate(net_plot, end_pause=30, width=7, height=6, units="in", res=300, bg="transparent")
  anim_save("net.gif")

  timeseries <- output %>%
    count(status, time) %>%
    complete(status, nesting(time), fill=list(n=0))
  line_plot <- ggplot(timeseries) +
    aes(x=time, y=n, fill=status) +
    geom_area() +
    transition_reveal(time) +
    scale_fill_manual(values=my_cols) +
    coord_cartesian(clip="off", expand=FALSE) +
    xlab("Time") + ylab("Number of Individuals") +
    theme_minimal() +
    theme(panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank(),
          axis.text.y=element_text(size=14),
          axis.title=element_text(size=16))
  line_gif <- animate(line_plot, end_pause=30, width=8, height=6, units="in", res=300, bg="transparent")
  anim_save("line.gif")

  # combined <- map2(net_gif %>% image_read() %>% as.list(),
  #      line_gif %>% image_read() %>% as.list(),
  #      ~image_append(c(.x, .y))) %>%
  #   lift(image_join)(.) %T>%
  #   image_write(filename)
}
set.seed(3)
make_composite_disease_gif()

appendGIFs <- function(gif1, gif2, gifout, vertically=FALSE, delay=0){
  currentdir <- getwd()
  on.exit(setwd(currentdir))
  tmpdir <- tempdir()
  invisible(file.remove(list.files(tmpdir, full.names = TRUE, pattern = "*.gif$")))
  file.copy(gif1, to = file.path(tmpdir, gif1))
  file.copy(gif2, to = file.path(tmpdir, gif2))
  setwd(tmpdir)
  command <- sprintf("/usr/bin/convert %s -coalesce gif1-%%05d.gif", gif1)
  system(command)
  command <- sprintf("/usr/bin/convert %s -coalesce gif2-%%05d.gif", gif2)
  system(command)
  nframes <- length(list.files(tmpdir, pattern = "^gif1-.*gif$"))
  tmp <- mclapply(1:nframes, mc.cores=8, function(i) {
    command <- sprintf("/usr/bin/convert gif1-%05d.gif gif2-%05d.gif %sappend gif-%05d.gif",
                       i-1, i-1, ifelse(vertically, "-", "+"), i)
    system(command)
  })
  command <- sprintf("/usr/bin/convert -loop 0 -delay %d gif-*.gif %s", delay, gifout)
  system(command)
  file.copy(gifout, file.path(currentdir, gifout), overwrite=TRUE)
}
appendGIFs("net.gif", "line.gif", "dynamics_1.gif")
