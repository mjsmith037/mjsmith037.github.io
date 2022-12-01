library(magrittr)
library(fontawesome)
library(shiny)
library(magrittr)
library(tidygraph)
library(tidyverse)
library(docstring)
library(ggraph)
library(gganimate)
library(magick)
library(parallel)
library(showtext)
library(ggbeeswarm)
library(patchwork)
library(broom)
library(scales)

showtext_auto()
font_add(family="Alegreya Sans",
         regular="~/.local/share/fonts/AlegreyaSans-Regular.otf")

source("../code/generate_contact_network.R")
source("../code/simulate_disease_on_network.R")
repo_location <- "~/Research/SARS-CoV-2_Network_Model"
source(str_c(repo_location, "/code/common_parameters.R"))

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

theme_set(theme_bw())

#### figures based on final figs in paper ######################################
axis_breaks <- c(1:9/1000, 1:9/100, 1:9/10)
axis_labels <- c(0.001, rep("", 8), 0.01, rep("", 8), 0.1, rep("", 8))

get_results <- function(fig_dir) {
  results <- bind_rows(read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Florida.csv"),
                                col_types="ddddddddddddfdf"),
                       read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Texas.csv"),
                                col_types="ddddddddddddfdf"),
                       read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Florida_2.csv"),
                                col_types="ddddddddddddfdf"),
                       read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Texas_2.csv"),
                                col_types="ddddddddddddfdf"),
                       read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Florida_3.csv"),
                                col_types="ddddddddddddfdf"),
                       read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Texas_3.csv"),
                                col_types="ddddddddddddfdf")) %>%
    # remove because constant
    select(!any_of(c("background_transmission_rate", "pseudo_risk"))) %>%
    mutate(risky_behavior_level = factor(risky_behavior_level, levels=0:2,
                                         labels=c("Vulnerable Households Avoid Work/School",
                                                  "Vulnerable Individuals Avoid Work/School",
                                                  "No Difference in Behavior")) %>% fct_rev())
}

make_combined_risk_pseudorisk_fig <- function(fig_dir) {
  risk_behavior_data <- bind_rows(read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Florida.csv"),
                                           col_types="ddddddddddddfdf"),
                                  read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Texas.csv"),
                                           col_types="ddddddddddddfdf"),
                                  read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Florida_2.csv"),
                                           col_types="ddddddddddddfdf"),
                                  read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Texas_2.csv"),
                                           col_types="ddddddddddddfdf"),
                                  read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Florida_3.csv"),
                                           col_types="ddddddddddddfdf"),
                                  read_csv(str_glue("{repo_location}/results/{fig_dir}_Background_Rate_Results_Texas_3.csv"),
                                           col_types="ddddddddddddfdf")) %>%
    # remove because constant
    select(!any_of(c("background_transmission_rate", "pseudo_risk"))) %>%
    select(!c(`Time to Vulnerable Infection`, number_of_households)) %>%
    pivot_longer(!c(classmate_transmission_rate, coworker_transmission_rate,
                    risky_behavior_level, locale), names_to="variable") %>%
    group_by(variable) %>%
    mutate(value=value %>% add(1) %>% log() %>% rescale(),
           classmate_transmission_rate = log(classmate_transmission_rate),
           coworker_transmission_rate = log(coworker_transmission_rate)) %>%
    ungroup()
  
  pseudorisk_data <- bind_rows(read_csv(str_glue("{repo_location}/results/{fig_dir}_PseudoRisk_Results_Florida.csv"),
                                        col_types="ddddddddddddfdf"),
                               read_csv(str_glue("{repo_location}/results/{fig_dir}_PseudoRisk_Results_Texas.csv"),
                                        col_types="ddddddddddddfdf"),
                               read_csv(str_glue("{repo_location}/results/{fig_dir}_PseudoRisk_Results_Florida_2.csv"),
                                        col_types="ddddddddddddfdf"),
                               read_csv(str_glue("{repo_location}/results/{fig_dir}_PseudoRisk_Results_Texas_2.csv"),
                                        col_types="ddddddddddddfdf"),
                               read_csv(str_glue("{repo_location}/results/{fig_dir}_PseudoRisk_Results_Florida_3.csv"),
                                        col_types="ddddddddddddfdf"),
                               read_csv(str_glue("{repo_location}/results/{fig_dir}_PseudoRisk_Results_Texas_3.csv"),
                                        col_types="ddddddddddddfdf")) %>%
    # remove because constant
    select(!any_of(c("background_transmission_rate", "pseudo_risk"))) %>%
    select(!c(`Time to Vulnerable Infection`, number_of_households)) %>%
    pivot_longer(!c(classmate_transmission_rate, coworker_transmission_rate,
                    risky_behavior_level, locale), names_to="variable") %>%
    group_by(variable) %>%
    mutate(value=value %>% add(1) %>% log() %>% rescale(),
           classmate_transmission_rate = log(classmate_transmission_rate),
           coworker_transmission_rate = log(coworker_transmission_rate)) %>%
    ungroup() %>%
    filter(risky_behavior_level == 0) %>% mutate(risky_behavior_level = factor(-1))
  
  full_plot_data <- bind_rows(risk_behavior_data, pseudorisk_data) %>%
    mutate(variable=factor(variable,
                           levels=c("Maximum Infectious", "Vulnerable Infected"),
                           labels=c("Log peak number of\ninfectious individuals",
                                    "Log number of vulnerable\nindividuals infected")),
           risky_behavior_level=factor(risky_behavior_level,
                                       levels=-1:2,
                                       labels=c("Links removed randomly\n(as many as when Vulnerable\nhouseholds avoid work/school)",
                                                "Vulnerable households\navoid work/school",
                                                "Vulnerable individuals\navoid work/school",
                                                "No difference in behavior")) %>%
             fct_rev()) %>%
    na.omit()
  
  ggplot(full_plot_data) +
    aes(x=classmate_transmission_rate, y=coworker_transmission_rate, z=value, colour=..value..) +
    stat_summary_hex(fun=mean, bins=43) +
    facet_grid(variable~risky_behavior_level) +
    coord_equal() +
    scale_x_continuous(name="Classmate Transmission Rate",
                       breaks=log(axis_breaks), labels=axis_labels, expand=expansion(add=c(-0.13, 0.013))) +
    scale_y_continuous(name="Coworker Transmission Rate",
                       breaks=log(axis_breaks), labels=axis_labels, expand=expansion(add=c(-0.13, 0.013))) +
    scale_fill_viridis_c(na.value="transparent", aesthetics=c("colour", "fill"),
                         breaks=c(0.05, 0.35, 0.65, 0.95), labels=c("Low", "", "", "High"),
                         guide=guide_colourbar(barwidth=unit(2.25, "in"),
                                               title="Rescaled outcome measure",
                                               title.position="top")) +
    theme(legend.position="bottom",
          plot.background=element_blank(),
          panel.background=element_blank(),
          text=element_text(size=14, family="Alegreya Sans"),
          panel.grid=element_blank(),
          legend.background=element_blank())
  ggsave(str_glue("../figures/LINCS/risk_comparison.svg"), width=9.5, height=6.25, bg="transparent")
}

make_locale_comp_fig <- function(fig_dir) {
  get_results(fig_dir) %>%
    mutate(across(c(`Maximum Infectious`, `Average Infectious`, `Total Died`, `Total Infected`),
                  ~divide_by(., pop_size)),
           `Vulnerable Infected` = `Vulnerable Infected` / vuln_pop_size) %>%
    select(!c(number_of_households, pop_size, vuln_pop_size)) %>%
    filter(`Total Infected` > 0.05,
           risky_behavior_level == "No Difference in Behavior") %>%
    pivot_longer(!c(classmate_transmission_rate, coworker_transmission_rate,
                    risky_behavior_level, locale), names_to="variable") %>%
    filter(variable %in% c("Maximum Infectious", "Total Died", "Vulnerable Infected", "vuln_pop_size")) %>%
    mutate(variable=factor(variable,
                           levels=c("Maximum Infectious", "Total Died", "Vulnerable Infected"),
                           labels=c("Maximum percentage\nconcurrently infectious",
                                    "Percentage of total\npopulation that died",
                                    "Percentage of vulnerable\nindividuals infected"))) %>%
    ggplot() +
    aes(y=value, x=locale, colour=locale) +
    geom_quasirandom(varwidth=TRUE, groupOnX=TRUE, show.legend=FALSE) +
    geom_boxplot(fill="white", colour="black", width=0.2, alpha=0.5, outlier.alpha=0, show.legend=FALSE) +
    scale_y_continuous(trans=pseudo_log_trans(sigma=0.0025, base=10),
                       breaks=c(0, 0.005, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1), #c(0, 0.005, 0.01, 0.05, 0.1, 0.5, 1),
                       minor_breaks=c(0.001, 0.002, 0.003, 0.004, 0.006, 0.007, 0.008, 0.009, 0.04, 0.06, 0.07, 0.08, 0.09, 0.4, 0.6, 0.7, 0.8, 0.9), #c(0.002, 0.003, 0.004, 0.02, 0.03, 0.04, 0.2, 0.3, 0.4),
                       labels=~str_c(round(., 3) * 100, "%")) +
    facet_wrap(~variable, scales="free", ncol=1, strip.position="right") +
    scale_colour_manual(values=my_cols[c(1, 11, 9)] %>% unname(), aesthetics=c("colour", "fill")) +
    coord_flip() +
    theme(axis.title=element_blank(),
          rect = element_rect(fill = "transparent"),
          text=element_text(family="Lato-Medium"))
  ggsave(str_glue("../figures/LINCS/locale_comparison.pdf"), width=4, height=5.65, bg="transparent")
}

make_inttype_comp_fig <- function(fig_dir) {
  get_results(fig_dir) %>%
    mutate(across(c(`Maximum Infectious`, `Average Infectious`, `Total Died`, `Total Infected`),
                  ~divide_by(., pop_size)),
           `Vulnerable Infected` = `Vulnerable Infected` / vuln_pop_size,
           Inf_Rate = `Maximum Infectious` / `Time to Peak`) %>%
    select(!`Time to Vulnerable Infection`) %>%
    filter(`Total Infected` > 0.05) %>%
    pivot_longer(!c(classmate_transmission_rate, coworker_transmission_rate,
                    risky_behavior_level, locale, number_of_households,
                    pop_size, vuln_pop_size),
                 names_to="variable") %>%
    group_by(locale, risky_behavior_level, variable) %>%
    group_modify(~lm(value~log(classmate_transmission_rate) + log(coworker_transmission_rate), data=.) %>%
                   tidy() %>% filter(term != "(Intercept)")) %>%
    mutate(term = factor(term,
                         levels=c("log(classmate_transmission_rate)", "log(coworker_transmission_rate)"),
                         labels=c("classmate", "coworker")),
           variable=factor(variable,
                           levels=c("Maximum Infectious",
                                    "Total Died"),
                           labels=c("Log peak number of\ninfectious individuals",
                                    "Log total number of\nindividuals that died")),
           risky_behavior_level=factor(risky_behavior_level,
                                       levels=c("No Difference in Behavior",
                                                "Vulnerable Individuals Avoid Work/School",
                                                "Vulnerable Households Avoid Work/School"),
                                       labels=c("No difference\nin behavior",
                                                "Vulnerable\nindividuals avoid\nwork/school",
                                                "Vulnerable\nhouseholds avoid\nwork/school"))) %>%
    na.omit() %>%
    ggplot() +
    aes(x=term, y=estimate, ymax=estimate + std.error, ymin=estimate - std.error,
        colour=locale, shape=risky_behavior_level) +
    geom_line(aes(x=as.integer(as.factor(term))), position=position_dodge2(width=0.15, reverse=TRUE)) +
    geom_linerange(size=1, position=position_dodge2(width=0.15, reverse=TRUE)) +
    geom_point(size=3, position=position_dodge2(width=0.15, reverse=TRUE)) +
    facet_wrap(~variable, scales="free", ncol=1, strip.position="right") +
    ylab(expression(atop("Linear model slope estimates",
                         paste(( y == m[1]*log(beta[classmate]) + m[2]*log(beta[coworker]) + b))))) +
    scale_colour_manual(values=my_cols[c(1, 11)] %>% unname()) +
    scale_x_discrete(expand=expansion(add=0.5), labels=scales::parse_format()) +
    coord_flip() +
    theme(legend.title=element_blank(),
          legend.position="bottom",
          axis.title.y=element_blank(),
          legend.margin=margin(5, 30, 5, -10),
          rect = element_rect(fill="transparent"),
          text=element_text(family="Lato-Medium"),
          legend.box="vertical",
          legend.key=element_rect(fill=NA))
  ggsave(str_glue("../figures/LINCS/inttype_comparison.pdf"), width=4, height=7.35, bg="transparent")
}

make_combined_risk_pseudorisk_fig(0.001)
# make_locale_comp_fig(0.001)
# make_inttype_comp_fig(0.001)

#### graph plots adding layers #################################################
# graph plotting takes a while
full_net <- generate_contact_network(100,
                                     classmate_transmission_rate=function(n) rep(0.005, n),
                                     coworker_transmission_rate=function(n) rep(0.005, n))
net <- full_net %E>% filter(type != "background") %>% 
  mutate(type = recode(type, "household"="Housemates", "classmate"="Classmates", "coworker"="Coworkers"))
layout <- igraph::layout_nicely(net)

ggraph(net, layout=layout) +
  geom_edge_link(edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  theme_graph(background=NULL) +
  theme(legend.position="none")
ggsave("../figures/LINCS/net.svg", width=7, height=6, bg="transparent")

ggraph(net %>% filter(type == "Housemates"), layout="nicely") +
  geom_edge_link(aes(colour=type),
                 edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  scale_edge_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide="none") +
  guides(edge_colour=guide_legend(override.aes=list(edge_width=1.5, edge_alpha=1))) +
  theme_graph(background=NULL) +
  theme(text=element_text(size=32, family="Alegreya Sans"),
        legend.position="bottom",
        legend.title=element_blank())
ggsave("../figures/LINCS/edges_zero.svg", width=7, height=6, bg="transparent")

ggraph(net %>% filter(type != "Coworkers"), layout="nicely") +
  geom_edge_link(aes(colour=type),
                 edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  scale_edge_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide="none") +
  guides(edge_colour=guide_legend(override.aes=list(edge_width=1.5, edge_alpha=1))) +
  theme_graph(background=NULL) +
  theme(text=element_text(size=32, family="Alegreya Sans"),
        legend.position="bottom",
        legend.title=element_blank())
ggsave("../figures/LINCS/edges_one.svg", width=7, height=6, bg="transparent")

ggraph(net, layout=layout) +
  geom_edge_link(aes(colour=type),
                 edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3) +
  scale_edge_colour_manual(values=my_cols) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide="none") +
  guides(edge_colour=guide_legend(override.aes=list(edge_width=1.5, edge_alpha=1))) +
  theme_graph(background=NULL) +
  theme(text=element_text(size=32, family="Alegreya Sans"),
        legend.position="bottom",
        legend.title=element_blank())
ggsave("../figures/LINCS/edges.svg", width=7, height=6, bg="transparent")

ggraph(net, layout=layout) +
  geom_edge_link(edge_alpha=0.67, edge_width=0.5,
                 end_cap=circle(radius=5, unit="pt"), start_cap=circle(radius=5, unit="pt")) +
  geom_node_point(size=3, aes(colour=ifelse(vulnerable, "Vulnerable", "Not Vulnerable"))) +
  scale_colour_manual(values=my_cols[c("Vulnerable", "Not Vulnerable")]) +
  scale_edge_alpha_manual(values=c(family=0.33, classmate=0.33, coworker=0.33), guide="none") +
  theme_graph(background=NULL) +
  theme(text=element_text(size=32, family="Alegreya Sans"),
        legend.position="bottom",
        legend.title=element_blank())
ggsave("../figures/LINCS/nodes.svg", width=7, height=6, bg="transparent")

#### disease spread gif ########################################################

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



#### locale household size and age differences #################################
house_size <- read_csv(str_glue("{repo_location}/data/household_size_demographics.csv"),
         skip=3, col_types="cddddddddl",
         col_names=c("Locale", "1", "2", "3", "4", "5", "6",
                     "7", # note actual heading here is 7 **or more**
                     "average_household_size", "extrapolated")) %>%
  pivot_longer(-Locale, names_to="Household Size", values_to="Proportion") %>%
  filter(Locale %in% c("Florida", "Texas")) %>%
  group_by(Locale) %>%
  mutate(Proportion = divide_by(Proportion, sum(Proportion))) %>%
  mutate(`Household Size` = factor(`Household Size`, levels=1:7,
                                   labels=c("1", "2", "3", "4", "5", "6", "> 6"))) %>%
  na.omit() %>%
  {ggplot(.) +
      aes(x=`Household Size`, y=Proportion, fill=Locale) +
      geom_bar(stat="Identity", position="dodge") +
      geom_text(aes(label=lab), nudge_y=0.01, nudge_x=0.2, size=5,
                data=filter(., Locale == "Texas", `Household Size` %in% c(4:6, "> 6")) %>% mutate(lab="*")) +
      scale_fill_manual(values=my_cols[c(1,11,3,9)] %>% unname()) +
      facet_wrap(~"Household Size") +
      theme(text=element_text(size=32, family="Alegreya Sans"))}
age_dist <- read_csv(str_glue("{repo_location}/data/age_demographics.csv"),
         skip=1, col_types="dddddddddddddc") %>%
  # filter(Locale %in% c("Texas", "Florida", "United States", "US_1960")) %>%
  filter(Locale %in% c("Florida", "Texas")) %>%
  pivot_longer(-Locale, names_to="Age", values_to="Proportion") %>%
  mutate(Age = Age %>% str_remove(" years") %>% factor() %>% fct_inorder(),
         Proportion = Proportion /100) %>%
  {ggplot(.) +
      aes(x=Age, y=Proportion, fill=Locale) +
      geom_bar(stat="Identity", position="dodge") +
      scale_fill_manual(values=my_cols[c(1,11,3,9)] %>% unname()) +
      facet_wrap(~"Age") +
      theme(text=element_text(size=32, family="Alegreya Sans"),
            axis.text.x=element_text(angle=30, hjust=1))}
house_size / age_dist + plot_layout(guides="collect") &
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.title.x=element_blank())
ggsave("../figures/LINCS/locale_demographic_comparison.svg", width=8, height=8)
