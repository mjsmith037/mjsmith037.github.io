library(ggraph)
library(tidygraph)
library(tidyverse)
library(patchwork)
library(magrittr)

theme_set(theme_bw())

distance <- tibble(distance=seq(0, 18, length.out=500),
                   probability=seq(0, 20, length.out=500) %>% {0.95 / (1.125^.)})
time <- tibble(time=seq(0, 45, length.out=500),
               probability=seq(0, 1, length.out=500) %>% {35 * .^2 / (3 + 35 * 0.96 * .^2)})
joint <- tcrossprod(time$probability, distance$probability) %>%
  as_tibble() %>%
  set_colnames(distance$distance) %>%
  mutate(time = time$time) %>%
  pivot_longer(!time, names_to="distance", values_to="probability") %>%
  mutate(distance=as.numeric(distance))

joint %>% filter(near(time, 15, tol=0.05), near(distance, 6, tol=0.02))

ggplot(distance) +
  aes(x=distance, y=probability) +
  geom_line(size=2) +
  scale_x_continuous(name="Distance (ft)", breaks=0:3 * 6) +
  ylab("Probability of infection") +
  ylim(0, 1) +
  ggtitle("Infection risk decreases with distance ...") +
ggplot(time) +
  aes(x=time, y=probability) +
  scale_x_continuous(name="Time (min)", breaks=0:3 * 15) +
  geom_line(size=2) +
  ylim(0, 1) +
  ggtitle("and increases with time spent ...") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
ggplot(joint) +
  aes(x=distance, y=time, fill=probability) +
  geom_raster(interpolate=TRUE) +
  geom_contour(aes(z=probability, colour="25% chance\nof infection"),
               breaks=0.25, linetype="21", size=1) +
  # geom_point(size=4, shape=10, stroke=1.5, fill=NA,
  #            colour=viridis::viridis_pal(option="H", begin=0.26)(1), data=tibble(distance=6, time=15)) +
  # geom_hline(aes(yintercept=I(15)), colour="red") +
  # geom_vline(aes(xintercept=I(6)), colour="red") +
  scale_x_continuous(name="Distance (ft)", breaks=0:3 * 6) +
  scale_y_continuous(name="Time (min)", breaks=0:3 * 15) +
  coord_cartesian(expand=FALSE) +
  scale_fill_viridis_c(option="E", limits=c(0, 1)) +
  scale_colour_manual(values=viridis::viridis_pal(option="H", begin=0.26)(1)) +
  ggtitle("leading to a region of likely infection used to inform guidelines") +
  guides(fill=guide_colourbar(barheight=9),
         colour=guide_legend(title=element_blank())) +
  plot_layout(widths=c(1, 1, 1.2)) &
  theme(text=element_text(family="Alegreya"),
        legend.margin=margin(),
        legend.box.margin=margin(t=0.5, unit="in"),
        plot.title=element_text(size=10))
ggsave("../../Images/blog_figures/vaccination_strategy/risk_distribution.png", width=10, height=3)

icons <- c("user",
  "user-alien",
  "user-astronaut",
  "user-cowboy",
  "user-crown",
  "user-graduate",
  "user-hard-hat",
  "user-headset",
  "user-injured",
  "user-md",
  "user-ninja",
  "user-nurse",
  "user-robot",
  "user-secret",
  "user-tie",
  "user-visor")

set.seed(144)
play_erdos_renyi(n=16, m=32) %>%
  mutate(icon = sample(icons, n(), replace=FALSE)) %>%
  mutate(col = ifelse(icon == "user-nurse", viridis::viridis_pal(option="H", begin=0.26)(1), "#000000")) %>%
  ggraph() +
  geom_node_text(aes(label=icon, colour=col), size=5, family="Font Awesome 5 Pro Solid", show.legend=FALSE) +
  geom_edge_link(start_cap=geometry(width=0.85),
                 end_cap=geometry(width=0.85)) +
  scale_colour_identity() +
  theme(panel.border=element_blank())
ggsave("../../Images/blog_figures/vaccination_strategy/network.png", width=4, height=3)
