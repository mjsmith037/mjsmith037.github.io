library(magrittr)
library(tidyverse)
library(tidygraph)
library(JuliaCall)
library(ggraph)
library(facetscales)
library(patchwork)

julia_setup()
julia_source("../../code/multipop_MANTIS.jl")

my_cols <- c("#f74700", "#016394", "#b6003b", "#005342")
scales_y <- list(
  `currently infectious (y)`    = scale_y_continuous(),
  `specific immunity (z)`       = scale_y_continuous(limits = c(0, 1)),
  `cross-reactive immunity (w)` = scale_y_continuous(limits = c(0, 1))
)

## structural parameters
set.seed(0)
struct <- c(2, 2)
strains <- expand.grid(lapply(struct, seq, from=1, by=1) %>%
                         setNames(str_c("locus", 1:length(.))))
n_populations <- 4
# initial_conditions <- rep(0.00001, prod(struct) * n_populations)
initial_conditions <- runif(prod(struct) * n_populations) %>%
  matrix(n_populations, prod(struct)) %>%
  apply(1, . %>% {. / (5 * sum(.))}) %>%
  t()
## dynamical parameters
beta <- 40            # Infection rate
gamma <- 0.75         # partial cross-protective immunity (cpi)
sigma <- 10           # recovery rate
mu <- 0.05            # disease induced mortality
# delta <- NA           # increase in cpi per allele (not yet implemented)
# epsilon <- 0          # seasonality

movement_rate <- 0.05
chi <- matrix(c(-movement_rate, 0, 0, 0,
                movement_rate, -movement_rate, 0, 0,
                0, movement_rate, -movement_rate, 0,
                0, 0, movement_rate, 0), 4, 4)

timeseries <- julia_call("runMANTIS", strainstructure=struct, tstep=1, tmax=1000,
                         beta=beta, gamma=gamma, sigma=sigma, mu=mu, chi=chi,
                         initvals=initial_conditions)$timeseries %>%
  set_colnames(c(expand.grid(str_c("Population_", 1:n_populations),
                             str_c("Strain_", apply(strains, 1, str_c, collapse="")),
                             c("y", "z", "w")) %>% apply(1, str_c, collapse="."),
                 "time")) %>%
  gather("details", "prevalence", -time) %>%
  separate(details, c("population", "strain", "equation"), "\\.") %>%
  mutate(variable = factor(equation, levels=c("y", "z", "w"),
                           labels=c("currently infectious (y)",
                                    "specific immunity (z)",
                                    "cross-reactive immunity (w)")),
         population = factor(population, levels=c("Population_1", "Population_2", "Population_3", "Population_4"),
                             labels=c("Population A", "Population B", "Population C", "Population D"))) %>%
  as_tibble()

ggplot(timeseries %>% filter(time > 0.85*max(time), strain == "Strain_11")) +
  aes(colour=population, y=prevalence, x=time) +
  geom_line() +
  facet_grid_sc(rows=vars(variable), cols=vars(population), scales=list(y=scales_y)) +
  scale_colour_manual(values=my_cols) +
  ylab("Proportion of population") +
  theme_bw() +
  theme(legend.position="none",
        strip.text.x=element_blank(),
        strip.background.x=element_blank())
# ggsave("dampening-cycles.svg", width=10.5, height=6)

aaa <- ggplot(timeseries %>% filter(time > 0.85*max(time), population == "Population A", equation == "y")) +
  aes(colour=strain, y=prevalence, x=time) +
  geom_line() +
  facet_grid(strain~variable) +
  scale_colour_manual(values=my_cols) +
  ylab("Proportion of population") +
  theme_bw() +
  theme(legend.position="none",
        strip.text.y=element_blank(),
        strip.background.y=element_blank())
bbb <- ggplot(timeseries %>% filter(time > 0.85*max(time), population == "Population A", equation == "z")) +
  aes(colour=strain, y=prevalence, x=time) +
  geom_line() +
  facet_grid(strain~variable) +
  scale_colour_manual(values=my_cols) +
  scale_y_continuous(limits=0:1) +
  ylab("Proportion of population") +
  theme_bw() +
  theme(legend.position="none",
        axis.title.y=element_blank(),
        strip.text.y=element_blank(),
        strip.background.y=element_blank())
ccc <- ggplot(timeseries %>% filter(time > 0.85*max(time), population == "Population A", equation == "w")) +
  aes(colour=strain, y=prevalence, x=time) +
  geom_line() +
  facet_grid(strain~variable) +
  scale_colour_manual(values=my_cols) +
  scale_y_continuous(limits=0:1) +
  ylab("Proportion of population") +
  theme_bw() +
  theme(legend.position="none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
aaa + bbb + ccc
# ggsave("dampening-cycles_all_strains.svg", width=10.5, height=6)
