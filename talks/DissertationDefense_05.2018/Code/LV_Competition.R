library(tidyverse)
library(stringr)
library(extrafont)

source("my_ggplot_theme.R")

########## GLOBAL PLOTTING VARIABLES ##########
two_pop_colours <- c("#4592d1", "#f07973")
stability_shapes <- c("1"=21, "-1"=19)

theme_set(my_theme())

BASE_HEIGHT <- 3
BASE_WIDTH <- 5
POINT_SIZE <- 3
LINE_THICKNESS <- 1

#### Lotka-Volterra Competition
lv_comp_graphical_methods <- function(a_pairs) {
    ## interaction strengths
    params <- a_pairs %>%
        mutate(a12_magnitude = ifelse(a12 < 1, "italic(a)[12] < 1", "italic(a)[12] > 1"),
               a21_magnitude = ifelse(a21 < 1, "italic(a)[21] < 1", "italic(a)[21] > 1"))
    ## isocline equations
    isoclines <- bind_rows(params %>% mutate(pop = "N1star", m = -1/a12, b = 1/a12),
                           params %>% mutate(pop = "N2star", m = -a21, b = 1))
    ## equilibria locations
    equilibria <- bind_rows(params %>%
                                mutate(N2 = (1 - a21) / (1 - a21 * a12),
                                       N1 = (1 - a12) / (1 - a12 * a21)),
                            ## trivial equilibria
                            params %>% mutate(N1=0, N2=0),
                            params %>% mutate(N1=0, N2=1),
                            params %>% mutate(N1=1, N2=0)) %>%
        rowwise() %>%
        ## note absence of rho -- rho is always positive in LV, so does not
        ##   affect stability qualitatively
        mutate(stability = eigen(matrix(c(1 - 2 * N1 - a12 * N2,
                                          -a21 * N2,
                                          -a12 * N1,
                                          (1 - 2 * N2 - a21 * N1)), 2, 2),
                                 only.values=TRUE)$values %>%
                   Re() %>% max() %>% sign() %>% as.factor())
    ## plotting
    p <- ggplot(isoclines) +
        ## non-trivial isoclines
        geom_abline(aes(slope=m, intercept=b, colour=pop), size=LINE_THICKNESS) +
        ## add trivial isoclines
        geom_hline(aes(colour="N2star", yintercept=0), size=LINE_THICKNESS) +
        geom_vline(aes(colour="N1star", xintercept=0), size=LINE_THICKNESS) +
        ## focus in on the feasible quandrant
        scale_x_continuous(#expand=c(0,0.01),
                           limits=c(0, params %>%
                                        summarise(max(1/a21, 1) * 1.1) %>%
                                        as.numeric()),
                           name=expression(paste(N[1]))) +
        scale_y_continuous(#expand=c(0,0.02),
                           limits=c(0, params %>%
                                        summarise(max(1/a12, 1) * 1.1) %>%
                                        as.numeric()),
                           name=expression(paste(N[2]))) +
        ## standardize colors
        scale_colour_manual(values=two_pop_colours) +
        ## facet by dynamics
        facet_grid(a12_magnitude~a21_magnitude, labeller=label_parsed) +
        theme(legend.position="none")
    ## and one with them
    p <- p +
        ## add dashed line for saddle point
        geom_abline(aes(slope=m, intercept=0), colour="#f4e6d4", linetype="dotted",
                    data=equilibria %>%
                        filter(N1 > 0, N2 > 0, a21_magnitude == "italic(a)[21] > 1") %>%
                        mutate(m=N2/N1)) +
        ## add equilibria
        geom_point(aes(x=N1, y=N2, shape=stability),
                   fill="white", size=POINT_SIZE, data=equilibria) +
        scale_shape_manual(values=stability_shapes) +
        theme(text=element_text(colour="#f4e6d4", family="Noto Serif"),
              strip.text=element_text(size=18, colour="#f4e6d4"),
              plot.background=element_rect(fill="#2b3141"))
    plot(p)
    ggsave(p, filename="../Figures/LV_Competition.svg", height=BASE_WIDTH, width=1.33*BASE_WIDTH, bg="transparent")
}
lv_comp_graphical_methods(a_pairs=expand.grid(list(a12=c(0.7, 1.9), a21=c(0.75, 2.5))))

