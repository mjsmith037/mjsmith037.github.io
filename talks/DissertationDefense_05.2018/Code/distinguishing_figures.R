library(igraph)
library(scales)
library(stringr)
library(tidyverse)
library(broom)

source("my_ggplot_theme.R")
theme_set(my_theme())

BIPLOT_HEIGHT <- 3.75
BIPLOT_WIDTH <- 1.5*BIPLOT_HEIGHT

source("~/Research/matt_s_problem/code/plotting/ggbiplot.R")
set.seed(0) # to replicate sampling

#### KEY PARAMETERS
N_FIT <- 100
results_file <- "~/Research/matt_s_problem/results/full_results.csv"

#### scales for ggplots
cols <- c("crimes"                  = "#82d255",
          "ecological interactions" = "#e07699",
          "antagonism"              = "#e07699",
          "mutualism"               = "#95a7d5",
          "microbiome"              = "#c86ede",
          "actor collaboration"     = "#ccba4e",
          "biogeography"            = "#79c8a5",
          "legislature"             = "#e37843",
          "authorship"              = "#c4b096")
shps <- c("crimes"                  = 0,
          "ecological interactions" = 8,
          "antagonism"              = 4,
          "mutualism"               = 3,
          "microbiome"              = 2,
          "actor collaboration"     = 7,
          "biogeography"            = 1,
          "legislature"             = 5,
          "authorship"              = 6)

## a useful function to remove all geoms of a given type from a ggplot object
remove_geom <- function(ggplot2_object, geom_type) {
    ## Nullify layers that match the requested type.
    layers <- lapply(ggplot2_object$layers, function(x) {
        if (class(x$geom)[1] %in% geom_type) NULL else x
    })
    ## Delete the unwanted layers.
    layers <- layers[!unlist(lapply(layers, is.null))]
    ## and replace them in the original ggplot object before returning
    ggplot2_object$layers <- layers
    return(ggplot2_object)
}
replace_legend <- function(plot_obj, new_legend) { 
    tmp <- ggplot_gtable(ggplot_build(plot_obj)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    tmp$grobs[[leg]] <- new_legend
    return(tmp)
}
extract_legend <- function(plot_obj) { 
    tmp <- ggplot_gtable(ggplot_build(plot_obj)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)
}
ellipse_path <- function(x) {
    theta <- c(seq(-pi, pi, length=50), seq(pi, -pi, length=50))
    base_circle <- cbind(cos(theta), sin(theta))
    if(nrow(x) <= 2) return(NULL)
    sigma <- x %>% select(PC1, PC2) %>% var()
    mu <- x %>% select(PC1, PC2) %>% summarise_all("mean") %>% as.numeric()
    ed <- 0.68 %>% qchisq(df=2) %>% sqrt()
    return(data.frame(sweep(base_circle %*% chol(sigma) * ed, 2, mu, FUN = '+')))
}


#### read in the results file
results_df <- read_csv(results_file,
                       ## specify type for these columns because they vary between networks or cause errors in parsing
                       col_types=cols(nrows = col_double(),
                                      H2 = col_double(),
                                      H3 = col_double(),
                                      H4 = col_double(),
                                      feature1 = col_character(),
                                      feature2 = col_character(),
                                      feature3 = col_character(),
                                      feature4 = col_character())) %>%
    ## clean up some names
    mutate(type = tolower(type),
           type = str_replace(type, "movies", "actor collaboration"),
           type = str_replace(type, "ecologicalinteractions", "ecological interactions"))
#### filtering/cleaning
## remove Minneapolis that has some strange nets
# results_df <- results_df %>% filter(feature1 != "Minneapolis")
## remove movies datasets
# results_df <- results_df %>% filter(type != "movies")


#### specify/calculate the variables to consider in PCA
Metrics <- list(
    # "ER_Ratio"          = ~l1 / l1_er,
    # "Reg_Ratio"         = ~l1 / l1_reg,
    # "Lap_Ratio"         = ~l1 / l1_lap,
    # "Overlap_Ratio"     = ~l1 / l1_olap,
    # "l2_Ratio"          = ~l2 / l2_mp,
    "Gap_Ratio_21"       = ~g12 / l1,
    "Gap_Ratio_32"       = ~g23 / l2,
    # "Lap_Gap_Ratio_21"  = ~lap_g12 / l1_lap,
    # "Lap_Gap_Ratio_32"  = ~lap_g23 / l2_lap,
    # "Gap_Lap_Gap_Ratio" = ~lap_g12 / l1_lap,
    # "MP_Gap_Ratio"      = ~(l2-l3) / (l2_mp-l3_mp),
    # "lm_Ratio"          = ~lm / lm_mp,
    # "m1_Ratio"          = ~bulk_moment_1 / mp_moment_1,
    # "m2_Ratio"          = ~bulk_moment_2 / mp_moment_2,
    # "m3_Ratio"          = ~bulk_moment_3 / mp_moment_3,
    # "m4_Ratio"          = ~bulk_moment_4 / mp_moment_4,
    # "l2l3_Ratio"        = ~l3 / l2,
    # "ipr1"              = ~ipr1,
    # "ipr2"              = ~ipr2,
    # "ipr3"              = ~ipr3,
    # "Alg_Connectence"   = ~alg_conn,
    # "cent_between"       = ~cent_between * log(nlinks),
    "cent_close"         = ~cent_close,
    # "cent_eigen"         = ~cent_eigen / log((nrows + ncols) / (nlinks + 0.1)),
    # "av_short_path"      = ~mean_path_length,
    "diam"               = ~diam / log(2*nlinks / (nrows + ncols)^2),
    # "H2"                 = ~log(H2) / log((nlinks) / (nrows + ncols) + 1),
    # "H3"                 = ~log(H3) / log((nlinks) / (nrows + ncols) + 1),
    # "H4"                 = ~log(H4) / log((nlinks) / (nrows + ncols) + 1),
    # "H17"                = ~log(H17) / log((nlinks) / (nrows + ncols) + 1),
    # "H4H2"               = ~H4 / H2,
    # "H17H4"              = ~H17 / H4,
    # "Q"                  = ~Q,
    # "N.olap"             = ~N.olap,
    # "N.temp"             = ~N.temp,
    # "N.nodf"             = ~N.nodf,
    # "deg_het_row"        = ~deg_het_row,
    # "deg_het_col"        = ~deg_het_col,
    "CM_Ratio"           = ~l1 / l1_cm
)

#### subsetting to equal sample sizes for fitting
to_fit_df <- results_df %>%
    filter(randomization == "None",
           # type != "actor collaboration",
           type != "ecological interactions",
           type != "biogeography",
           type != "antagonism",
           type != "mutualism") %>%
    group_by(type) %>%
    sample_n(N_FIT) %>% 
    ungroup()

#### run the PCA
pca_df <- to_fit_df %>% transmute_(.dots=Metrics)
pca_results <- prcomp(pca_df, center=TRUE, scale=TRUE)

#### plot the fit data
g_fit <- ggbiplot(pca_results, groups=to_fit_df$type, subgroups=to_fit_df$type, ellipse=TRUE, var.axes=FALSE) +
    scale_colour_manual(name="Network Type", values=cols) + scale_shape_manual(name="Network Type", values=shps) +
    coord_cartesian(xlim=c(-3, 2.05), ylim=c(-3.15, 1.4)) +
    theme(legend.position="bottom", legend.title=element_blank())

#### plot the test data
## get the coordinates
to_test_df <- results_df %>%
    filter(randomization == "None",
           # type != "actor collaboration",
           type != "ecological interactions",
           type != "biogeography",
           type != "antagonism",
           type != "mutualism") %>%
    anti_join(to_fit_df) %>%
    mutate_(.dots=Metrics) %>%
    ## center and scale according to the fitting data
    mutate_at(names(Metrics),
              funs((. - (pca_df %>% summarise_all(mean))$.) /
                       (pca_df %>% summarise_all(sd))$.))
## transform into PCA space
to_test_df <- to_test_df %>%
    rowwise() %>%
    ## the ggbiplot code divides the coordinates by the standard deviation
    ## in the pca results. I don't know why, but to make the points line up
    ## with the ellipses, we do so here as well.
    do(as.numeric(.[names(Metrics)]) %*% pca_results$rotation %>%
           sweep(2, pca_results$sdev, '/') %>% 
           tbl_df()) %>% 
    ungroup() %>%
    bind_cols(to_test_df)

ggsave(g_fit,
       width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/Fit_", str_replace(basename(results_file), "\\.csv", ".svg")))
ggsave((g_fit %>% remove_geom(c("GeomPoint", "GeomPath"))) + 
           geom_point(aes(x=PC1, y=PC2, colour=type, shape=type), data=to_test_df) +
           coord_cartesian(xlim=c(-3,2.05), ylim=c(-3.15, 1.4)) +
           theme(panel.grid=element_blank()),
       width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/Test_", str_replace(basename(results_file), "\\.csv", ".svg")))

#### plot the randomized webs
## get the coordinates
full_df <- results_df %>%
    filter(#type != "actor collaboration",
           type != "ecological interactions",
           type != "biogeography",
           type != "antagonism",
           type != "mutualism") %>%
    mutate_(.dots=Metrics) %>%
    ## center and scale according to the fitting data
    mutate_at(names(Metrics),
              funs((. - (pca_df %>% summarise_all(mean))$.) /
                       (pca_df %>% summarise_all(sd))$.))
## transform into PCA space
full_df <- full_df %>%
    rowwise() %>%
    ## the ggbiplot code divides the coordinates by the standard deviation
    ## in the pca results. I don't know why, but to make the points line up
    ## with the ellipses, we do so here as well.
    do(as.numeric(.[names(Metrics)]) %*% pca_results$rotation %>%
           sweep(2, pca_results$sdev, '/') %>% 
           tbl_df()) %>%
    ungroup() %>%
    bind_cols(full_df)
## order for nicer plotting
full_df$randomization <- factor(full_df$randomization,
                                levels=c("None", "Erdos-Renyi", "Configuration model"),
                                labels=c("Empirical", "Erdos-Renyi", "Configuration"))
## remove the fitting data points before adding the new ones
ggsave(g_fit %>% remove_geom("GeomPoint") +
           geom_point(aes(x=PC1, y=PC2, colour=type, shape=type), size=2, data=full_df %>% filter(randomization == "Empirical")) +
           coord_cartesian(xlim=c(-3, 2.05), ylim=c(-3.15, 1.5)) +
           theme(legend.position="bottom",
                 legend.title=element_blank()),
       width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/Empirical_", str_replace(basename(results_file), "\\.csv", ".svg")))
ggsave(g_fit %>% remove_geom("GeomPoint") +
           geom_point(aes(x=PC1, y=PC2, colour=type, shape=type), size=2, data=full_df %>% filter(randomization == "Erdos-Renyi")) +
           coord_cartesian(xlim=c(-3, 2.05), ylim=c(-3.15, 1.5)) +
           theme(legend.position="bottom",
                 legend.title=element_blank()),
       width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/ER-Rand_", str_replace(basename(results_file), "\\.csv", ".svg")))
ggsave(g_fit %>% remove_geom("GeomPoint") +
           geom_point(aes(x=PC1, y=PC2, colour=type, shape=type), size=2, data=full_df %>% filter(randomization == "Configuration")) +
           coord_cartesian(xlim=c(-3, 2.05), ylim=c(-3.15, 1.5)) +
           theme(legend.position="bottom",
                 legend.title=element_blank()),
       width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/CM-Rand_", str_replace(basename(results_file), "\\.csv", ".svg")))

#### empirical overlay
emp_df <- results_df %>%
    filter(randomization == "None",
           type == "ecological interactions" | type == "biogeography") %>%
    mutate(type = tolower(feature1)) %>%
    mutate(randomization = "Empirical") %>%
    mutate_(.dots=Metrics) %>%
    ## center and scale according to the fitting data
    mutate_at(names(Metrics),
              funs((. - (pca_df %>% summarise_all(mean))$.) /
                       (pca_df %>% summarise_all(sd))$.))
## transform into PCA space
emp_df <- emp_df %>%
    rowwise() %>%
    ## the ggbiplot code divides the coordinates by the standard deviation
    ## in the pca results. I don't know why, but to make the points line up
    ## with the ellipses, we do so here as well.
    do(as.numeric(.[names(Metrics)]) %*% pca_results$rotation %>%
           sweep(2, pca_results$sdev, '/') %>% 
           tbl_df()) %>%
    ungroup() %>%
    bind_cols(emp_df)

## convert type to show relevant info (chimera of feature 1 and type)
emp_df <- emp_df %>% mutate(type=str_replace(type, "fungi|islands|mountains", "biogeography")) %>%
    ## remove an outlier
    filter(name != "Kaiser-Bunbury_2014_M_PL_061_17")

## remove the fitting data points before adding the new ones
emp_legend <- (ggplot(emp_df) +
                   geom_point(aes(x=PC1, y=PC2, colour=type, shape=type), size=2, data=emp_df) +
                   geom_path(aes(x=PC1, y=PC2, color=type, group=type), data=emp_df %>%
                                 group_by(type) %>%
                                 do(ellipse_path(.)) %>% 
                                 ungroup()) +
                   scale_shape_manual(values=c("antagonism"    = 4,
                                               "mutualism"     = 3,
                                               "biogeography"  = 1)) +
                   scale_colour_manual(values=c("antagonism"   = "#e07699",
                                                "mutualism"    = "#95a7d5",
                                                "biogeography" = "#79c8a5")) +
                   theme(legend.position="bottom",
                         legend.title=element_blank())) %>%
    extract_legend()
g_emp <- g_fit %>% remove_geom("GeomPoint") +
    geom_point(aes(x=PC1, y=PC2, colour=type, shape=type), size=2, data=emp_df) +
    geom_path(aes(x=PC1, y=PC2, color=type, group=type), data=emp_df %>%
                  group_by(type) %>%
                  do(ellipse_path(.)) %>% 
                  ungroup()) +
    coord_cartesian(xlim=c(-3, 2.05), ylim=c(-3.15, 1.5)) +
    scale_colour_manual(values=c("crimes"                  = "grey70",
                                 "ecological interactions" = "grey70",
                                 "antagonism"              = "#e07699",
                                 "mutualism"               = "#95a7d5",
                                 "microbiome"              = "grey70",
                                 "actor collaboration"     = "grey70",
                                 "biogeography"            = "#79c8a5",
                                 "legislature"             = "grey70",
                                 "authorship"              = "grey70"), guide=FALSE) +
    theme(legend.position="bottom",
          legend.title=element_blank())
ggsave(g_emp %>% replace_legend(emp_legend), width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/EmpOverlay_", str_replace(basename(results_file), "\\.csv", ".svg")))

### crime focal plot
city_legend <- (ggplot(full_df) +
                    geom_point(aes(x=PC1, y=PC2, colour=feature1, shape=feature1), size=2, 
                               data=full_df %>% filter(randomization == "Empirical", type == "crimes")) +
                    geom_path(aes(x=PC1, y=PC2, color=feature1, group=feature1),
                              data=full_df %>%
                                  filter(randomization == "Empirical", type == "crimes") %>%
                                  group_by(feature1) %>%
                                  do(., ellipse_path(.))) +
                    facet_wrap(~randomization, ncol=1) +
                    scale_shape_manual(name="City",
                                       values=c("Chicago"                 = 0,
                                                "Denver"                  = 1,
                                                "Minneapolis"             = 2,
                                                "San Francisco"           = 5,
                                                "Washington DC"           = 6)) +
                    scale_colour_manual(name="City",
                                        values=c("Chicago"                = "#95a7d5",
                                                 "Denver"                 = "#82d255",
                                                 "Minneapolis"            = "#c86ede",
                                                 "San Francisco"          = "#ccba4e",
                                                 "Washington DC"          = "#e37843")) +
                    theme(legend.position="bottom",
                          legend.title=element_blank())) %>%
    extract_legend()

tmp <- g_fit %>% remove_geom("GeomPoint") +
    geom_point(aes(x=PC1, y=PC2, colour=feature1, shape=feature1), alpha=0.75, size=2, 
               data=full_df %>% filter(randomization == "Empirical", type == "crimes")) +
    geom_path(aes(x=PC1, y=PC2, color=feature1, group=feature1), data=full_df %>%
                  filter(randomization == "Empirical", type == "crimes") %>%
                  group_by(feature1) %>%
                  do(ellipse_path(.)) %>% 
                  ungroup()) +
    scale_shape_manual(values=c("Chicago"                 = 0,
                                "Denver"                  = 1,
                                "Minneapolis"             = 2,
                                "San Francisco"           = 5,
                                "Washington DC"           = 6)) +
    scale_colour_manual(values=c("crimes"                  = "grey70",
                                 "ecological interactions" = "grey70",
                                 "microbiome"              = "grey70",
                                 "actor collaboration"     = "grey70",
                                 "biogeography"            = "grey70",
                                 "legislature"             = "grey70",
                                 "authorship"              = "grey70",
                                 "Chicago"                 = "#95a7d5",
                                 "Denver"                  = "#82d255",
                                 "Minneapolis"             = "#c86ede",
                                 "San Francisco"           = "#ccba4e",
                                 "Washington DC"           = "#e37843"), guide=FALSE) +
    coord_cartesian(xlim=c(-1.26,1.26), ylim=c(-1.425,0.9)) +
    theme(legend.position="bottom",
          legend.title=element_blank())
ggsave(tmp %>% replace_legend(city_legend), width=BIPLOT_WIDTH, height=BIPLOT_HEIGHT, bg="transparent",
       filename=str_c("../Figures/CrimeFocus_", str_replace(basename(results_file), "\\.csv", ".svg")))

