source("plotting_functions.R")

library(vegan)
library(tidyverse)
library(stringr)
library(mnormt)

normalize <- function(mat) apply(mat, 2, function(col) (col - mean(col)) / sd(col))

equal_spacing <- function(n, first, last) seq(first, last, length.out=n+1) %>% head(-1)
constant <- function(n, value, filler_variable) rep(value, n)
alt_exp <- function(x, rate, base) rate * base^(-rate * x)
logistic <- function(x, transition, k) 1 / (1 + exp(-k * (x - transition)))
normalized_normal <- function(nn, mu, sig) rnorm(nn, mu, sig) %>% `-`(min(.)) %>% `/`(max(.))

## functions to produce probabilistic occurance matrices ####
build_bounded_probabilitymatrix <- function(n_species, species_dfuncs, species_dist_para_1, species_dist_para_2, site_locations) {
    species_distributions <- lapply(1:n_species, function(ii) {
        return(function(xx) {
            species_dfuncs[[ii]](xx, species_dist_para_1[ii], species_dist_para_2[[ii]])
        })
    })
    prob_observed <- lapply(species_distributions, function(spp_dist) return(spp_dist(site_locations) %>% t))
    return(prob_observed)
}
build_1d_looping_probabilitymatrix <- function(n_species, species_dfuncs, species_dist_para_1, species_dist_para_2, site_locations) {
    ## for looping geometries, it makes more sense to center the distributions
    ## at zero and shift them later
    species_distributions <- lapply(1:n_species, function(ii) {
        return(function(xx) {
            species_dfuncs[[ii]](xx, 0, species_dist_para_2[[ii]])
        })
    })
    prob_observed <- lapply(1:n_species, function(ii) {
        ## we pick the density associated with the minimum distance to the
        ## distribution center for convenience
        species_distributions[[ii]](pmin(abs(site_locations - species_dist_para_1[ii]),
                                         1 - abs(site_locations - species_dist_para_1[ii]))) %>% t
    })
    return(prob_observed)
}
build_2d_looping_probabilitymatrix <- function(n_species, species_dfuncs, species_dist_para_1, species_dist_para_2, site_locations) {
    species_distributions <- lapply(1:n_species, function(ii) {
        return(function(xx) {
            species_dfuncs[[ii]](xx, rep(0, 2), species_dist_para_2[[ii]])
        })
    })
    prob_observed <- lapply(1:n_species, function(ii) {
        ## we pick the density associated with the minimum distance to the
        ## distribution center for convenience
        species_distributions[[ii]](
            site_locations %>%
                as_data_frame() %>%
                mutate(x_dist = V1 - species_dist_para_1[ii,1],
                       y_dist = V2 - species_dist_para_1[ii,2]) %>%
                rowwise() %>%
                transmute(min_x_dist = c(x_dist, 1 - abs(x_dist))[which.min(abs(c(x_dist, 1 - abs(x_dist))))],
                          min_y_dist = c(y_dist, 1 - abs(y_dist))[which.min(abs(c(y_dist, 1 - abs(y_dist))))]) %>%
                as.matrix()
        )
    })
    return(prob_observed)
}
build_3d_looping_probabilitymatrix <- function(n_species, species_dfuncs, species_dist_para_1, species_dist_para_2, site_locations) {
    species_distributions <- lapply(1:n_species, function(ii) {
        return(function(xx) {
            species_dfuncs[[ii]](xx, rep(0, 3), species_dist_para_2[[ii]])
        })
    })
    prob_observed <- lapply(1:n_species, function(ii) {
        ## we pick the density associated with the minimum distance to the
        ## distribution center for convenience
        species_distributions[[ii]](
            site_locations %>%
                as_data_frame() %>%
                mutate(x_dist = V1 - species_dist_para_1[ii,1],
                       y_dist = V2 - species_dist_para_1[ii,2],
                       z_dist = V3 - species_dist_para_1[ii,3]) %>%
                rowwise() %>%
                transmute(min_x_dist = c(x_dist, 1 - abs(x_dist))[which.min(abs(c(x_dist, 1 - abs(x_dist))))],
                          min_y_dist = c(y_dist, 1 - abs(y_dist))[which.min(abs(c(y_dist, 1 - abs(y_dist))))],
                          min_z_dist = c(z_dist, 1 - abs(z_dist))[which.min(abs(c(z_dist, 1 - abs(z_dist))))]) %>%
                as.matrix()
        )
    })
    return(prob_observed)
}
build_4d_looping_probabilitymatrix <- function(n_species, species_dfuncs, species_dist_para_1, species_dist_para_2, site_locations) {
    species_distributions <- lapply(1:n_species, function(ii) {
        return(function(xx) {
            species_dfuncs[[ii]](xx, rep(0, 4), species_dist_para_2[[ii]])
        })
    })
    prob_observed <- lapply(1:n_species, function(ii) {
        ## we pick the density associated with the minimum distance to the
        ## distribution center for convenience
        species_distributions[[ii]](
            site_locations %>%
                as_data_frame() %>%
                mutate(w_dist = V1 - species_dist_para_1[ii,1],
                       x_dist = V2 - species_dist_para_1[ii,2],
                       y_dist = V3 - species_dist_para_1[ii,3],
                       z_dist = V4 - species_dist_para_1[ii,4]) %>%
                rowwise() %>%
                transmute(min_w_dist = c(w_dist, 1 - abs(w_dist))[which.min(abs(c(w_dist, 1 - abs(w_dist))))],
                          min_x_dist = c(x_dist, 1 - abs(x_dist))[which.min(abs(c(x_dist, 1 - abs(x_dist))))],
                          min_y_dist = c(y_dist, 1 - abs(y_dist))[which.min(abs(c(y_dist, 1 - abs(y_dist))))],
                          min_z_dist = c(z_dist, 1 - abs(z_dist))[which.min(abs(c(z_dist, 1 - abs(z_dist))))]) %>%
                as.matrix()
        )
    })
    return(prob_observed)
}

## function to recursively remove full/empty rows/cols as these contain no information
remove_uninformative_nodes <- function(mat) {
    sub_mat <- mat
    n_uninformative <- sum((rowSums(sub_mat) == 0) | (rowSums(sub_mat) == ncol(sub_mat))) +
        sum((colSums(sub_mat) == 0) | (colSums(sub_mat) == nrow(sub_mat)))
    while (n_uninformative != 0) {
        sub_mat <- sub_mat[((rowSums(sub_mat) != 0) & (rowSums(sub_mat) != ncol(sub_mat))),]
        sub_mat <- sub_mat[,((colSums(sub_mat) != 0) & (colSums(sub_mat) != nrow(sub_mat)))]
        n_uninformative <- sum((rowSums(sub_mat) == 0) | (rowSums(sub_mat) == ncol(sub_mat))) +
            sum((colSums(sub_mat) == 0) | (colSums(sub_mat) == nrow(sub_mat)))
    }
    if ((nrow(sub_mat) != nrow(mat)) | (ncol(sub_mat) != ncol(mat))) {
        warning("REDUCING MATRIX SIZE! now only ", nrow(sub_mat), " by ",
                ncol(sub_mat), " (was ", nrow(mat), " by ", ncol(mat), ")\n")
    }
    return(sub_mat)
}
make_mat <- function(edge_behavior, d, rseed, n_sites, n_species,
                     site_location_dist, si_para_1, si_para_2,
                     species_center_dist, sp_para_1, sp_para_2,
                     species_dist, sd_para_1, sd_para_2) {
    set.seed(rseed)
    ## number and distribution of sites
    site_para_1 <- rep(si_para_1, d)
    site_para_2 <- rep(si_para_2, d)
    site_locations <- 1:d %>%
        lapply(function(ii) get(site_location_dist)(n_sites, site_para_1[ii], site_para_2[ii])) %>%
        unlist() %>% matrix(n_sites, d)
    
    ## number and distribution of species centers
    species_para_1 <- rep(sp_para_1, d)
    species_para_2 <- rep(sp_para_2, d)
    species_centers <- 1:d %>%
        lapply(function(ii) get(species_center_dist)(n_species, species_para_1[ii], species_para_2[ii])) %>%
        unlist() %>% matrix(n_species, d)
    
    ## distributions of species around centers (each species could have its own density kernel)
    species_dfuncs <- lapply(1:n_species, function(xx) get(species_dist))
    ifelse(sd_para_1 == "SC", species_dist_para_1 <- species_centers, species_dist_para_1 <- sd_para_1)
    species_dist_para_2 <- lapply(1:n_species, function(ii, param) {return(param)}, param=sd_para_2)
    
    ## turn probabilities into binary occurance matrix ####
    prob_func <- ifelse(edge_behavior == "bounded",
                        "build_bounded_probabilitymatrix",
                        str_c("build_", d, "d_", edge_behavior, "_probabilitymatrix"))
    prob_observed <- get(prob_func)(n_species, species_dfuncs, species_dist_para_1,
                                    species_dist_para_2, site_locations)
    
    occurance_matrix <- do.call(rbind, prob_observed) %>%
        ## normalize densities to have maximum of 1 and minimum of 0
        # apply(1, function(row) (row - min(row)) / max(row - min(row))) %>%
        # ## apply by row transposes, so transpose to reverse
        # t() %>%
        ## if density is larger than uniform random variable, then count as observed
        `>`(matrix(runif(n_species * n_sites), n_species, n_sites)) %>%
        ## alternatively, if density is above threshold, then count as observed
        # `>`(0.4) %>%
        ## make integer binary
        `*`(1) %>%
        ## order according to known dimensions
        .[order(species_centers[,1]), order(site_locations[,1])] %>%
        ## remove full/empty rows/cols
        remove_uninformative_nodes()
    return(occurance_matrix)
}

#### STANDARD FULL DIAGNOSTIC PLOTS ####
## 1d very large
occ_mat <- make_mat("bounded", 1, 0, 1000, 1500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_bounded_large.svg", 16.5, 4)

occ_mat <- make_mat("looping", 1, 0, 1000, 1500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_looping_large.svg", 16.5, 4)

## 1d standard
occ_mat <- make_mat("bounded", 1, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_bounded.svg", 16.5, 4)

occ_mat <- make_mat("looping", 1, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_looping.svg", 16.5, 4)

## 2d standard
occ_mat <- make_mat("bounded", 2, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(2) * 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/2d_bounded.svg", 16.5, 4)

occ_mat <- make_mat("looping", 2, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(2) * 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/2d_looping.svg", 16.5, 4)

## high d
occ_mat <- make_mat("looping", 3, 0, 1000, 2500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(3) * 0.01)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/3d_bounded.svg", 16.5, 4)

occ_mat <- make_mat("looping", 4, 0, 1000, 2500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(4) * 0.01)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/4d_bounded.svg", 16.5, 4)

## high var
occ_mat <- make_mat("bounded", 1, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.2)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_bounded_midvar.svg", 16.5, 4)

occ_mat <- make_mat("looping", 2, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(2) * 0.2)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_looping_midvar.svg", 16.5, 4)

occ_mat <- make_mat("bounded", 1, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.5)
image(occ_mat)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_bounded_highvar.svg", 16.5, 4)

occ_mat <- make_mat("looping", 2, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(2) * 0.5)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_looping_highvar.svg", 16.5, 4)

## species distributions clustered at mid-elevation
occ_mat <- make_mat("bounded", 1, 0, 200, 500, "runif", 0, 1, "normalized_normal", 0, 1, "dmnorm", "SC", 0.1)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/1d_bounded_midelev.svg", 16.5, 4)

#### TOEPLITZ, CIRCULANT ####
t_mat <- toeplitz(500:0) %>% `/`(500) %>% mat_to_df() %>%
    ggplot() +
    aes(x=one, y=-1 * two, fill=overlap) +
    geom_raster() +
    scale_fill_distiller(palette = "Spectral") +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    ylab("") +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          legend.title=element_blank())
panel_height <- unit(1,"npc") - sum(ggplotGrob(t_mat)[["heights"]][-3])
t_mat + guides(fill=guide_colorbar(barheight=panel_height))
ggsave(filename="../Figures/toeplitz.svg", width=7, height=7)


c_mat <- toeplitz(c(250:0, 1:250)) %>% `/`(501) %>% mat_to_df() %>%
    ggplot() +
    aes(x=one, y=-1 * two, fill=overlap) +
    geom_raster() +
    scale_fill_distiller(palette = "Spectral") +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    ylab("") +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          legend.title=element_blank())
panel_height <- unit(1,"npc") - sum(ggplotGrob(c_mat)[["heights"]][-3])
c_mat + guides(fill=guide_colorbar(barheight=panel_height))
ggsave(filename="../Figures/circulant.svg", width=7, height=7)

#### HIGHER D TABLE PLOTS ####
occ_mat <- make_mat("looping", 3, 0, 1000, 2500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(3) * 0.01)
p <- occ_mat %>% normalize() %>% pca_table_plot() %>% ggsave(filename="../Figures/3d_pca_table.png", width=12, height=12)

occ_mat <- make_mat("looping", 4, 0, 1000, 2500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", diag(4) * 0.01)
p <- occ_mat %>% normalize() %>% pca_table_plot() %>% ggsave(filename="../Figures/4d_pca_table.png", width=12, height=12)

#### EXAMPLE MATRICES ####
occ_mat <- make_mat("bounded", 1, 0, 100, 200, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
occ_mat %>%
    as_data_frame() %>%
    rownames_to_column("one") %>%
    gather("two", "overlap", -one) %>%
    mutate(one = as.integer(one) / max(as.integer(one)),
           two = two %>% id_to_axis(),
           overlap = as.logical(overlap)) %>%
    ggplot() +
    aes(x=one, y=-1 * two, fill=overlap) +
    geom_raster() +
    scale_fill_manual(values=c("white", "#9e0142")) +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    ylab("Sites") + xlab("Species") +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(size=24),
          legend.position="none")
ggsave(filename="../Figures/occurance.svg", width=10, height=5)

set.seed(0)
scrambled_occ_mat <- occ_mat[sample(nrow(occ_mat)),]
scrambled_occ_mat %>%
    as_data_frame() %>%
    rownames_to_column("one") %>%
    gather("two", "overlap", -one) %>%
    mutate(one = as.integer(one) / max(as.integer(one)),
           two = two %>% id_to_axis(),
           overlap = as.logical(overlap)) %>%
    ggplot() +
    aes(x=one, y=-1 * two, fill=overlap) +
    geom_raster() +
    scale_fill_manual(values=c("white", "#9e0142")) +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    ylab("Sites") + xlab("Species") +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(size=24),
          legend.position="none")
ggsave(filename="../Figures/scrambled_occurance.svg", width=10, height=5)

scrambled_occ_mat %>%
    prcomp(center=TRUE, scale.=TRUE) %>%
    .$rotation %>%
    .[,1:2] %>%
    as_data_frame() %>%
    ggplot() +
    aes(x=PC1, y=PC2) +
    geom_point(size=0.5) +
    facet_grid(.~str_c("Principal Component Analysis"), scales="free") +
    coord_equal() +
    theme_bw() +
    theme(axis.text=element_blank())
ggsave(filename="../Figures/raw_presabs_pca.svg", width=4, height=4.25)

scrambled_occ_mat %>% normalize() %>% projmat_plot()
ggsave(filename="../Figures/scrambled_distance_matrix.svg", width=4, height=4)

occ_mat %>% normalize() %>% projmat_plot()
ggsave(filename="../Figures/distance_matrix.svg", width=4, height=4)

occ_mat %>% normalize() %>% pca_plot()
ggsave(filename="../Figures/pca_standalone.svg", width=4, height=4.25)

#### NESTEDNESS ####
occ_mat <- make_mat("bounded", 1, 0, 1000, 2500, "runif", 0, 1, "constant", 0, NA, "logistic", seq(1, 0.1,length.out=2500), 15)
occ_mat %>%
    as_data_frame() %>%
    rownames_to_column("one") %>%
    gather("two", "overlap", -one) %>%
    mutate(one = as.integer(one) / max(as.integer(one)),
           two = two %>% id_to_axis(),
           overlap = as.logical(overlap)) %>%
    ggplot() +
    aes(x=one, y=-1 * two, fill=overlap) +
    geom_raster() +
    scale_fill_manual(values=c("white", "#9e0142")) +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    ylab("Sites") + xlab("Species") +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(size=24),
          legend.position="none")
ggsave(filename="../Figures/nested_occurance.svg", width=10, height=5)
p <- occ_mat %>% normalize() %>% full_diagnostic_plot("../Figures/nested_fullplot.png", 16.5, 4)

#### ALTERNATIVE METHODS ####
occ_mat <- make_mat("bounded", 1, 2, 500, 750, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
lapply(c("bray", "manhattan", "mountford", "raup", "kulczynski", "binomial"), function(meth) {
    vegdist(occ_mat, method=meth, binary=TRUE) %>%
        prcomp(center=TRUE, scale.=TRUE) %>%
        .$rotation %>%
        .[,1:2] %>%
        as_data_frame() %>%
        mutate(dist_metric = meth)
}) %>%
    bind_rows() %>%
    ggplot() +
    aes(x=PC1, y=PC2) +
    geom_point(size=0.5) +
    facet_wrap(~dist_metric, scales="free") +
    coord_equal() +
    theme_bw() +
    theme(axis.text=element_blank())
ggsave(filename="../Figures/alternative_distance_metrics.svg", width=12.5, height=9)

x <- occ_mat %>% normalize() %>%
    `%*%`(t(.)) %>%
    metaMDS(trymax=50, k=2, parallel=8, center=TRUE)

y_comm <- vegdist(occ_mat, method="bray", binary=TRUE)
y <- metaMDS(y_comm, trymax=100, k=2, parallel=8, center=TRUE, previous.best=y)

#### SIMULATION FRAMEWORK ####
make_sim_framework_figure <- function(edge_behavior, d, rseed, n_sites, n_species,
                                      site_location_dist, si_para_1, si_para_2,
                                      species_center_dist, sp_para_1, sp_para_2,
                                      species_dist, sd_para_1, sd_para_2) {
    set.seed(rseed)
    ## number and distribution of sites
    site_para_1 <- rep(si_para_1, d)
    site_para_2 <- rep(si_para_2, d)
    site_locations <- 1:d %>%
        lapply(function(ii) get(site_location_dist)(n_sites, site_para_1[ii], site_para_2[ii])) %>%
        unlist() %>% matrix(n_sites, d)
    
    ## number and distribution of species centers
    species_para_1 <- rep(sp_para_1, d)
    species_para_2 <- rep(sp_para_2, d)
    species_centers <- 1:d %>%
        lapply(function(ii) get(species_center_dist)(n_species, species_para_1[ii], species_para_2[ii])) %>%
        unlist() %>% matrix(n_species, d)
    
    ## distributions of species around centers (each species could have its own density kernel)
    species_dfuncs <- lapply(1:n_species, function(xx) get(species_dist))
    ifelse(sd_para_1 == "SC", species_dist_para_1 <- species_centers, species_dist_para_1 <- sd_para_1)
    species_dist_para_2 <- lapply(1:n_species, function(ii, param) {return(param)}, param=sd_para_2)
    
    ## turn probabilities into binary occurance matrix ####
    prob_func <- ifelse(edge_behavior == "bounded",
                        "build_bounded_probabilitymatrix",
                        str_c("build_", d, "d_", edge_behavior, "_probabilitymatrix"))
    prob_observed <- get(prob_func)(n_species, species_dfuncs, species_dist_para_1,
                                    species_dist_para_2, site_locations)
    
    plotting_data <- do.call(rbind, prob_observed) %>%
        as_data_frame() %>%
        mutate(sp_cent=as.vector(species_centers)) %>%
        gather("variable", "value", -sp_cent) %>%
        mutate(x=rep(as.vector(site_locations), each=n_species))
    focal_sp <- plotting_data %>% select(sp_cent) %>% distinct() %>% sample_n(5)
    p <- ggplot(plotting_data) +
        geom_path(aes(x=x, y=value, group=sp_cent), alpha=0.1) +
        geom_path(aes(x=x, y=value, colour=as.factor(sp_cent), group=sp_cent), size=0.75,
                  data=plotting_data %>% right_join(focal_sp)) +
        geom_vline(aes(xintercept=sp_cent), linetype="dashed", alpha=0.1) +
        geom_vline(aes(xintercept=sp_cent, colour=as.factor(sp_cent)), linetype="dashed",
                   plotting_data %>% right_join(focal_sp)) +
        geom_hline(aes(yintercept=0)) +
        geom_point(aes(x=x, y=0), size=4, alpha=0.1, data=plotting_data %>% select(x) %>% distinct() %>% sample_n(100)) +
        geom_point(aes(x=x, y=0), size=4, data=plotting_data %>% select(x) %>% distinct() %>% sample_n(10)) +
        scale_x_continuous(expand=c(0,0)) +
        ylab("Probability") +
        theme(legend.position="none",
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank(),
              panel.border=element_blank())
    ggsave(filename="../Figures/simulation_framework.svg", width=7,height=3)
    return(p)
}
p <- make_sim_framework_figure("bounded", 1, 0, 1000, 100, "equal_spacing", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)

