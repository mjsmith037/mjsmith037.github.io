rm(list=ls())
library(gridExtra)
library(tidyverse)
library(stringr)
library(mnormt)

id_to_axis <- function(id_vect) {
    id_vect %>% str_extract("\\d+") %>% as.integer() %>% `/`(max(.))
}
mat_to_df <- function(mat) {
    mat %>%
        as_data_frame() %>%
        mutate(one = colnames(.) %>% id_to_axis()) %>%
        gather("two", "overlap", -one) %>%
        mutate(two = two %>% id_to_axis(),
               overlap = overlap / max(overlap))
}
projmat_plot <- function(B) {
    # bind_rows((B %*% t(B)) %>% mat_to_df() %>% mutate(orientation = "species"),
    #           (t(B) %*% B) %>% mat_to_df() %>% mutate(orientation = "sites")) %>%
    (B %*% t(B)) %>% mat_to_df() %>% mutate(orientation = "species") %>%
        ggplot() +
        aes(x=one, y=-1 * two, fill=overlap) +
        geom_raster(show.legend=FALSE) +
        facet_grid(str_c("Projection Matrices")~orientation) +
        scale_fill_distiller(palette = "Spectral") +
        scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
        ylab("") +
        theme_bw() +
        theme(axis.text=element_text(colour=NA),
              axis.ticks=element_blank(),
              axis.title.x=element_blank())
}
run_pca <- function(mat) {
    prcomp(mat, center=TRUE, scale.=TRUE) %>%
        .$rotation %>%
        .[,1:2] %>%
        as_data_frame()
}
pca_plot <- function(B) {
    # bind_rows(run_pca(B %*% t(B)) %>% mutate(orientation = "species"),
    #           run_pca(t(B) %*% B) %>% mutate(orientation = "sites")) %>%
    run_pca(B %*% t(B)) %>% mutate(orientation = "species") %>%
        ggplot() +
        aes(x=PC1, y=PC2) +
        geom_point() +
        facet_grid(str_c("PCA")~orientation, scales="free") +
        coord_equal() +
        theme_bw()
}
get_n_eigenvectors <- function(mat, n_vecs) {
    eigen(mat, symmetric=TRUE)$vectors[,1:n_vecs] %>%
        as_data_frame() %>%
        rownames_to_column() %>%
        gather("vector", "value", -rowname) %>%
        mutate(rowname=as.integer(rowname))
}
eigvect_plot <- function(B, n_vecs=5) {
    # bind_rows(get_n_eigenvectors(B %*% t(B), n_vecs) %>% mutate(orientation="species"),
    #           get_n_eigenvectors(t(B) %*% B, n_vecs) %>% mutate(orientation="sites")) %>%
    get_n_eigenvectors(B %*% t(B), n_vecs) %>% mutate(orientation="species") %>%
        ggplot() +
        aes(x=rowname, y=value) +
        geom_point() +
        facet_grid(vector~orientation, scales="free") +
        theme_bw() +
        theme(axis.title.x=element_blank())
}
full_diagnostic_plot <- function(B, filename=NULL, ww=8, hh=3) {
    p <- arrangeGrob(projmat_plot(B), pca_plot(B), eigvect_plot(B),
                     layout_matrix=matrix(c(1,2,3,3),1,4))
    if (!is.null(filename)) ggsave(p, file=filename, width=ww, height=hh)
    return(p)
}

equal_spacing <- function(n, first, last) seq(first, last, length.out=n+1) %>% head(-1)
constant <- function(n, value, filler_variable) rep(value, n)
alt_exp <- function(x, rate, base) rate * base^(-rate * x)
logistic <- function(x, transition, k) -1 / (1 + exp(-k * (x - transition)))

#### functions to produce probabilistic occurance matrices ####
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
    #### PARAMETER SPECIFICATION ####
    outfile_base <- str_c(d, "d_", edge_behavior)
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
    species_dist_para_2 <- lapply(1:n_species, function(ii, param) {
        param
    }, param=sd_para_2)
    
    ## turn probabilities into binary occurance matrix ####
    prob_func <- ifelse(edge_behavior == "bounded",
                        "build_bounded_probabilitymatrix",
                        str_c("build_", d, "d_", edge_behavior, "_probabilitymatrix"))
    prob_observed <- get(prob_func)(n_species, species_dfuncs, species_dist_para_1, species_dist_para_2, site_locations)
    
    occurance_matrix <- do.call(rbind, prob_observed) %>%
        ## normalize densities to have maximum of 1 and minimum of 0
        apply(1, function(row) (row - min(row)) / max(row - min(row))) %>%
        ## apply by row transposes, so transpose to reverse
        t() %>%
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
    
    ## normalize by column
    B <- occurance_matrix %>% apply(2, function(col) (col - mean(col)) / sd(col))
    return(B)
}

B <- make_mat("bounded", 1, 0, 200, 500, "runif", 0, 1, "runif", 0, 1, "dmnorm", "SC", 0.1)
p <- full_diagnostic_plot(B, "../Figures/1d_bounded_0.svg", 10, 3)


