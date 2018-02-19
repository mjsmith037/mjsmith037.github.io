library(gridExtra)
library(tidyverse)
library(stringr)

theme_set(theme_bw())

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
    mat_to_df(B %*% t(B)) %>%
        ggplot() +
        aes(x=one, y=-1 * two, fill=overlap) +
        geom_raster(show.legend=FALSE) +
        facet_grid(.~str_c("Projection Matrix (\"Sites in Common\")")) +
        scale_fill_distiller(palette = "Spectral") +
        scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
        ylab("Species") + xlab("Species") +
        theme(axis.text=element_blank(),
              axis.ticks=element_blank())
}
run_pca <- function(mat, n_pcs=2) {
    prcomp(mat, center=TRUE, scale.=TRUE) %>%
        .$rotation %>%
        .[,1:n_pcs] %>%
        as_data_frame()
}
pca_plot <- function(B) {
    run_pca(B %*% t(B)) %>%
        ggplot() +
        aes(x=PC1, y=PC2) +
        geom_point(size=0.5) +
        facet_grid(.~str_c("Principal Component Analysis"), scales="free") +
        coord_equal() +
        theme(axis.text=element_blank())
}
get_n_eigenvectors <- function(mat, n_vecs) {
    eigen(mat, symmetric=TRUE)$vectors[,1:n_vecs] %>%
        as_data_frame() %>%
        rownames_to_column() %>%
        gather("vector", "value", -rowname) %>%
        mutate(rowname=as.integer(rowname))
}
eigvect_plot <- function(B, n_vecs=5) {
    get_n_eigenvectors(B %*% t(B), n_vecs) %>%
        ggplot() +
        aes(x=rowname, y=value) +
        geom_point(size=0.5) +
        facet_grid(vector~., scales="free") +
        theme(axis.title.x=element_blank())
}
full_diagnostic_plot <- function(B, filename=NULL, ww=8, hh=3) {
    p <- arrangeGrob(projmat_plot(B), pca_plot(B), eigvect_plot(B),
                     layout_matrix=matrix(c(1,2,3,3),1,4))
    if (!is.null(filename)) ggsave(p, file=filename, width=ww, height=hh)
    return(p)
}

pca_table_plot_data <- function(dat) {
    zzz <- dat %>% gather("Axis", "Value", -orientation)
    zzz %>% group_by(orientation) %>% 
        do(group_by(., Axis) %>%
               do(lapply(unique(zzz$Axis),
                         function(xx) 
                             bind_cols(., filter(zzz,
                                                 orientation == unique(.$orientation),
                                                 Axis == xx) %>%
                                           select(-orientation))) %>%
                      bind_rows()))
}

pca_table_plot <- function(B, n_pcs=4, orient = "rows") {
    if (orient == "rows") {
        pc_res <- run_pca(B %*% t(B), n_pcs)
    } else {
        pc_res <- run_pca(B %*% t(B), n_pcs)
    }
    pc_res %>%
        mutate(orientation = orient) %>% 
        pca_table_plot_data() %>%
        ggplot() +
        aes(x=Value, y=Value1) +
        geom_point(size=0.5, alpha=0.25) +
        facet_grid(Axis~Axis1, scales="free") +
        coord_equal() +
        theme(axis.title = element_blank())
}
