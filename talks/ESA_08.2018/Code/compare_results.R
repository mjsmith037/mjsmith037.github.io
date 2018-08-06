library(stringr)
source("extract_order_prototype.R")

library(tools)
library(parallel)
library(broom)
library(tidyverse)

# matfile_base <- "simmat_1d_bounded_3-MC3_Distance"
# results_dir <- "~/Research/EcologicalLinearity/Results/SimulatedMatrices"
# data_dir <- "~/Research/EcologicalLinearity/Data/SimulatedMatrices"
matfile_base <- "nutnet"
results_dir <- "~/Research/EcologicalLinearity/Results/EmpiricalMatrices"
data_dir <- "~/Research/EcologicalLinearity/Data/EmpiricalMatrices"

comp_dd <- 1

res_files <- list.files(results_dir, matfile_base, full.names=TRUE)
load(res_files[1])
orderings <- mclapply(1:length(res_files), mc.cores=1, function(ii) {
    ## read the file
    load(res_files[ii])
    ## save the ordering
    return(search_results %>% filter(ordering == "new_order") %>% .$position)
})
get_fitnesses <- function(dat) {
    load(str_c(data_dir, "/", str_replace(dat$Web[1], "transposed", ""), ".RData"))
    occurance_matrix <- Data$B
    # occurance_matrix <- B
    B <- occurance_matrix[,apply(occurance_matrix, 2, sd) != 0] %>%
        apply(2, function(col) (col - mean(col)) / sd(col))
    if (unique(dat$R_OR_C) == "1") {
        similarity_distance <- mclapply(dat$ordering, mc.cores=8, function(xx) {
            full_distance(t(occurance_matrix)[xx %>% str_split(" ") %>% unlist %>% as.integer,], comp_dd)
        })
        cosine_lm_likelihood <- mclapply(dat$ordering, mc.cores=8, function(xx) {
            cosine_linear_model(xx %>% str_split(" ") %>% unlist %>% as.integer,
                                eigen(t(B) %*% B)$vectors[,NN])
        })
    } else {
        similarity_distance <- mclapply(dat$ordering, mc.cores=8, function(xx) {
            full_distance(occurance_matrix[xx %>% str_split(" ") %>% unlist %>% as.integer,], comp_dd)
        })
        cosine_lm_likelihood <- mclapply(dat$ordering, mc.cores=8, function(xx) {
            cosine_linear_model(xx %>% str_split(" ") %>% unlist %>% as.integer,
                                eigen(B %*% t(B))$vectors[,NN])
        })
    }
    dat %>%
        mutate(fitness_dist=similarity_distance %>% unlist,
               fitness_lm=cosine_lm_likelihood %>% unlist)
}
result_df <- data_frame(original_file=res_files) %>%
    mutate(file_base = original_file %>% file_path_sans_ext() %>% basename()) %>%
    separate(file_base, c("Web", "Search", "R_OR_C", "Wrapping", "dd",
                          "n_steps", "n_chains", "r_seed"), "-") %>%
    mutate(ordering=lapply(orderings, str_c, collapse=" ") %>% unlist()) %>%
    filter(dd == 0) %>%
    group_by(Web, R_OR_C) %>%
    do(get_fitnesses(.)) %>%
    ungroup()

ggplot(result_df %>%
           gather("variable", "value", fitness_dist, fitness_lm) %>%
           mutate(Wrapping=ifelse(as.integer(Wrapping), "Looping", "Bounded"),
                  R_OR_C=ifelse(as.integer(R_OR_C), "by columns", "by rows"))) +
    aes(x=value, fill=str_c(n_steps, n_chains, Wrapping, dd, sep="_")) +
    geom_dotplot(stackgroups=TRUE, binpositions="all") +
    facet_grid(Web~variable*R_OR_C, scales="free") +
    theme_bw() + theme(legend.position="bottom",
                       legend.title=element_blank())

## note fitnesses are uncorrelated
result_df %>% group_by(Wrapping, R_OR_C) %>% #ggplot() + aes(x=fitness_dist, y=fitness_lm) + geom_point()
    do(bind_cols(lm(.$fitness_dist~.$fitness_lm) %>% tidy() %>% filter(term != "(Intercept)"),
                 cor(.$fitness_dist, .$fitness_lm) %>% tidy() %>% rename(cor_R2=x)))

## comparing best result to original
result_df %>%
    select(-ordering, -original_file) %>%
    group_by(Web, R_OR_C, Search) %>%
    filter(fitness_dist == min(fitness_dist) | fitness_lm == min(fitness_lm))
# full_distance(t(occurance_matrix)[str_c(1:61, " ") %>% str_split(" ") %>% unlist %>% as.integer,], comp_dd)

plot_comparison <- function(dat) {
    load(dat$original_file[1])
    rm(list="full_diagnostic_plot")
    source("plotting_functions.R")
    before <- mat %>% #t() %>%
        # .[,apply(., 2, sd) != 0] %>%
        # apply(2, function(col) (col - mean(col)) / sd(col)) %>% t() %>%
        full_diagnostic_plot(filename="../Figures/Empirical_Before.svg", 16.5, 4)
    after <- mat[,#dat %>% filter(R_OR_C == "0") %>% .$ordering %>% str_split(" ") %>% unlist() %>% as.integer(),#] %>% t() %>%
                 dat %>% filter(R_OR_C == "1") %>% .$ordering %>% str_split(" ") %>% unlist() %>% as.integer()] %>% #t() %>%
        # .[,apply(., 2, sd) != 0] %>%
        # apply(2, function(col) (col - mean(col)) / sd(col)) %>% t() %>%
        full_diagnostic_plot(filename="../Figures/Empirical_After.svg", 16.5, 4)
    return(dat)
}
best_order <- result_df %>%
    group_by(Web) %>%
    arrange(fitness_dist, fitness_lm) %>%
    group_by(R_OR_C) %>%
    do(head(., 1)) %>%
    ungroup() %>%
    do(plot_comparison(.)) %>%
    ungroup()

## does it correlate with initial ordering?
result_df %>%
    group_by(Web, R_OR_C) %>%
    arrange(fitness_dist, fitness_lm) %>%
    do(head(., 1)) %>%
    do(cor(1:MAT_DIMS[as.integer(.$R_OR_C)+1],
           str_split(.$ordering, " ") %>% unlist() %>% as.integer()) %>%
           tidy())

## look at the new ordering (for comparing to known orders)
order_df <- data_frame(n=result_df %>%
                           arrange(fitness_dist, fitness_lm) %>%
                           do(head(., 1)) %>%
                           .$ordering %>% str_split(" ") %>% unlist() %>% as.integer()) %>%
    mutate(o=n():1) ## reverse ordering as necessary to make correlation positive
ggplot(order_df) +
    aes(x=o, y=n, colour=as.factor(o)) +
    geom_point() +
    ylab(str_c("Best Found Ordering (",
               # read_table("~/Research/EcologicalLinearity/Data/SimulatedMatrices/simmat_1d_bounded_3.txt", col_names=FALSE) %>%
               read_table("~/Research/EcologicalLinearity/Code/C/nutnet_GreatPlainsPlus.txt", col_names=FALSE) %>%
                   as.matrix() %>% .[,order_df$n] %>% t() %>% full_distance(comp_dd) %>% format(scientific=FALSE, big.mark=","), ")")) +
    xlab(str_c("Original Ordering (",
               # read_table("~/Research/EcologicalLinearity/Data/SimulatedMatrices/simmat_1d_bounded_3.txt", col_names=FALSE) %>%
               read_table("~/Research/EcologicalLinearity/Code/C/nutnet_GreatPlainsPlus.txt", col_names=FALSE) %>%
                   as.matrix() %>% t() %>% full_distance(comp_dd) %>% format(scientific=FALSE, big.mark=","), ")")) +
    theme_bw() + theme(legend.position="none")
ggsave(filename="../Figures/compare_empirical_orderings.svg", width=7, height=5)
