library(tools)
library(stringr)
library(broom)
library(tidyverse)
library(parallel)

source("plotting_functions.R")

# cargs <- commandArgs(TRUE)
# # cargs <- c("../../Data/SimulatedMatrices/simmat_1d_bounded_0.RData", "hill_climb_distance", 10, 10, 7)
# # cargs <- c("../../Data/SimulatedMatrices/simmat_1d_bounded_10.RData", "hill_climb_lm", 1000, 1000, 14, 0)
# matfile <- cargs[1]
# load(matfile)
# search_function_string <- cargs[2]
# N_STEPS <- as.integer(cargs[3])
# if (is.na(N_STEPS)) N_STEPS <- 1000
# N_TRIES <- as.integer(cargs[4])
# if (is.na(N_TRIES)) N_TRIES <- 100
# N_CORES <- as.integer(cargs[5])
# if (is.na(N_CORES)) N_CORES <- 1
# R_SEED <- as.integer(cargs[6])
# if (is.na(R_SEED)) R_SEED <- 0
# set.seed(R_SEED)

#### Approach 1: linear model fitting of cosine to eigenvectors ####
## vector to fit
NN <<- 2
## fitness
cosine_linear_model <- function(xx, yy, vect_num=NN, just_fit=TRUE) {
    # ssp <- spectrum(yy, plot=FALSE)
    # period <- length(xx) * (vect_num - 1) #1/ssp$freq[ssp$spec==max(ssp$spec)]
    # model <- lm(yy~sin(2 * pi / period * xx) + cos(2 * pi / period * xx))
    # if (just_fit) return(model %>% glance %>% .$r.squared) else return(model)
    model <- NULL
    try(model <- nls(yy~a + b * cos(c * (xx + d)),
                     start=c(a=0, b=yy %>% range %>% diff %>% `/`(2),
                             c=pi / (length(xx) / vect_num), d=0),
                     lower=c(-Inf,  yy %>% range %>% diff %>% `/`(3),
                               0.5 * pi / (length(xx) / vect_num), -pi * length(xx)),
                     upper=c( Inf,  yy %>% range %>% diff,
                               2 * pi / (length(xx) / vect_num),  pi * length(xx)),
                     control=list(maxiter=100, minFactor=1/2^12), algorithm="port"),
        silent=TRUE)
    ## fitness is log likelihood
    if (!is.null(model)) {
        if (just_fit) {
            return(model %>% glance %>% .$logLik)
        } else return(model)
    } else return(NA)
}
## mutation
swap_two_elements <- function(vec) {
    to_switch <- vec %>% length %>% sample(2)
    tmp <- vec[to_switch[1]]
    vec[to_switch[1]] <- vec[to_switch[2]]
    vec[to_switch[2]] <- tmp
    return(vec)
}
## searching
hill_climb_lm <- function(original_ordering, mat,
                          steps=N_STEPS, tries=N_TRIES,
                          fitness_function=cosine_linear_model,
                          mutate_function=swap_two_elements) {
    vector_values <- eigen(mat %*% t(mat))$vectors[,NN]
    steps_left <- steps
    best_ordering <- ordering <- original_ordering
    best_fitness <- fitness <- fitness_function(original_ordering, vector_values)
    ## hill-climb until `steps` steps have passed without improvement
    while (steps_left > 0) {
        ## at each step, try `tries` different mutations and choose the best
        ordering <- mclapply(1:tries, mc.cores=N_CORES, function(ii) mutate_function(ordering)) %>%
            .[[map_dbl(., fitness_function, yy=vector_values) %>% which.max]]
        fitness <- fitness_function(ordering, vector_values)
        if (fitness > best_fitness) {
            ## if the best step improves the fitness, update the records and reset the step count
            cat("\r", fitness)
            best_fitness <- fitness
            best_ordering <- ordering
            steps_left <- steps
            ## otherwise, increment the steps-without-improvement count
        } else steps_left <- steps_left - 1
    }
    return(list(best_ordering))
}
# search_results <- get_n_eigenvectors(B %*% t(B), NN) %>%
#     filter(vector == ifelse(NN==1, "value", str_c("V", NN))) %>%
#     select(old_order = rowname, value) %>%
#     mutate(new_order = hill_climb_lm(old_order, value)) %>%
#     gather("ordering", "position", -value)

fitted_function <- function(xx) {
    model <- cosine_linear_model(search_results %>%
                                     filter(ordering == "new_order") %>%
                                     .$position,
                                 search_results %>%
                                     filter(ordering == "new_order") %>%
                                     .$value,
                                 just_fit=FALSE) %>%
        tidy
    params <- setNames(model$estimate, model$term)
    return(params[['a']] + params[['b']] * cos(params[['c']] * (xx + params[['d']])))
}
# ggplot(search_results) +
#     aes(x=position, y=value) +
#     geom_point() +
#     stat_function(fun=fitted_function) +
#     facet_wrap(~ordering)

#### Approach 2: gap minimization (doesn't find good solutions -- not sure why yet) ####
## tally the number of true/false positives/negatives in a network given a
## matrix of interval starts and ends for each row
tally_score_rowwise <- function(intervals, mat) {
    intervals %>% rowwise() %>% mutate(
        ## true positive: expecting 1 (in interval) and 1 present
        true_pos  = sum(mat[row, start:end]),
        ## false negative: expecting 1 (in interval) yet 0 present
        false_neg = sum(1 - mat[row, start:end]),
        ## false positive: expecting 0 (not in interval) yet 1 present
        false_pos = sum(mat[row, -(start:end)]),
        ## true negative: expecting 0 (not in interval) and 0 present
        true_neg  = sum(1 - mat[row, -(start:end)])) %>%
        ungroup()
}
log_if <- function(x) if (x == 0) return(0) else return(log(x))
loglikelihood <- function(tally_scores) {
    tally_scores %>%
        summarise_at(vars(contains("_")), sum) %>%
        mutate(p = true_pos / (true_pos + false_neg),
               q = false_pos / (false_pos + true_neg),
               loglik = true_pos * log_if(p) +
                   false_neg * log_if(1 - p) +
                   false_pos * log_if(q) +
                   true_neg * log_if(1 - q)) %>%
        .$loglik
}
get_intervals <- function(mat) {
    ## ensure mat is binary
    mat <- ifelse(mat > 0, 1, 0)
    ## Find possible intervals (starting and ending with a 1)
    all_intervals <- mat %>%
        tbl_df() %>%
        mutate(row=row_number()) %>%
        unite(row_string, -row, sep="") %>%
        group_by(row) %>%
        do(str_locate_all(., "1+")[[1]] %>%
               as.data.frame() %>%
               expand.grid() %>%
               filter(start <= end)) %>%
        ungroup()
    ## Simplify list of intervals (remove duplicate choices and obviously worse ones)
    worthwhile_intervals <- all_intervals %>%
        group_by(row) %>%
        full_join(tally_score_rowwise(., mat),
                  by = c("row", "start", "end")) %>%
        ## Remove obviously worse intervals -- THIS ISN'T VALID for unknown reasons
        # full_join(summarise(., max_true = max(true_pos + true_neg)), by = "row") %>%
        # filter(true_pos + true_neg == max_true) %>%
        distinct(true_pos, false_neg, false_pos, true_neg, .keep_all=TRUE) %>%
        ungroup()
    return(worthwhile_intervals)
}

hill_climb_gaps <- function(intervals, mat, steps_without_improvement=N_STEPS) {
    ## ensure mat is binary
    mat <- ifelse(mat > 0, 1, 0)
    ## search until `steps_without_improvement` mutations have been attempted
    ## without improving the likelihood
    best_choices <- intervals %>%
        select(row, start, end) %>%
        distinct(row, .keep_all=TRUE)
    best_likelihood <- best_choices %>% tally_score_rowwise(mat) %>% loglikelihood()
    ## remove interval choices that can't be changed
    available_changes <- intervals %>%
        select(row, start, end) %>%
        group_by(row) %>%
        left_join(tally(.), by="row") %>%
        filter(n > 1) %>%
        select(-n) %>%
        ungroup()
    steps <- 0
    while(steps < steps_without_improvement) {
        ## mutate
        new_choices <- available_changes %>%
            anti_join(best_choices, by=c("row", "start", "end")) %>%
            sample_n(1) %>%
            bind_rows(best_choices) %>%
            distinct(row, .keep_all=TRUE)
        ## evaluate
        new_likelihood <- new_choices %>% tally_score_rowwise(mat) %>% loglikelihood()
        ## keep if better
        if (new_likelihood > best_likelihood) {
            best_choices <- new_choices
            best_likelihood <- new_likelihood
            steps <- 0
            cat("\r", best_likelihood)
        }
        steps <- steps + 1
    }
    return(list(best_choices, best_likelihood))
}
meta_hill_climb <- function(original_ordering, mat,
                            steps=N_STEPS, tries=1,
                            fitness_function=hill_climb_gaps,
                            mutate_function=swap_two_elements) {
    steps_left <- steps
    best_ordering <- ordering <- original_ordering
    fitness <- fitness_function(mat %>% get_intervals(), mat)
    best_fitness <- fitness[[2]]
    best_intervals <- fitness[[1]]
    ## hill-climb until `steps` steps have passed without improvement
    while (steps_left > 0) {
        ## at each step, try `tries` different mutations and choose the best
        ordering <- mclapply(1:tries, mc.cores=N_CORES, function(ii) mutate_function(best_ordering)) %>% .[[1]]
            # .[[lapply(., function(oo) fitness_function(mat[oo,] %>% get_intervals(), mat[oo,])) %>% which.min]]
        fitness <- fitness_function(mat[ordering,] %>% get_intervals(), mat[ordering,])
        if (fitness[[2]] > best_fitness) {
            ## if the best step improves the fitness, update the records and reset the step count
            # cat("\r", fitness)
            best_fitness <- fitness[[2]]
            best_intervals <- fitness[[1]]
            best_ordering <- ordering
            steps_left <- steps
            ## otherwise, increment the steps-without-improvement count
        } else steps_left <- steps_left - 1
    }
    return(list(best_ordering, best_fitness, best_intervals))
}

## find the best intervals by searching all of them (very slow):
depth_first_search <- function(previous_branchings, intervals, mat, current_likelihood) {
    current_likelihood <- intervals[previous_branchings,] %>%
        tally_score_rowwise(mat) %>%
        loglikelihood()
    ## if the current partial likelihood is already worse than the best full one,
    ## then discontinue searching along this branch
    if (current_likelihood < best_likelihood) {
        # cat("Pruning Branch\n")
        return(NULL)
    }
    ## identify children
    next_row <- head(unique(intervals$row)[!(unique(intervals$row) %in%
                                                 intervals$row[previous_branchings])], 1)
    children <- which(intervals$row == next_row)
    ## if no children, at bottom, so return final piece of loglikelihood
    if (length(children) == 0) {
        if (current_likelihood > best_likelihood) {
            best_likelihood <<- current_likelihood
            # cat(best_likelihood, "\r")
        }
        return(NULL)
    }
    ## otherwise, for each child, recursively call the DFS
    for (child in children) {
        ## add to the likelihood
        depth_first_search(c(previous_branchings, child),
                           intervals,
                           mat,
                           current_likelihood)
    }
}
DFS <- function(intervals) {
    ## Count possibilities for each row and order by decreasing number of
    ## branchings and ordering within brancings by quality (i.e. -pi * (1 - qi))
    sorted_intervals <- intervals %>%
        full_join(count(., row), by = "row") %>%
        mutate(pi = true_pos / (true_pos + false_neg),
               qi = false_pos / (false_pos + true_neg)) %>%
        arrange(n, -pi * (1 - qi)) %>%
        select(row, start, end)
    #### Commence the search
    best_likelihood <<- -1000000
    depth_first_search(1, sorted_intervals, mat, 0)
}

# Sys.time()
# full_search_results <- meta_hill_climb(1:nrow(B), B)
# search_results <- get_n_eigenvectors(B %*% t(B), NN) %>%
#     filter(vector == ifelse(NN==1, "value", str_c("V", NN))) %>%
#     select(old_order = rowname, value) %>%
#     mutate(new_order = full_search_results[[1]]) %>%
#     gather("ordering", "position", -value)
# Sys.time()
# image(B)
# image(B[full_search_results[[1]],])
# full_search_results[[3]] %>% tally_score_rowwise((B > 0) * 1) %>% loglikelihood()

#### Approach 3: minimize multi-dimensional distance between adjacent nodes of network ####
distance <- function(vec1, vec2) sqrt(sum((vec1 - vec2)^2))
full_distance <- function(mat, dd=2) {
    pairs <- combn(1:nrow(mat), 2) %>% t %>% as_data_frame %>%
        rowwise %>%
        mutate(dist = distance(mat[V1,], mat[V2,])) %>%
        ungroup
    lapply(1:nrow(mat), function(row) {
        pairs %>%
            filter(V1 == row | V2 == row) %>%
            mutate(dist = rank(dist, ties.method="min")) %>%
            filter(abs(V1 - V2) <= dd) %>%
            summarise(sum(dist)) %>%
            as.numeric
    }) %>% unlist %>% sum
}
hill_climb_distance <- function(original_ordering, mat,
                                steps=N_STEPS, tries=N_TRIES,
                                fitness_function=full_distance,
                                mutate_function=swap_two_elements) {
    steps_left <- steps
    best_ordering <- ordering <- original_ordering
    best_fitness <- fitness <- fitness_function(mat[best_ordering,])
    ## hill-climb until `steps` steps have passed without improvement
    while (steps_left > 0) {
        ## at each step, try `tries` different mutations and choose the best
        ordering <- mclapply(1:tries, mc.cores=N_CORES, function(ii) mutate_function(ordering)) %>%
            .[[mclapply(., mc.cores=N_CORES, function(oo) fitness_function(mat[oo,])) %>% which.min]]
        fitness <- fitness_function(mat[ordering,])
        if (fitness < best_fitness) {
            ## if the best step has higher fitness, update record and reset step count
            # cat("\r", fitness)
            best_fitness <- fitness
            best_ordering <- ordering
            steps_left <- steps
            ## otherwise, increment the steps-without-improvement count
        } else steps_left <- steps_left - 1
    }
    cat("\ndone searching\n")
    return(list(best_ordering))
}

# Sys.time()
# search_results <- get_n_eigenvectors(B %*% t(B), NN) %>%
#     filter(vector == ifelse(NN==1, "value", str_c("V", NN))) %>%
#     select(old_order = rowname, value) %>%
#     mutate(new_order = hill_climb_distance(old_order, B)) %>%
#     gather("ordering", "position", -value)
# Sys.time()
# 
# B %>% full_diagnostic_plot() %>% plot()
# B[search_results$position[search_results$ordering == "new_order"],] %>%
#     full_diagnostic_plot() %>% plot()

#### Approach 4: Direct fitting of multivariate distributions ####

##########################

# times <- Sys.time()
# full_search_results <- get(search_function_string)(1:nrow(B), B)
# search_results <- get_n_eigenvectors(B %*% t(B), NN) %>%
#     filter(vector == ifelse(NN==1, "value", str_c("V", NN))) %>%
#     select(old_order = rowname, value) %>%
#     mutate(new_order = full_search_results[[1]]) %>%
#     gather("ordering", "position", -value)
# times[2] <- Sys.time()
# print(times[2] - times[1])
# save.image(file=str_c("../../Results/SimulatedMatrices/",
#                       file_path_sans_ext(basename(matfile)),
#                       "-", search_function_string, "-", N_STEPS, "-", N_TRIES,
#                       "-", R_SEED, ".RData"))



