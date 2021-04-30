library(magrittr)
library(tidygraph)
library(docstring)
library(tidyverse)
library(assertthat)

transition_matrix <- function(prob_no_infection, sigma, rho, gamma, mu) {
  #        S,                             E,                Ia,          Is,     R,  D
  matrix(c(0, 1 - min(prob_no_infection, 1),                 0,           0,     0,  0,  # S
           0,                             0, sigma * (1 - rho), sigma * rho,     0,  0,  # E
           0,                             0,                 0,           0, gamma,  0,  # Ia
           0,                             0,                 0,           0, gamma, mu,  # Is
           0,                             0,                 0,           0,     0,  0,  # R
           0,                             0,                 0,           0,     0,  0), # D
         6, 6, byrow=TRUE) %>%
    set_rownames(c("S", "E", "Ia", "Is", "R", "D")) %>%
    set_colnames(c("S", "E", "Ia", "Is", "R", "D")) %>%
    subtract(diag(rowSums(.) - 1))
}

state_at_next_timestep <- function(prev_state, n, beta, sigma, rho, gamma, mu, nu, vulnerable) {
  # the probability of infection is one minus the probability that all potentially
  # infectious connections fail to infect at this timepoint
  # TODO: consider replacing apply with matrixStats::colProds()
  prob_no_infection <- (1 - beta)^do.call(rbind, n) %>% apply(2, prod)
  lapply(1:length(prev_state), function(ii) {
    transition_matrix(prob_no_infection[ii], sigma, rho,
                      gamma * (vulnerable[ii] * 3 + !vulnerable[ii]), # vulnerable indiv. have longer recoveries
                      mu + vulnerable[ii] * nu # vulnerable indiv. have greater death rates
                      ) %>%
      .[prev_state[ii],] %>% sample(names(.), 1, prob=.)
  }) %>% unlist(use.names=FALSE) %>% factor(levels=c("S", "E", "Ia", "Is", "R", "D"))
}

run_SARSCoV2_simulation <- function(timesteps, contact_network,
                                    # TODO: different parameters for symptomatic/asymptomatic
                                    beta_values=c(household=0.33, background=0.001, classmate=0.1), #, between_household=0.5
                                    sigma=1/7, rho=1/4, gamma=1/14, mu=0.01/100, nu=14/100) {
  #' Network Disease Simulation Framework
  #'
  #' @description Code to simulate a disease process on a network of individuals
  #' @author Matthew J. Michalska-Smith
  #'
  #' @param timesteps the integer number of timesteps to take in the simulation
  #' @param contact_network a tidygraph representation of a binary directed or
  #' undirected network of contacts between individuals, or a square adjacency
  #' matrix that will be converted into a tidygraph
  #' @param eta the per capita background level of disease transmission (S -> E)
  #' @param beta_values a named vector containing the relative (to background)
  #' infection rate for each interaction type: transition rate for S -> E. These
  #' numbers are scalar multipliers for eta
  #' @param sigma the disease progression rate: transition from E -> Ia/Is
  #' @param rho the proportion of E -> Ia/Is that will be symptomatic
  #' @param gamma the recovery rate: transition rate for Ia/Is-> R
  #' @param mu the death rate of symptomatic infectious individuals
  #' @param nu the additional death rate for vulnerable individuals
  #'
  #' @return
  #'
  #' @note
  #' All parameters are typically defined to be within [0, 1]
  #'
  #' @examples
  #'

  # ensure mapping of beta values
  types_present <- contact_network %E>% as_tibble() %>% pull(type) %>% unique()
  assert_that(beta_values %>% names() %>% setdiff("background") %>% setequal(types_present),
              msg="Mismatch between beta values and layers in contact network")

  # initialize with one (asymptomatic) infected node (chosen at random)
  contact_network %<>%
    activate(nodes) %>%
    mutate("1" = sample(c("Ia", rep("S", nrow(as_tibble(.)) - 1))) %>%
             factor(levels=c("S", "E", "Ia", "Is", "R", "D")))

  # progress through the discrete time epidemic simulation
  for (timestep in 2:timesteps) {
    contact_network %<>%
      activate(edges) %>%
      # identify which links have the potential to produce new infectious individuals
      # i.e. which links connect an infectious individual with a susceptible one?
      mutate(pot_inf_link = edge_is_between(.N() %>% pull() %>% equals("S"),
                                            .N() %>% pull() %>% str_starts("I"))) %>%
      activate(nodes) %>%
      # see how many potentially infecting connections each individual has
      mutate(!!!(activate(., edges) %>% as_tibble() %>% pull(type) %>% unique() %>%
      {set_names(map(., ~quo(centrality_degree(weights=pot_inf_link * (type == !!.), mode="in"))),
                 str_c("number_infectious_", .))}),
      number_infectious_background = as_tibble(.) %>% pull(str_c(timestep - 1)) %>% str_starts("I") %>% sum()) %>%
      # create a new column for this timestep, updating statuses when neccessary
      # the probability of a state-change this timestep is determined by the
      # model parameters
      mutate(!!str_c(timestep) := state_at_next_timestep(!!sym(str_c(timestep - 1)),
                                                         list(!!!syms(str_c("number_infectious_", names(beta_values)))),
                                                         beta_values, sigma, rho, gamma, mu, nu, vulnerable))
  }
  return(contact_network)
}
