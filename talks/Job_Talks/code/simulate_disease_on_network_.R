library(magrittr)
library(tidygraph, warn.conflicts=FALSE)
library(docstring, warn.conflicts=FALSE)
library(tidyverse)

run_disease_network_simulation <- function(timesteps, contact_network, model_structure,
                                           beta, sigma=0, gamma, xi=0) {
  #' Network Disease Simulation Framework
  #'
  #' @description Code to simulate a disease process on a network of individuals
  #' @author Matthew J. Michalska-Smith
  #'
  #' @param timesteps the integer number of timesteps to take in the simulation
  #' @param contact_network a tidygraph representation of a binary directed or
  #' undirected network of contacts between individuals, or a square adjacency
  #' matrix that will be converted into a tidygraph
  #' @param model_structure choice of compartmental model to simulate choices1
  #' currently include SI(S), SIR(S), and SEIR(S), provided as a string (e.g. "SIS")
  #' @param beta (required) the infection rate: transition rate for S -> I/E
  #' @param sigma the disease progression rate: transition from E -> I if there is
  #' an exposed class. Defaults to 0. Note this means if an E class is present and
  #' this parameter is not adjusted, then the E class will be a sink.
  #' @param gamma (required) the recovery rate: transition rate for I -> S/R (set to 0 for SI)
  #' @param xi the immunity-loss rate: transition from R -> S. Defaults to 0. Note
  #' this means that if the model structure is SIRS or SEIRS and this parameter
  #' is not adjusted, then will behave as if it were SIR or SEIR, respectively
  #'
  #' @return a tibble with three columns containing a timeseries showing the number of
  #' nodes in each disease class over the time of the simulation
  #'
  #' @note
  #' The dynamical parameters (beta, sigma, gamma, and xi) dictate transitions
  #' between the classes specified by the model structure. If set to 0, individuals
  #' never leave a class. if set to 1, individuals always leave the class in the
  #' following timestep.
  #'
  #' @examples
  #' contact_network <- play_erdos_renyi(50, 0.4, directed=FALSE)
  #' timeseries <- run_disease_network_simulation(50, contact_network, "SIR", beta=0.1, gamma=0.1)
  #' ggplot(timeseries) + aes(x=time, y=n, colour=status) + geom_line()

  # check that all necessary parameters are present (and non-zero)
  if (not(all(c(beta, sigma, gamma, xi) >=0))) stop("All parameters must be >= 0")
  if (beta == 0) warning("Must have non-zero beta for infection to occur")
  if (str_detect(model_structure, "E") & sigma == 0)
    warning("There is an exposed class, but with sigma = 0 there is no way to get out of it")
  if (str_detect(model_structure, "R") & gamma == 0)
    warning("There is a removed class, but with gamma = 0 there is no way to get into it")
  if (str_detect(model_structure, "RS") & xi == 0)
    warning("The model suggests waning immunity, but with xi = 0 there is no way to exit the removed class")
  if (str_detect(model_structure, "I$") & gamma != 0) {
    warning("The model suggests no immunity, yet a gamma > 0 was provided. Setting to 0 before continuing")
    gamma <- 0
  }
  # parse model structure
  model_states <- str_split(model_structure, "")[[1]]
  if (head(model_states, 1) == tail(model_states, 1)) model_states %<>% head(-1)

  # initialize with one infected node (chosen at random)
  if (is.matrix(contact_network)) contact_network %<>% as_tbl_graph()
  contact_network %<>%
    activate(nodes) %>%
    mutate("0" = sample(c("I", rep("S", nrow(as_tibble(.)) - 1))) %>%
             factor(levels=model_states))

  # progress through the discrete time epidemic simulation
  for (timestep in 1:timesteps) {
    contact_network %<>%
      activate(edges) %>%
      # identify which links have the potential to produce new infectious individuals
      # i.e. which links connect an infectious individual with a susceptible one?
      mutate(pot_inf_link = edge_is_between(.N()[str_c(timestep - 1)] == "S",
                                            .N()[str_c(timestep - 1)] == "I")) %>%
      activate(nodes) %>%
      # the probability of a state-change this timestep is determined by the model parameters:
      mutate(prob_change_status = case_when(
        !!sym(str_c(timestep - 1)) == "S" ~
          # depends on how many potentially infecting connections each individual has
          1 - (1 - beta)^(centrality_degree(weights=pot_inf_link, mode="in")), # infection rate
        !!sym(str_c(timestep - 1)) == "E" ~ sigma,      # rate of disease progression
        !!sym(str_c(timestep - 1)) == "I" ~ gamma,      # recovery rate
        !!sym(str_c(timestep - 1)) == "R" ~ xi,         # rate of immunity loss
        # if there is not a rate parameter for this state, then don't change
        TRUE ~ 0),
        # create a new column for this timestep, updating statuses when neccessary
        !!str_c(timestep) :=
          model_states[!!sym(str_c(timestep - 1)) %>%
                         as.integer() %>%
                         add(rbernoulli(n(), prob_change_status)) %>%
                         # in some models, we loop around to the beginning again
                         {ifelse(. > length(model_states), 1, .)}] %>%
          factor(levels=model_states))
  }

  # convert the output to a more manageable format
  timeseries <- contact_network %N>%
    # remove unnecessary columns
    select(-prob_change_status) %>%
    # remove the network data
    as_tibble() %>%
    # wide -> long format
    pivot_longer(names_to="time", values_to="status", everything()) %>%
    # clean up types
    mutate(time = as.integer(time),
           status = factor(status, levels=model_states)) %>%
    # summarise number of each class at each timestep
    count(status, time) %>%
    # add 0's for missing classes at each timestep
    complete(status, nesting(time), fill=list(n=0))

  # return the cleaned output
  return(timeseries)
}
