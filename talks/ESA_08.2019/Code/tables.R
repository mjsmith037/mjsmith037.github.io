library(igraph)
library(tidygraph)
library(tidyverse)
library(kableExtra)
library(JuliaCall)

options(knitr.table.format = "latex")

julia_source("/home/michalska-smith/Research/CompetetiveTradeoff/Code/get_intransitivity.jl")

na_symbol <- "\\parbox[c]{0.8em}{\\hspace{1.5pt}\\tiny\\color{gray}\\faIcon[light]{times}\\hspace{1.5pt}}"
sig_small_symbol <- "\\parbox[b]{0.8em}{\\faIcon[light]{angle-down}}"
sig_sig_small_symbol <- "\\parbox[b]{0.8em}{\\faIcon[light]{angle-double-down}}"
sig_large_symbol <- "\\parbox[b]{0.8em}{\\faIcon[light]{angle-up}}"
sig_sig_large_symbol <- "\\parbox[b]{0.8em}{\\faIcon[light]{angle-double-up}}"
no_diff_symbol <- "{\\parbox[c]{0.8em}{\\hspace{1.5pt}\\tiny\\color{gray}\\faIcon[light]{minus}\\hspace{1.5pt}}}"

OVERLAP_LINK_CUTOFF <- 0.75
source("/home/michalska-smith/Research/CompetetiveTradeoff/Code/tidy_tests.R")
Sys.setenv(PATH=paste(Sys.getenv("PATH"),"/usr/local/texlive/2018/bin/x86_64-linux",sep=":"))

raw_network_data <- read_csv("/home/michalska-smith/Research/CompetetiveTradeoff/Data/complete_competetive_index.csv",
                             col_types="ccccccccdddddd") %>%
  filter(leaf_x == leaf_y) %>%
  mutate(isolate_x = str_c(isolate_x, type_x, sep="."),
         isolate_y = str_c(isolate_y, type_y, sep=".")) %>%
  do(bind_rows(transmute(., treatment = treatment_x, leaf        = leaf_x,
                         row_type     = type_x,      row_isolate = isolate_x,
                         col_type     = type_y,      col_isolate = isolate_y,
                         overlap      = x_on_y_pw),
               transmute(., treatment = treatment_x, leaf        = leaf_x,
                         row_type     = type_y,      row_isolate = isolate_y,
                         col_type     = type_x,      col_isolate = isolate_x,
                         overlap      = y_on_x_pw))) %>%
  # filter(overlap > OVERLAP_LINK_CUTOFF) %>%
  mutate(treatment = str_replace_all(treatment, c("C"="Control", "N"="NPK Supplemented")),
         row_type = str_replace_all(row_type, c("bacto"="Bacteria", "fungi"="Fungi")),
         col_type = str_replace_all(col_type, c("bacto"="Bacteria", "fungi"="Fungi")))

network_data <- tbl_graph(
  nodes = raw_network_data %>%
    do(bind_rows(transmute(., isolate=row_isolate, type=row_type, leaf, treatment),
                 transmute(., isolate=col_isolate, type=col_type, leaf, treatment))) %>%
    distinct(),
  edges = raw_network_data %>%
    select(row_isolate, col_isolate, overlap, leaf, treatment) %>%
    mutate(type=str_c(str_extract(row_isolate, "\\w+$"), "->", str_extract(col_isolate, "\\w+$"))))

network_data %>%
  morph(to_components) %>%
  crystallise() %>%
  rowwise() %>%
  use_series(graph) %>%
  lapply(. %>%
           filter(!node_is_isolated()) %>%
           mutate(w_clustering = transitivity(as.igraph(.), type="barrat", weights=edge_attr(as.igraph(.), "overlap")),
                  w_degree = centrality_degree(mode="all", weights=overlap, normalized=TRUE),
                  w_closeness = centrality_closeness(mode="all", weights=overlap, normalized=TRUE),
                  w_out_degree = centrality_degree(mode="out", weights=overlap, normalized=TRUE),
                  w_in_degree = centrality_degree(mode="in", weights=overlap, normalized=TRUE),
                  w_out_closeness = centrality_closeness(mode="out", weights=1/overlap, normalized=TRUE),
                  w_in_closeness = centrality_closeness(mode="in", weights=1/overlap, normalized=TRUE)) %>%
           activate(edges) %>%
           filter(overlap > OVERLAP_LINK_CUTOFF) %>%
           activate(nodes) %>%
           filter(!node_is_isolated()) %>%
           mutate(clustering = transitivity(as.igraph(.), type="local"),
                  degree = centrality_degree(mode="all", normalized=TRUE),
                  closeness = centrality_closeness(mode="all", normalized=TRUE),
                  out_degree = centrality_degree(mode="out", normalized=TRUE),
                  in_degree = centrality_degree(mode="in", normalized=TRUE),
                  out_closeness = centrality_closeness(mode="out", normalized=TRUE),
                  in_closeness = centrality_closeness(mode="in", normalized=TRUE)) %>%
           as_tibble()) %>%
  bind_rows() %>%
  select(-.tidygraph_node_index) %>%
  gather("metric", "value", -isolate, -type, -leaf, -treatment) %>%
  group_by(treatment, leaf, metric) %>%
  do(tidy_t_test_long(., "type", "value")) %>%
  group_by(treatment, leaf) %>%
  do(mutate(., leaf = str_extract(leaf, "\\d+"),
            adjpval = p.adjust(p.value),
            sig = case_when(adjpval < 0.001 ~ ifelse(Bacteria < Fungi,
                                                     cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="red!60!black"),
                                                     cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="blue!60!black")),
                            adjpval < 0.01  ~ ifelse(Bacteria < Fungi,
                                                     cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="red"),
                                                     cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="blue")),
                            adjpval < 0.05  ~ ifelse(Bacteria < Fungi,
                                                     cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="red!60!white"),
                                                     cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="blue!60!white")),
                            TRUE ~ "\\faIcon[light]{square}"))) %>%
  ungroup() %>%
  do(bind_cols(filter(., treatment == "Control"), filter(., treatment == "NPK Supplemented"))) %>%
  mutate(weighted = case_when(str_detect(metric, "^w")~"Weighted", TRUE~"Binary"),
         direction = case_when(str_detect(metric, "^(w_)?in")~"In", str_detect(metric, "^(w_)?out")~"Out", TRUE~"All"),
         metric = str_replace_all(str_replace_all(metric, "^w_", ""), c("^in_"="", "^out_"="")),
         metric = factor(metric, levels=c("clustering", "degree", "closeness"))) %>%
  arrange(leaf, metric) %>%
  group_by(weighted, direction, metric) %>%
  summarise(display = str_c(sig, collapse="\\,"), display1 = str_c(sig1, collapse="\\,")) %>%
  ungroup() %>%
  kable(row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lllcc", "")),
        col.names=c("Weights", "Direction", "Metric", "Control", "NPK Supplemented"), escape=FALSE) %>%
  collapse_rows(1:3) %>%
  save_kable("../Figures/centrality_t_test_table.pdf",
             latex_header_includes="\\usepackage[pro]{fontawesome5}\\definecolor{blue}{HTML}{006989}\\definecolor{red}{HTML}{9b1d20}")
system("pdftoppm ../Figures/centrality_t_test_table.pdf ../Figures/centrality_t_test_table -png -r 300")

network_data %>%
  morph(to_components) %>%
  mutate(group = group_spinglass(spins=2, weights=overlap)) %>%
  unmorph() %>%
  as_tibble() %>%
  group_by(treatment, leaf) %>%
  do(chisq.test(.$type, .$group) %>% tidy()) %>%
  ungroup() %>%
  mutate(weights="Weighted") %>%
  bind_rows(network_data %>%
              activate(edges) %>%
              filter(overlap > OVERLAP_LINK_CUTOFF, leaf != "C5") %>%
              activate(nodes) %>%
              filter(!node_is_isolated(), leaf != "C5") %>%
              morph(to_components) %>%
              mutate(group = group_spinglass(spins=2)) %>%
              unmorph() %>%
              as_tibble() %>%
              group_by(treatment, leaf) %>%
              do(chisq.test(.$type, .$group) %>% tidy()) %>%
              ungroup() %>%
              mutate(weights="Binary")) %>%
  select(-method, -parameter) %>%
  mutate(leaf = str_extract(leaf, "\\d+")) %>%
  bind_rows(tibble(treatment="Control", leaf="5", statistic=NA, p.value=NA, weights="Binary")) %>%
  mutate(statistic = round(statistic, 3),
         sig = case_when(is.na(p.value) ~ "\\hspace{1.25pt}\\faIcon[light]{times}\\hspace{1.25pt}",
                         p.value < 0.001 ~ cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="black"),
                         p.value < 0.01  ~ cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="black!60!white"),
                         p.value < 0.05  ~ cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="black!30!white"),
                         TRUE ~ "\\faIcon[light]{square}")) %>%
  do(bind_cols(filter(., treatment == "Control"), filter(., treatment == "NPK Supplemented"))) %>%
  select(-treatment, -treatment1, -leaf1, -weights1) %>%
  arrange(leaf) %>%
  group_by(weights) %>%
  summarise(display = str_c(sig, collapse="\\,"), display1 = str_c(sig1, collapse="\\,")) %>%
  ungroup() %>%
  kable(row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lcc", "")),
        col.names=c("Weights", "Control", "NPK Supplemented"), escape=FALSE) %>%
  save_kable("../Figures/chi_square_clustering_table.pdf",
             latex_header_includes="\\usepackage[pro]{fontawesome5}\\definecolor{blue}{HTML}{006989}\\definecolor{red}{HTML}{9b1d20}")
system("pdftoppm ../Figures/chi_square_clustering_table.pdf ../Figures/chi_square_clustering_table -png -r 300")

############### Network measures

get_metrics_from_graph <- function(g) {
  ig <- g %>%
    activate(edges) %>%
    filter(overlap >= OVERLAP_LINK_CUTOFF) %>%
    as.igraph()
  xx <- as_adjacency_matrix(ig, sparse=FALSE)
  w_xx <- as_adjacency_matrix(as.igraph(g), attr="overlap", sparse=FALSE)
  tibble(
    treatment = g %>% as_tibble() %>% use_series(treatment) %>% unique(),
    leaf = g %>% as_tibble() %>% use_series(leaf) %>% unique(),
    ## proportion of realized links
    connectance=sum(xx)/prod(dim(xx)),
    w_connectance = sum(w_xx)/prod(dim(w_xx)),
    ## mean degree
    meandeg=xx %>% colSums() %>% mean(),
    w_meandeg=w_xx %>% colSums() %>% mean(),
    ## mean proportion of in-degrees out of total degree
    meanpropin=mean(colSums(xx) / (colSums(xx) + rowSums(xx))),
    w_meanpropin=mean(colSums(w_xx) / (colSums(w_xx) + rowSums(w_xx))),
    ## sd of proportion of in-degrees out of total degree
    sdpropin=sd(colSums(xx) / (colSums(xx) + rowSums(xx))),
    w_sdpropin=sd(colSums(w_xx) / (colSums(w_xx) + rowSums(w_xx))),
    ## standard deviations of degree distributions
    sdin=xx %>% colSums() %>% sd(),
    w_sdin=w_xx %>% colSums() %>% sd(),
    sdout=xx %>% rowSums() %>% sd(),
    w_sdout=w_xx %>% rowSums() %>% sd(),
    ## directed loops of length n=2,3,4,5
    loops2=xx %*% xx %>% diag() %>% sum(),
    loops3=xx %*% xx %*% xx %>% diag() %>% sum(),
    loops4=xx %*% xx %*% xx %*% xx %>% diag() %>% sum(),
    loops5=xx %*% xx %*% xx %*% xx %*% xx %>% diag() %>% sum(),
    w_loops2=w_xx %*% w_xx %>% diag() %>% sum(),
    w_loops3=w_xx %*% w_xx %*% w_xx %>% diag() %>% sum(),
    w_loops4=w_xx %*% w_xx %*% w_xx %*% w_xx %>% diag() %>% sum(),
    w_loops5=w_xx %*% w_xx %*% w_xx %*% w_xx %*% w_xx %>% diag() %>% sum(),
    ## rightmost eigenvalues
    l1=eigen(xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[1],
    l2=eigen(xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[2],
    l3=eigen(xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[3],
    w_l1=eigen(w_xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[1],
    w_l2=eigen(w_xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[2],
    w_l3=eigen(w_xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[3],
    ## clustering coefficient
    clustering=transitivity(ig),
    w_clustering=0)
}

network_data %>%
  morph(to_components) %>%
  crystallise() %>%
  rowwise() %>%
  do(get_metrics_from_graph(.$graph)) %>%
  full_join(read_csv("/home/michalska-smith/Research/CompetetiveTradeoff/Results/intransitivity_binary.csv", col_types="cddd") %>% transmute(leaf, intransitivity=pw), by="leaf") %>%
  full_join(read_csv("/home/michalska-smith/Research/CompetetiveTradeoff/Results/intransitivity_weighted.csv", col_types="cddd") %>% transmute(leaf, w_intransitivity=pw), by="leaf") %>%
  gather("metric", "value", -leaf, -treatment) %>%
  filter(leaf != "C5") %>%
  group_by(metric) %>%
  do(tidy_t_test_long(., "treatment", "value")) %>%
  ungroup() %>%
  mutate(weighted = ifelse(str_detect(metric, "^w_"), "Weighted", "Binary"),
         metric = str_replace_all(metric, "^w_", ""),
         `NPK Supplemented` = ifelse(metric == "clustering" & weighted == "Weighted", NA, `NPK Supplemented`),
         Control = ifelse(metric == "clustering" & weighted == "Weighted", NA, Control)) %>%
  group_by(weighted) %>% # in case adjusting p-values
  mutate(p.value = case_when(is.na(p.value) ~ "\\hspace{1.25pt}\\faIcon[light]{times}\\hspace{1.25pt}",
                             p.value < 0.001 ~ ifelse(Control < `NPK Supplemented`,
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="npk!60!black"),
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="control!60!black")),
                             p.value < 0.01  ~ ifelse(Control < `NPK Supplemented`,
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="npk"),
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="control")),
                             p.value < 0.05  ~ ifelse(Control < `NPK Supplemented`,
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="npk!60!white"),
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="control!60!white")),
                             TRUE ~ "\\faIcon[light]{square}")) %>%
  ungroup() %>%
  select(weighted, metric, Control, everything()) %>%
  do(bind_cols(filter(., weighted=="Binary") %>% select(-weighted),
               filter(., weighted=="Weighted") %>% select(-weighted, -metric))) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  kable(row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lcccccc", "")),
        col.names=c("Metric", "Control", "NPK Supplemented", "p-Value",
                    "Control", "NPK Supplemented", "p-Value"), escape=FALSE,
        linesep=c("\\addlinespace", "\\addlinespace", "\\addlinespace", "", "",
                  "\\addlinespace", "", "", "", "\\addlinespace", "", "\\addlinespace", "", "")) %>%
  add_header_above(header=c(" "=1, "Binary"=3, "Weighted"=3)) %>%
  save_kable("../Figures/metric_raw_t_test.pdf",
             latex_header_includes="\\usepackage[pro]{fontawesome5}\\definecolor{control}{HTML}{575a3f}\\definecolor{npk}{HTML}{d7471e}")
system("pdftoppm ../Figures/metric_raw_t_test.pdf ../Figures/metric_raw_t_test -png -r 300")

triad_names <- c("1. A, B, C", "2. A->B, C", "3. A<->B, C", "4. A<-B->C",
                 "5. A->B<-C", "6. A->B->C", "7. A<->B<-C", "8. A<->B->C",
                 "9. A->B<-C->A", "10. A->B->C->A", "11. A<->B<->C",
                 "12. A<-B->C<->A", "13. A->B<-C<->A", "14. A->B->C<->A",
                 "15. A->B<->C<->A", "16. A<->B<->C<->A")

get_triads_from_graph <- function(g) {
  g %>% activate(edges) %>% filter(overlap > OVERLAP_LINK_CUTOFF) %>%
    as.igraph() %>% triad.census() %>% setNames(triad_names) %>% t() %>% as_tibble() %>%
    mutate(treatment = g %>% as_tibble() %>% use_series(treatment) %>% unique(),
           leaf = g %>% as_tibble() %>% use_series(leaf) %>% unique())
}

network_data %>%
  morph(to_components) %>%
  crystallise() %>%
  rowwise() %>%
  do(get_triads_from_graph(.$graph)) %>%
  gather("triad", "value", -leaf, -treatment) %>%
  group_by(triad) %>%
  filter(leaf != "C5") %>%
  do(tidy_t_test_long(., "treatment", "value")) %>%
  ungroup() %>%
  select(triad, Control, `NPK Supplemented`, p.value) %>%
  mutate(p.value = case_when(is.na(p.value) ~ "\\hspace{1.25pt}\\faIcon[light]{times}\\hspace{1.25pt}",
                             p.value < 0.001 ~ ifelse(Control < `NPK Supplemented`,
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="npk!60!black"),
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="control!60!black")),
                             p.value < 0.01  ~ ifelse(Control < `NPK Supplemented`,
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="npk"),
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="control")),
                             p.value < 0.05  ~ ifelse(Control < `NPK Supplemented`,
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="npk!60!white"),
                                                      cell_spec("\\faIcon[solid]{square}", escape=FALSE, color="control!60!white")),
                             TRUE ~ "\\faIcon[light]{square}"),
         # p.value = tidy_p_value(p.value),
         triad = factor(triad, levels=triad_names, ordered=TRUE)) %>%
  arrange(triad) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  mutate(triad = str_c("\\parbox[c]{0.5em}{\\vspace{1pt}\\scalebox{0.5}{\\input{'/home/michalska-smith/Research/CompetetiveTradeoff/Triad_Descriptions/Triad_Diagrams/",
                       unlist(str_extract_all(triad, "\\d+\\.")), "tex'}}\\vspace{1pt}}")) %>%
  kable(row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lccc", "")),
        col.names=c("Triad", "Control", "NPK Supplemented", "p-Value"), escape=FALSE,
        linesep=c("\\addlinespace", "\\addlinespace", "", "", "", "\\addlinespace",
                  "", "", "", "\\addlinespace", "", "", "", "\\addlinespace", "\\addlinespace")) %>%
  save_kable("../Figures/triad_raw_t_test.pdf",
             latex_header_includes="\\usepackage{tikz}\\usepackage[pro]{fontawesome5}\\definecolor{control}{HTML}{575a3f}\\definecolor{npk}{HTML}{d7471e}")
system("pdftoppm ../Figures/triad_raw_t_test.pdf ../Figures/triad_raw_t_test -png -r 300")

####################### network metrics against config model

load("~/Research/CompetetiveTradeoff/Code/Network_Structure_Nulls.RData")

number_of_unique_randomizations <- configmodel_communities %>%
  lapply(. %>% lapply(. %>% as_adjacency_matrix() %>% as.vector() %>% str_c(collapse="")) %>%
           unique() %>% length()) %>%
  setNames(names(configmodel_communities)) %>% unlist()

get_config_metrics <- function(g) {
  xx <- as_adjacency_matrix(g, sparse=FALSE)
  tibble(
    ## directed loops of length n=2,3,4,5
    loops2=xx %*% xx %>% diag() %>% sum(),
    loops3=xx %*% xx %*% xx %>% diag() %>% sum(),
    loops4=xx %*% xx %*% xx %*% xx %>% diag() %>% sum(),
    loops5=xx %*% xx %*% xx %*% xx %*% xx %>% diag() %>% sum(),
    ## rightmost eigenvalues
    l1=eigen(xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[1],
    l2=eigen(xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[2],
    l3=eigen(xx, only.values=TRUE)$values %>% Re() %>% sort(decreasing=TRUE) %>% .[3],
    ## clustering coefficient
    clustering=transitivity(g),
    intransitivity=list(xx) %>J% mcmcmc(9L, 10000L, 50L, 50L) %>J% intransitivity)
}

configmodel_metrics <- configmodel_communities %>%
  lapply(. %>% lapply(get_config_metrics) %>% bind_rows()) %>%
  bind_rows() %>%
  mutate(leaf = rep(names(configmodel_communities), each=1000))

full_join(
  configmodel_metrics %>%
    gather("metric", "value", -leaf) %>%
    right_join(sympatric_communities %>% lapply(get_config_metrics) %>% bind_rows() %>%
                 mutate(leaf = names(sympatric_communities)) %>% gather("metric", "value", -leaf),
               by=c("leaf", "metric")) %>%
    group_by(leaf, metric) %>%
    summarise(p.value = sum(value.x < value.y) / n()) %>%
    mutate(p.value = ifelse(leaf %in% names(number_of_unique_randomizations[number_of_unique_randomizations > 20]), p.value, NA),
           sig = case_when(is.na(p.value)  ~ na_symbol,
                           p.value < 0.01  ~ sig_sig_small_symbol,
                           p.value < 0.05  ~ sig_small_symbol,
                           p.value > 0.99  ~ sig_sig_large_symbol,
                           p.value > 0.95  ~ sig_large_symbol,
                           TRUE ~ no_diff_symbol)) %>%
    separate(leaf, c("treatment", "leaf"), 1) %>%
    select(treatment, leaf, metric, sig) %>%
    do(bind_cols(filter(., treatment == "C"), filter(., treatment == "N"))) %>%
    arrange(leaf) %>%
    group_by(metric) %>%
    summarise(display = str_c(sig, collapse="\\,"), display1 = str_c(sig1, collapse="\\,")) %>%
    ungroup(),
  configmodel_metrics %>%
    gather("metric", "value", -leaf) %>%
    group_by(leaf, metric) %>%
    summarise(sd=sd(value), mean=mean(value)) %>%
    ungroup() %>%
    right_join(sympatric_communities %>% lapply(get_config_metrics) %>% bind_rows() %>%
                 mutate(leaf = names(sympatric_communities)) %>% gather("metric", "value", -leaf),
               by=c("leaf", "metric")) %>%
    mutate(z.score = ifelse(sd == 0, NaN, (value - mean) / sd)) %>%
    mutate(z.score = ifelse(leaf %in% names(number_of_unique_randomizations[number_of_unique_randomizations > 20]),
                            z.score, NA)) %>%
    mutate(sig = case_when(is.na(z.score)  ~ na_symbol,
                           z.score < -4  ~ sig_sig_small_symbol,
                           z.score < -2  ~ sig_small_symbol,
                           z.score > 4  ~ sig_sig_large_symbol,
                           z.score > 2  ~ sig_large_symbol,
                           TRUE ~ no_diff_symbol)) %>%
    separate(leaf, c("treatment", "leaf"), 1) %>%
    select(treatment, leaf, metric, sig) %>%
    do(bind_cols(filter(., treatment == "C"), filter(., treatment == "N"))) %>%
    arrange(leaf) %>%
    group_by(metric) %>%
    summarise(display = str_c(sig, collapse="\\,"), display1 = str_c(sig1, collapse="\\,")) %>%
    ungroup(),
  by=c("metric")
) %>%
  kable(row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lcccc", "")),
        col.names=c("Metric", "Control", "NPK Supplemented", "Control", "NPK Supplemented"), escape=FALSE,
        linesep=c("\\addlinespace", "\\addlinespace", "", "", "\\addlinespace", "", "", "")) %>%
  add_header_above(c(" " = 1, "p-value" = 2, "z-score" = 2)) %>%
  save_kable("../Figures/metric_config_table.pdf",
             latex_header_includes="\\usepackage[pro]{fontawesome5}\\definecolor{gray}{HTML}{888888}")
system("pdftoppm ../Figures/metric_config_table.pdf ../Figures/metric_config_table -png -r 300")

full_join(
  configmodel_community_triads %>%
    full_join(sympatric_community_triads, by=c("leaf", "treatment", "triad")) %>%
    group_by(leaf, triad) %>%
    summarise(p.value = sum(count.x < count.y) / n()) %>%
    mutate(p.value = ifelse(leaf %in% names(number_of_unique_randomizations[number_of_unique_randomizations > 20]), p.value, NA),
           sig = case_when(is.na(p.value)  ~ na_symbol,
                           p.value < 0.01  ~ sig_sig_small_symbol,
                           p.value < 0.05  ~ sig_small_symbol,
                           p.value > 0.99  ~ sig_sig_large_symbol,
                           p.value > 0.95  ~ sig_large_symbol,
                           TRUE ~ no_diff_symbol)) %>%
    separate(leaf, c("treatment", "leaf"), 1) %>%
    select(treatment, leaf, triad, sig) %>%
    do(bind_cols(filter(., treatment == "C"), filter(., treatment == "N"))) %>%
    arrange(leaf) %>%
    group_by(triad) %>%
    summarise(display = str_c(sig, collapse="\\,"), display1 = str_c(sig1, collapse="\\,")) %>%
    ungroup(),
  configmodel_community_triads %>%
    group_by(leaf, triad) %>%
    summarise(sd=sd(count), mean=mean(count)) %>%
    ungroup() %>%
    full_join(sympatric_community_triads, by=c("leaf", "triad")) %>%
    mutate(z.score = ifelse(sd == 0, NaN, (count - mean) / sd)) %>%
    mutate(z.score = ifelse(leaf %in% names(number_of_unique_randomizations[number_of_unique_randomizations > 20]),
                            z.score, NA)) %>%
    separate(leaf, c("treatment", "leaf"), 1) %>%
    mutate(sig = case_when(is.na(z.score)  ~ na_symbol,
                           z.score < -4  ~ sig_sig_small_symbol,
                           z.score < -2  ~ sig_small_symbol,
                           z.score > 4  ~ sig_sig_large_symbol,
                           z.score > 2  ~ sig_large_symbol,
                           TRUE ~ no_diff_symbol)) %>%
    select(treatment, leaf, triad, sig) %>%
    do(bind_cols(filter(., treatment == "C"), filter(., treatment == "N"))) %>%
    arrange(leaf) %>%
    group_by(triad) %>%
    summarise(display = str_c(sig, collapse="\\,"), display1 = str_c(sig1, collapse="\\,")) %>%
    ungroup(),
  by=c("triad")
) %>%
  mutate(triad = str_c("\\parbox[c]{0.5em}{\\vspace{1pt}\\scalebox{0.5}{\\input{'/home/michalska-smith/Research/CompetetiveTradeoff/Triad_Descriptions/Triad_Diagrams/",
                       unlist(str_extract_all(triad, "\\d+\\.")), "tex'}}\\vspace{1pt}}")) %>%
  kable(row.names=FALSE, booktabs=TRUE, align=unlist(str_split("lcccc", "")),
        col.names=c("Triad", "Control", "NPK Supplemented", "Control", "NPK Supplemented"), escape=FALSE,
        linesep=c("\\addlinespace", "\\addlinespace", "", "", "", "\\addlinespace",
                  "", "", "", "\\addlinespace", "", "", "", "\\addlinespace", "\\addlinespace")) %>%
  add_header_above(c(" " = 1, "p-value" = 2, "z-score" = 2)) %>%
  save_kable("../Figures/triad_config_table.pdf",
             latex_header_includes="\\usepackage{tikz}\\usepackage[pro]{fontawesome5}\\definecolor{gray}{HTML}{888888}")
system("pdftoppm ../Figures/triad_config_table.pdf ../Figures/triad_config_table -png -r 300")


configmodel_metrics %>%
  filter(leaf != "C5") %>%
  gather("metric", "value", -leaf) %>%
  group_by(leaf, metric) %>%
  summarise(sd=sd(value), mean=mean(value)) %>%
  ungroup() %>%
  right_join(sympatric_communities %>% lapply(get_config_metrics) %>% bind_rows() %>%
               mutate(leaf = names(sympatric_communities)) %>% gather("metric", "value", -leaf),
             by=c("leaf", "metric")) %>%
  mutate(z.score = ifelse(sd == 0, NaN, (value - mean) / sd)) %>%
  mutate(z.score = ifelse(leaf %in% names(number_of_unique_randomizations[number_of_unique_randomizations > 100]),
                          z.score, NA)) %>%
  mutate(treatment = str_extract(leaf, "\\w")) %>%
  group_by(metric) %>%
  do(tidy_t_test_long(., "treatment", "z.score"))


configmodel_community_triads %>%
  group_by(leaf, triad) %>%
  summarise(sd=sd(count), mean=mean(count)) %>%
  ungroup() %>%
  full_join(sympatric_community_triads, by=c("leaf", "triad")) %>%
  mutate(z.score = ifelse(sd == 0, NaN, (count - mean) / sd)) %>%
  mutate(z.score = ifelse(leaf %in% names(number_of_unique_randomizations[number_of_unique_randomizations > 20]),
                          z.score, NA)) %>%
  na.omit() %>%
  separate(leaf, c("treatment", "leaf"), 1) %>%
  add_count(triad, treatment) %>%
  filter(n > 3) %>%
  add_count(triad) %>%
  filter(n > 4) %>%
  group_by(triad) %>%
  do(tidy_t_test_long(., "treatment", "z.score"))
