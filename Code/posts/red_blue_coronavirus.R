library(tidyverse)
library(lubridate)
library(janitor)

county_party <- read_csv("~/Desktop/countypres_2000-2016.csv") %>%
  filter(year == 2016, office == "President") %>%
  group_by(FIPS) %>%
  top_n(1, candidatevotes) %>%
  ungroup()
fips_codes <- read_csv("~/Desktop/fips_codes.csv", skip=4) %>%
  unite("fips", `State Code (FIPS)`, `County Code (FIPS)`, sep="") %>%
  select(level=`Summary Level`, fips, area_name=`Area Name (including legal/statistical area description)`) %>%
  filter(level == "050") # %in% c("010", "040", "050"))
county_populations <- read_csv("~/Desktop/county_data.txt") %>%
  select(fips, population=POP010210, land_area=LND110210, population_density=POP060210) %>%
  left_join(fips_codes, by="fips")
cases_by_county <- read_csv(file="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  select(fips=FIPS, matches("^\\d")) %>%
  pivot_longer(-fips, names_to="date", values_to="cases")

# https://coolors.co/33658a-b1ede8-0c7c59-a79162-f87575-d62839

# density plots ################################################################

cutoff_function <- function(vec, p_upper, p_lower=p_upper) {
  case_when(vec > quantile(vec, p_upper) ~ "upper",
            vec <= quantile(vec, p_lower) ~ "lower",
            TRUE ~ "middle")
}

expected_cutoffs <- county_populations %>%
  filter(!str_ends(fips, "000")) %>%
  mutate(density_class = cutoff_function(population_density, 0.9, 0.5)) %>%
  count(density_class, wt=population) %>%
  mutate(percent = n / sum(n))

left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  mutate(density_class = cutoff_function(population_density, 0.9, 0.5)) %>%
  group_by(fips) %>% mutate(cases = cases - lag(cases)) %>% ungroup() %>%
  count(date, density_class, wt=cases) %>%
  group_by(date) %>% mutate(percent = n / sum(n)) %>% ungroup() %>%
  {ggplot(.) + aes(x=date, y=percent, colour=density_class) +
      geom_hline(aes(yintercept=percent), linetype="dashed", colour=c("#e67580", "#a79162", "#5b96c2"),
                 data=mutate(expected_cutoffs)) +
      geom_text(aes(x=x, label=lab), colour=c("#e67580", "#a79162", "#5b96c2"),
                nudge_y=-0.01, nudge_x=-2, hjust=0, vjust=c(1, -1, -1),
                data=mutate(expected_cutoffs, x=rep(min(.$date), 3),
                            lab=c("Percent of population in bottom 50% counties by density",
                                  "Percent of population in the next 40% (50%-90%) of counties by density",
                                  "Percent of population in top 10% most dense couties"))) +
      geom_line(size=1.5) +
      scale_colour_manual(values=c("lower"="#d62839", "middle"="#0c7c59", "upper"="#33658a"),
                          labels=c("Bottom 50% of counties by density",
                                   "Middle 40% (50%-90%) counties by density\t     ",
                                   "Top 10% most dense counties\t"),
                          guide=guide_legend(reverse=TRUE)) +
      scale_x_date(expand=expansion(mult=c(0.01, 0.05)), date_labels="%B") +
      scale_y_continuous(name="Percent of new cases daily", limits=c(0, 1),
                         labels=c("0%", "25%", "50%", "75%", "100%"),
                         expand=c(0,0)) +
      ggtitle("Percent of new COVID-19 cases occuring daily in counties segregated by population density") +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         axis.title.x=element_blank()) +
      labs(caption="COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")}
ggsave("../../Images/blog_figures/red_blue_coronavirus/counties_by_density.png", width=10, height=6)

left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  mutate(density_class = cutoff_function(population_density, 0.95)) %>%
  group_by(fips) %>% mutate(cases = (cases - lag(cases)) / population) %>% ungroup() %>%
  count(date, density_class, wt=cases) %>%
  group_by(date) %>% mutate(percent = n / sum(n)) %>% ungroup() %>%
  {ggplot(.) + aes(x=date, y=percent*100, colour=density_class) +
      geom_hline(yintercept=c(95, 5), linetype="dashed", color=c("#e67580", "#5b96c2")) +
      geom_line(size=1.5) +
      scale_colour_manual(values=c("upper"="#33658a", "middle"="#0c7c59", "lower"="#d62839")) +
      scale_x_date(expand=expansion(mult=c(0.01, 0.05)), date_labels="%B") +
      scale_y_continuous(name="Percent of new cases daily per capita", limits=c(0, 100), labels=c("0%", "25%", "50%", "75%", "100%"), expand=c(0,0)) +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         axis.title.x=element_blank())}

left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  mutate(density_class = cutoff_function(population_density, 0.95)) %>%
  count(date, density_class, wt=cases) %>%
  mutate(n = ifelse(density_class == "lower", -n, n)) %>%
  {ggplot(.) + aes(x=date, y=n, colour=density_class, fill=density_class) +
      geom_area() +
      scale_fill_manual(values=c("lower"="#d62839", "upper"="#33658a"),
                        labels=c("Remaining 95% of counties", "Top 5% most dense counties\t\t"),
                        guide=guide_legend(reverse=TRUE),
                        aesthetics=c("colour", "fill")) +
      scale_x_date(expand=expansion(mult=c(0.01, 0.05)), date_labels="%B") +
      scale_y_continuous(breaks=c(-1e6, -5e5, 0, 5e5, 1e6, 1.5e6),
                         labels=c("1 000 000", "500 000", "0", "500 000", "1 000 000", "1 500 000"),
                         name="Total Number of Cases") +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         axis.title.x=element_blank())}

# party graphs #################################################################


expected_cutoffs <- county_populations %>%
  mutate(fips = as.integer(fips)) %>%
  left_join(county_party, by=c("fips"="FIPS")) %>%
  na.omit() %>%
  count(party, wt=population) %>%
  mutate(percent = n / sum(n))

left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>%
  left_join(county_party, by=c("fips"="FIPS")) %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  # mutate(density_class = cutoff_function(population_density, 0.95)) %>%
  group_by(fips) %>% mutate(cases = cases - lag(cases)) %>% ungroup() %>%
  na.omit() %>%
  count(date, party, wt=cases) %>%
  group_by(date) %>% mutate(percent = n / sum(n)) %>% ungroup() %>%
  {ggplot(.) + aes(x=date, y=percent, colour=party) +
      geom_hline(aes(yintercept=percent), linetype="dashed", colour=c("#5b96c2", "#e67580"),
                 data=mutate(expected_cutoffs)) +
      geom_line(size=1.5) +
      geom_text(aes(x=x, label=lab), colour=c("#5b96c2", "#e67580"),
                nudge_y=c(-0.005, -0.015), nudge_x=-2, hjust=0, vjust=c(-1, 1),
                data=mutate(expected_cutoffs, x=rep(min(.$date), 2),
                            lab=c("Percent of population in 'Clinton' counties",
                                  "Percent of population in 'Trump' counties"))) +
      scale_colour_manual(values=c("democrat"="#33658a", "republican"="#d62839"),
                          labels=c("Counties that voted for Clinton in 2016\t",
                                   "Counties that voted for Trump in 2016")) +
      scale_x_date(expand=expansion(mult=c(0.01, 0.05)), date_labels="%B") +
      scale_y_continuous(name="Percent of new cases daily", limits=c(0, 1),
                         labels=c("0%", "25%", "50%", "75%", "100%"),
                         expand=c(0,0)) +
      ggtitle("Percent of new COVID-19 cases occuring daily in 'red' and 'blue' counties over time") +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         legend.text=element_text(size=14),
                         axis.title.x=element_blank()) +
      labs(caption="MIT Election Data and Science Lab, 2018, Harvard Dataverse, County Presidential Election Returns 2000-2016\nCOVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")}
ggsave("../../Images/blog_figures/red_blue_coronavirus/counties_redo.png", width=10, height=6)

left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>%
  left_join(county_party, by=c("fips"="FIPS")) %>%
  na.omit() %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  group_by(fips) %>% mutate(cases = (cases - lag(cases)) / population_density) %>% ungroup() %>%
  count(date, party, wt=cases) %>%
  group_by(date) %>% mutate(percent = n / sum(n)) %>% ungroup() %>%
  {ggplot(.) + aes(x=date, y=percent, colour=party) +
      geom_hline(aes(yintercept=percent), linetype="dashed", colour=c("#5b96c2", "#e67580"),
                 data=tabyl(county_party, party)) +
      geom_line(size=1.5) +
      geom_text(aes(x=x, label=lab), colour=c("#5b96c2", "#e67580"),
                nudge_y=c(-0.014, -0.009), hjust=0, vjust=c(1, -1),
                data=mutate(tabyl(county_party, party), x=rep(min(.$date), 2) + 7,
                            lab=c("Percent of all counties that voted for Clinton",
                                  "Percent of all counties that voted for Trump"))) +
      scale_colour_manual(values=c("democrat"="#33658a", "republican"="#d62839"),
                          labels=c("Counties that voted for Clinton in 2016\t",
                                   "Counties that voted for Trump in 2016")) +
      scale_x_date(expand=expansion(mult=c(0.03, 0.05)), date_labels="%B") +
      scale_y_continuous(name="Percent of new cases daily per capita", limits=c(0, 1),
                         labels=c("0%", "25%", "50%", "75%", "100%"), expand=c(0,0)) +
      ggtitle("Percent of new COVID-19 cases occuring daily in 'red' and 'blue' counties over time",
              "Number of new cases normalized according to county population") +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         legend.text=element_text(size=14),
                         axis.title.x=element_blank()) +
      labs(caption="MIT Election Data and Science Lab, 2018, Harvard Dataverse, County Presidential Election Returns 2000-2016\nCOVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")}
ggsave("../../Images/blog_figures/red_blue_coronavirus/counties_percapita.png", width=10, height=6)

left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>%
  left_join(county_party, by=c("fips"="FIPS")) %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  group_by(fips) %>% mutate(cases = (cases - lag(cases))) %>% ungroup() %>%
  count(date, party, wt=cases) %>%
  mutate(n = ifelse(party == "republican", -n, n)) %>%
  {ggplot(.) + aes(x=date, y=n, colour=party, fill=party) +
      geom_area() +
      scale_colour_manual(values=c("democrat"="#33658a", "republican"="#d62839"),
                          labels=c("Counties that voted for Clinton in 2016\t\t",
                                   "Counties that voted for Trump in 2016"),
      aesthetics=c("colour", "fill")) +
      scale_x_date(expand=expansion(mult=c(0.03, 0.05)), date_labels="%B") +
      scale_y_continuous(breaks=c(-1e4, -2e4, 0, 1e4, 2e4, 3e4),
                         labels=c("10 000", "20 000", "0", "10 000", "20 000", "30 000"),
                         name="Total number of new cases daily\t\t   ") +
      ggtitle("New COVID-19 cases occuring daily in 'red' and 'blue' counties over time") +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         legend.text=element_text(size=14),
                         axis.title.x=element_blank()) +
      labs(caption="MIT Election Data and Science Lab, 2018, Harvard Dataverse, County Presidential Election Returns 2000-2016\nCOVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University")}
ggsave("../../Images/blog_figures/red_blue_coronavirus/total_newcases.png", width=10, height=6)


left_join(county_populations %>% mutate(fips = as.integer(fips)),
          county_party, by=c("fips"="FIPS")) %>%
  na.omit() %>%
  mutate(party = fct_rev(party)) %>%
          {
            ggplot(.) +
              aes(x=population_density, fill=party, colour=party) +
              geom_density(aes(y=..count..), alpha=0.75, size=1.5) +
              xlab("People per square mile") + ylab("Number of counties") +
              ggtitle("County population densities by electoral candidate preference in 2016") +
              scale_colour_manual(values=c("republican"="#d62839", "democrat"="#33658a"),
                                  labels=c("Counties that voted for Trump in 2016",
                                           "Counties that voted for Clinton in 2016"),
                                  aesthetics=c("colour", "fill")) +
              scale_x_log10() +
              theme_bw() + theme(legend.title=element_blank(),
                                 legend.position="bottom",
                                 legend.text=element_text(size=12))
          }
ggsave("../../Images/blog_figures/red_blue_coronavirus/county_densities.png", width=7, height=5)



left_join(county_populations %>% mutate(fips = as.integer(fips)),
          cases_by_county, by=c("fips")) %>% left_join(county_party, by=c("fips"="FIPS")) %>%
  mutate(date = mdy(date)) %>%
  filter(date >= mdy("02-29-2020")) %>%
  group_by(fips) %>% mutate(cases = (cases - lag(cases))/population) %>% ungroup() %>% na.omit() %>%
  {
    ggplot(.) +
      aes(x=date, y=cases, colour=party) +
      geom_point(alpha=0.1) +
      geom_smooth() +
      ylab("Number of new per capita cases daily") +
      ggtitle("Number of new COVID-19 cases occuring daily in 'red' and 'blue' counties over time",
              "Number of new cases normalized according to county population") +
      scale_colour_manual(values=c("democrat"="#33658a", "republican"="#d62839"),
                          labels=c("Counties that voted for Clinton in 2016",
                                   "Counties that voted for Trump in 2016")) +
      scale_y_log10() +
      theme_bw() + theme(legend.title=element_blank(),
                         legend.position="bottom",
                         legend.text=element_text(size=12),
                         axis.title.x=element_blank())}
