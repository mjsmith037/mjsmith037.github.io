library(magrittr)
library(tidyverse)
library(lubridate)
library(httr)
library(XML)
library(gganimate)
library(ggrepel)
library(broom)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                 na.strings = "", fileEncoding = "UTF-8-BOM") %>%
  as_tibble() %>%
  mutate(date=dmy(dateRep)) %>%
  select(-dateRep, -geoId, -day, -month, -year, -continentExp)

testing <- read_csv("~/Desktop/full-list-total-tests-for-covid-19.csv") %>%
  mutate(Date = mdy(Date)) %>%
  mutate(Entity)

left_join(data, testing, by=c("countryterritoryCode" = "Code",
                              "date" = "Date")) %>%
  group_by(countriesAndTerritories, popData2018, date) %>%
  transmute(tests=`Total tests`, cases=cumsum(cases)) %>%
  group_by(countriesAndTerritories) %>%
  na.omit() %>%
  filter(cases > 0) %>%
  arrange(date) %>% group_modify(~tail(., 1)) %>%
  {ggplot(.) +
      aes(x=tests/popData2018, y=cases/popData2018, label=str_replace_all(countriesAndTerritories, "_", " ")) +
      geom_text_repel(
        data          = filter(., countriesAndTerritories == "United_States_of_America"),
        segment.size  = 0.5,
        nudge_x = -0.015,
        nudge_y = 0.0001,
        segment.color = "#666666",
        direction     = "both"
      ) +
      geom_point() +
      geom_smooth(method="lm", se=FALSE, colour="#CC3F0C", formula=y~x + 0) +
      coord_trans(x="log", y="log") +
      scale_x_continuous(breaks=c(0.0001, 0.001, 0.01, 0.1),
                         minor_breaks=c(0.0005, 0.005, 0.05, 0.5),
                         labels=c("1 / 10 000", "1 / 1000", "1 / 100", "1 / 10")) +
      scale_y_continuous(breaks=c(0.000001, 0.00001, 0.0001),
                         minor_breaks=c(0.000005, 0.00005, 0.0005),
                         labels=c("1 / 1 000 000", "1 / 100 000", "1 / 10 000")) +
      ggtitle("Cases Detected by Tests Run", "Each normalized by the country's population") +
      xlab("Tests Run Per Capita") + ylab("COVID-19 Cases Per Capita") +
      theme_bw()}
ggsave("../../Images/blog_figures/cases_by_tests.png", width=7, height=5)
