Minnesota COVID Report
================

Report last run: 2023-03-03 21:39:41

## Introduction

This is an example report that uses COVID-19 data from the New York
Times to illustrate the use of automation processes.

First, we load some necessary libraries, define some key variables, then
read in the data:

``` r
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(forcats)
library(knitr)

LAG_DAYS <- 7
POP_DENOM <- 100000

## County populations (read from a local data file in this repo)
pops <- read_csv("countypop_us.csv")

## COVID-19 case counts from the NYTimes (read from the web; updated daily)
county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2023.csv")

rate_data <- county_data %>%
  select(date, state, county, cases) %>%
  mutate(date = ymd(date)) %>%
  left_join(pops, by = c("state", "county")) %>%
  group_by(state, county) %>%
  mutate(cases_lag = lag(cases, LAG_DAYS),
         totalcases_last = cases - cases_lag) %>%
  ungroup() %>%
  mutate(rate_last = totalcases_last / pop * POP_DENOM)
```

### Minnesota

Here is a plot of COVID-19 rates since Jan. 1, 2023 in the 10 most
populous Minnesota counties:

``` r
## Identify the top 10 most populous counties
top10_pop <- pops %>% filter(state == "Minnesota") %>%
  arrange(desc(pop)) %>%
  slice(1:10) %>%
  mutate(county = factor(county))

## Make the plot
rate_data %>%
    filter(state == "Arizona", 
         county %in% top10_pop$county) %>%
  ggplot(aes(x = date, y = rate_last, color = county)) +
  geom_line(linewidth = 2) +
  xlab(NULL) +
  ylab("7-day COVID-19 case total per 100,000 population") +
  scale_color_discrete(name = "") +
  theme_minimal() +
  ggtitle("COVID-19 rates for the ten most populous Minnesota counties", 
          subtitle = paste("Latest data:", max(rate_data$date)))
```

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

### United States

The following plot shows the distribution of current COVID-19 rates by
county for each state in the United States. The x-axis is truncated at a
7-day rate of 500 per 100,000 people for improved readability.

``` r
rate_data %>%
  filter(date == max(date),
         !is.na(rate_last)) %>%
  mutate(state = fct_reorder(factor(state), -rate_last, median, na.rm = TRUE)) %>%
  ggplot(aes(x = rate_last, y = state)) +
  geom_boxplot() +
  xlim(c(0,500)) +
  xlab("7-day COVID-19 total cases per 100,000 people") +
  ylab(NULL) +
  theme_minimal() +
  ggtitle("Distribution of county-level COVID-19 case rates, by state",
          subtitle = paste("Latest data:", max(rate_data$date)))
```

![](README_files/figure-commonmark/unnamed-chunk-3-1.png)

Here is a table of the 20 counties with the highest 7-day per 100,000
COVID-19 case rates:

``` r
rate_data %>%
  filter(date == max(date),
         !is.na(rate_last)) %>%
  select(county, state, pop, rate_last) %>%
  rename(covid_rate = rate_last) %>%
  mutate(covid_rate = round(covid_rate)) %>%
  slice(1:20) %>%
  knitr::kable()
```

| county    | state   |    pop | covid_rate |
|:----------|:--------|-------:|-----------:|
| Autauga   | Alabama |  55869 |         48 |
| Baldwin   | Alabama | 223234 |         56 |
| Barbour   | Alabama |  24686 |         93 |
| Bibb      | Alabama |  22394 |         89 |
| Blount    | Alabama |  57826 |         99 |
| Bullock   | Alabama |  10101 |         50 |
| Butler    | Alabama |  19448 |        103 |
| Calhoun   | Alabama | 113605 |         97 |
| Chambers  | Alabama |  33254 |        102 |
| Cherokee  | Alabama |  26196 |         69 |
| Chilton   | Alabama |  44428 |         61 |
| Choctaw   | Alabama |  12589 |         16 |
| Clarke    | Alabama |  23622 |         64 |
| Clay      | Alabama |  13235 |        166 |
| Cleburne  | Alabama |  14910 |        201 |
| Coffee    | Alabama |  52342 |        117 |
| Colbert   | Alabama |  55241 |        119 |
| Conecuh   | Alabama |  12067 |         83 |
| Coosa     | Alabama |  10663 |        113 |
| Covington | Alabama |  37049 |         78 |
