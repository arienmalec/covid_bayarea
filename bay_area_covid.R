library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)

six_counties <-
    c(
        "Santa Clara", "San Francisco", "Marin",
        "Alameda", "Contra Costa", "San Mateo"
    )

nyturl <- "https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"

sixcty <-
    read_csv(nyturl) %>%
    filter(state == "California") %>%
    filter(county %in% six_counties) %>%
    filter(date >= "2020-03-01") %>%
    select(date, cases, deaths, county)

all_six_cty <- sixcty %>%
    group_by(date) %>%
    summarize(cases = sum(cases), deaths = sum(deaths)) %>%
    mutate(county = "Bay Area Six-County Region")

all_six_cty$new_cases <-
    all_six_cty$cases - lag(all_six_cty$cases, default = 0)
all_six_cty$new_deaths <-
    all_six_cty$deaths - lag(all_six_cty$deaths, default = 0)



nyc <-
    read_csv("./covid-19-data/us-counties.csv") %>%
    filter(county == "New York City") %>%
    filter(date >= "2020-03-01") %>%
    assign_periods() %>%
    select(date, period, cases, deaths, county)

nyc$new_cases <- nyc$cases - lag(nyc$cases, default = 0)
nyc$new_deaths <- nyc$deaths - lag(nyc$deaths, default = 0)
factor(nyc$period, levels = periods)

la <-
    read_csv("./covid-19-data/us-counties.csv") %>%
    filter(county == "Los Angeles") %>%
    filter(date >= "2020-03-01") %>%
    assign_periods() %>%
    select(date, period, cases, deaths, county)


la$new_cases <- la$cases - lag(la$cases, default = 0)
la$new_deaths <- la$deaths - lag(la$deaths, default = 0)
factor(la$period, levels = periods)



all_six_cty <- sixcty %>%
    group_by(date, period) %>%
    summarize(cases = sum(cases), deaths = sum(deaths)) %>%
    mutate(county = "Bay Area Six-County Region")
factor(all_six_cty$period, levels = periods)

all_six_cty$new_cases <-
    all_six_cty$cases - lag(all_six_cty$cases, default = 0)
all_six_cty$new_deaths <-
    all_six_cty$deaths - lag(all_six_cty$deaths, default = 0)


nyc_la_bayarea <- bind_rows(nyc, la, all_six_cty)
factor(nyc_la_bayarea$period, levels = periods)

p_case_sm <-
    ggplot(sixcty, aes(x = date, y = cases, fill = period, color = county)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10() +
    ggtitle("COVID-19 Case Totals by County for Bay Area 6-County Region",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_death_sm <-
    ggplot(sixcty, aes(x = date, y = deaths, fill = period, color = county)) +
    geom_smooth(method = lm) +
    geom_point() +
    scale_y_log10() +
    ggtitle("COVID-19 Death Totals by County for Bay Area 6-County Region",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_all_cases <-
    ggplot(all_six_cty, aes(x = date, y = cases, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Case Totals for Bay Area 6-Country Region",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_all_deaths <-
    ggplot(all_six_cty, aes(x = date, y = deaths, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Death Totals for Bay Area 6-Country Region",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_bay_la_nyc_deaths <-
    ggplot(nyc_la_bayarea,
        aes(x = date, y = deaths, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Death Totals for Bay Area 6-Country Region, LA & NYC",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_bay_la_nyc_cases <-
    ggplot(nyc_la_bayarea,
        aes(x = date, y = cases, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Case Totals for Bay Area 6-Country Region, LA & NYC",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_bay_la_nyc_new_cases <-
    ggplot(nyc_la_bayarea,
        aes(x = date, y = new_cases, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 New Cases By Day for Bay Area 6-Country Region, LA & NYC",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_bay_la_nyc_new_cases_linear <-
    ggplot(nyc_la_bayarea,
        aes(x = date, y = new_cases, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("COVID-19 New Cases By Day for Bay Area 6-Country Region, LA & NYC",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()


p_bay_la_nyc_new_deaths <-
    ggplot(nyc_la_bayarea,
        aes(x = date, y = new_deaths, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle(
        "COVID-19 New Deaths By Day for Bay Area 6-Country Region, LA & NYC",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few()

p_all_new_cases <-
    ggplot(all_six_cty, aes(x = date, y = new_cases)) +
    geom_point() +
    scale_y_log10(labels = scales::comma) +
    scale_x_date(
        date_breaks = "1 month",
        date_minor_breaks = "1 week",
        date_labels = "%B") +
    ggtitle("COVID-19 New Cases By Day for Bay Area 6-Country Region",
        subtitle =
            "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
    ) +
    theme_few() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))


png("~/Desktop/bay_area_covid_by_cty.png")
print(p_case_sm)
dev.off()

png("~/Desktop/bay_area_covid_d_by_cty.png")
print(p_death_sm)
dev.off()

png("~/Desktop/bay_area_covid.png")
print(p_all_cases)
dev.off()

png("~/Desktop/bay_area_covid_d.png")
print(p_all_deaths)
dev.off()

png("~/Desktop/bay_area_covid.png")
print(p_all_cases)
dev.off()

png("~/Desktop/bay_area_la_nyc_covid.png")
print(p_bay_la_nyc_cases)
dev.off()

png("~/Desktop/bay_area_la_nyc_covid_d.png")
print(p_bay_la_nyc_deaths)
dev.off()

png("~/Desktop/bay_area_la_nyc_covid_new_cases.png")
print(p_bay_la_nyc_new_cases)
dev.off()

png("~/Desktop/bay_area_la_nyc_covid_new_cases_linear.png")
print(p_bay_la_nyc_new_cases_linear)
dev.off()


png("~/Desktop/bay_area_la_nyc_covid_new_deaths.png")
print(p_bay_la_nyc_new_deaths)
dev.off()