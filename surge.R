library(tidyverse)
library(ggthemes)


mob <- read_csv("2020_US_Region_Mobility_Report.csv")

six_counties_mob <-
    c(
        "Santa Clara County", "San Francisco County", "Marin County",
        "Alameda County", "Contra Costa County", "San Mateo County"
    )


sixctymob <-
    mob %>%
    filter(sub_region_1 == "California") %>%
    filter(sub_region_2 %in% six_counties_mob)

six_counties <-
    c(
        "Santa Clara", "San Francisco", "Marin",
        "Alameda", "Contra Costa", "San Mateo"
    )

add_new_columns <- function(df) {
    df$new_cases <- df$cases - lag(df$cases, default = 0)
    df$new_deaths <- df$deaths - lag(df$deaths, default = 0)
    return(df)
}

nyt <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv")

sixcty <-
    nyt %>%
    filter(state == "California") %>%
    filter(county %in% six_counties) %>%
    filter(date >= "2020-03-01") %>%
    select(date, cases, deaths, county)

all_six_cty <- sixcty %>%
    group_by(date) %>%
    summarize(cases = sum(cases), deaths = sum(deaths)) %>%
    mutate(county = "Bay Area Six-County Region")
all_six_cty <- add_new_columns(all_six_cty)

county_filter <- function (df, fstate, fcounty) {
    new_df <-
        df %>%
        filter(state == fstate) %>%
        filter(county == fcounty) %>%
        filter(date >= "2020-03-01") %>%
        select(date, cases, deaths, county) %>%
        group_by(date)

    new_df <- add_new_columns(new_df)
    return(new_df)
}

king <- county_filter(nyt,"Washington", "King")
la <- county_filter(nyt,"California", "Los Angeles")
sf <- county_filter(nyt, "California", "San Francisco")
alameda <- county_filter(nyt, "California", "Alameda")
scruz <- county_filter(nyt, "California","Santa Cruz")

log_graph <- function(df, graph_title) {
    p <- ggplot(df, aes(x=date, y=new_cases)) +
        geom_point() +
        scale_y_log10(labels = scales::comma) +
        scale_x_date(
            date_breaks = "1 month",
            date_minor_breaks = "1 week",
            date_labels="%B") +
        ggtitle(graph_title,
        subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)"
        ) +
        theme_few() +
        theme(axis.text.x=element_text(angle=60, hjust=1))
    return(p)
}


p_residential <- ggplot (data=sixctymob,
    aes(x=date,
        y=residential_percent_change_from_baseline)) +
    facet_grid(sub_region_2 ~ .) +
    geom_line() +
    scale_x_date(
        date_breaks = "1 month",
        date_minor_breaks = "1 week",
        date_labels="%B") +
    ggtitle("COVID-19 Residential Trends for Bay Area 6-Country Region",
        subtitle = "Data from Google (https://www.google.com/covid19/mobility/)"
    ) +
    theme_few() +
    theme(axis.text.x=element_text(angle=60, hjust=1))



p_all_new_cases <- log_graph(all_six_cty, "COVID-19 New Cases By Day for Bay Area 6-Country Region")
p_sf_new_cases <- log_graph(sf, "COVID-19 New Cases By Day for San Francisco")
p_alameda_new_cases <- log_graph(alameda, "COVID-19 New Cases By Day for Alameda County")
p_la_new_cases <- log_graph(la, "COVID-19 New Cases By Day for Los Angeles")
p_king_new_cases <- log_graph(king, "COVID-19 New Cases By Day for King County")
p_scruz_new_cases <- log_graph(scruz, "COVID-19 New Cases By Day for Santa Cruz County")

png("~/Desktop/bay_area_new_cases_log.png")
print(p_all_new_cases)
dev.off()

png("~/Desktop/sf_new_cases_log.png")
print(p_sf_new_cases)
dev.off()


png("~/Desktop/alameda_new_cases_log.png")
print(p_alameda_new_cases)
dev.off()

png("~/Desktop/la_new_cases_log.png")
print(p_la_new_cases)
dev.off()

png("~/Desktop/king_new_cases_log.png")
print(p_king_new_cases)
dev.off()


png("~/Desktop/santa_cruz_new_cases_log.png")
print(p_scruz_new_cases)
dev.off()


png("~/Desktop/bay_area_mobility.png")
print(p_residential)
dev.off()