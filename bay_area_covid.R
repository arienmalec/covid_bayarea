library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)


assign_periods <- function(d) {
	mutate(d, period = case_when (
		date < "2020-03-16" ~ "Mar 1H",
		date >= "2020-03-16" & date < "2020-04-01" ~ "Mar 2H",
		date >= "2020-04-01" & date < "2020-04-16" ~ "Apr 1H",
		date >= "2020-04-16" & date < "2020-05-01" ~ "Apr 2H",
		date >= "2020-05-01" & date < "2020-05-16" ~ "May 1H",
		date >= "2020-05-16" & date < "2020-05-01" ~ "May 2H",
		TRUE				~ "ERROR"))
}

six_counties <-
	c("Santa Clara", "San Francisco","Marin",
	"Alameda","Contra Costa", "San Mateo")

sixcty <-
	read_csv("./covid-19-data/us-counties.csv") %>%
	filter(state == "California") %>%
	filter(county %in% six_counties) %>%
	filter(date >= "2020-03-01") %>%
	assign_periods %>%
	select(date, period, cases, deaths, county)
factor(sixcty$period)

nyc <-
	read_csv("./covid-19-data/us-counties.csv") %>%
	filter(county == "New York City") %>%
	filter(date >= "2020-03-01") %>%
	assign_periods %>%
	select(date, period, cases, deaths, county)

nyc$new_cases <- nyc$cases - lag(nyc$cases, default=0)
nyc$new_deaths <- nyc$deaths - lag(nyc$deaths, default=0)
factor(nyc$period)

la <- 
	read_csv("./covid-19-data/us-counties.csv") %>%
	filter(county == "Los Angeles") %>%
	filter(date >= "2020-03-01") %>%
	assign_periods %>%
	select(date, period, cases, deaths, county)


la$new_cases <- la$cases - lag(la$cases, default=0)
la$new_deaths <- la$deaths - lag(la$deaths, default=0)
factor(la$period)


all_six_cty <- sixcty %>%
    group_by(date, period) %>%
    summarize(cases=sum(cases),deaths=sum(deaths)) %>%
    mutate(county = "Bay Area Six-County Region")

all_six_cty$new_cases <- all_six_cty$cases - lag(all_six_cty$cases, default=0)
all_six_cty$new_deaths <- all_six_cty$deaths - lag(all_six_cty$deaths, default=0)


nyc_la_bayarea <- bind_rows(nyc,la, all_six_cty)

p_case_sm <-
    ggplot(sixcty, aes(x = date, y = cases, fill = period, color = county)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10() +
    ggtitle("COVID-19 Case Totals by County for Bay Area 6-County Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()
    
p_death_sm <-
    ggplot(sixcty, aes(x = date, y = deaths, fill = period, color = county)) +
    geom_smooth(method = lm) +
    geom_point() +
    scale_y_log10() +
    ggtitle("COVID-19 Death Totals by County for Bay Area 6-County Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()
    
p_all_cases <-
    ggplot(all_six_cty, aes(x = date, y = cases, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Case Totals for Bay Area 6-Country Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()
    
p_all_deaths <-
    ggplot(all_six_cty, aes(x = date, y = deaths, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Death Totals for Bay Area 6-Country Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()

p_bay_la_nyc_deaths <-
    ggplot(nyc_la_bayarea, aes(x = date, y = deaths, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels = scales::comma) +
    ggtitle("COVID-19 Death Totals for Bay Area 6-Country Region, LA & NYC", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()

p_bay_la_nyc_cases <-
    ggplot(nyc_la_bayarea, aes(x = date, y = cases, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels=scales::comma) +
    ggtitle("COVID-19 Case Totals for Bay Area 6-Country Region, LA & NYC", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()

p_bay_la_nyc_new_cases <-
    ggplot(nyc_la_bayarea, aes(x = date, y = new_cases, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels=scales::comma) +
    ggtitle("COVID-19 New Cases By Day for Bay Area 6-Country Region, LA & NYC", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()

p_bay_la_nyc_new_deaths <-
    ggplot(nyc_la_bayarea, aes(x = date, y = new_deaths, color = county, fill = period)) +
    geom_point() +
    geom_smooth(method = lm) +
    scale_y_log10(labels=scales::comma) +
    ggtitle("COVID-19 New Deaths By Day for Bay Area 6-Country Region, LA & NYC", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
    theme_few()


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

png("~/Desktop/bay_area_la_nyc_covid_new_deaths.png")
print(p_bay_la_nyc_new_deaths)
dev.off()