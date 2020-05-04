library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)

assign_periods <- function(d) {
	mutate(d, period = case_when (
		date < "2020-03-16" ~ "1H Mar",
		date >= "2020-03-16" & date < "2020-04-01" ~ "2H Mar",
		date >= "2020-04-01" & date < "2020-04-16" ~ "1H Apr",
		date >= "2020-04-16" & date < "2020-05-01" ~ "1H Apr",
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
factor(nyc$period)

la <- 
	read_csv("./covid-19-data/us-counties.csv") %>%
	filter(county == "Los Angeles") %>%
	filter(date >= "2020-03-01") %>%
	assign_periods %>%
	select(date, period, cases, deaths, county)
factor(la$period)


all_six_cty <- sixcty %>%
	group_by(date, period) %>%
	summarize(cases=sum(cases),deaths=sum(deaths)) %>%
	mutate(county = "Bay Area Six-County Region")

nyc_la_bayarea <- bind_rows(nyc,la, all_six_cty)
