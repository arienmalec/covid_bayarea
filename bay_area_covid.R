library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)

six_counties <-
	c("Santa Clara", "San Francisco","Marin",
	"Alameda","Contra Costa", "San Mateo")

sixcty <-
	read_csv("./data/covid-19-data/us-counties.csv") %>%
	filter(state == "California") %>%
	filter(date >= "2020-03-01") %>%
	filter(county %in% six_counties) %>%
	mutate(period = case_when (
		date < "2020-03-16" ~ "pre-shelter",
		date > "2020-03-29" ~ "shelter+2w",
		TRUE				~ "shelter"))
factor(sixcty$period)

nyc <-
	read_csv("./covid-19-data/us-counties.csv") %>%
	filter(county == "New York City") %>%
	filter(date >= "2020-03-01") %>%
	mutate(period = case_when (
		date < "2020-03-16" ~ "pre-shelter",
		date > "2020-03-29" ~ "shelter+2w",
		TRUE				~ "shelter")) %>%
	select(date, period, cases, deaths, county)
factor(nyc$period)

la <- 
	read_csv("./covid-19-data/us-counties.csv") %>%
	filter(county == "Los Angeles") %>%
	filter(date >= "2020-03-01") %>%
	mutate(period = case_when (
		date < "2020-03-16" ~ "pre-shelter",
		date > "2020-03-29" ~ "shelter+2w",
		TRUE				~ "shelter")) %>%
	select(date, period, cases, deaths, county)
factor(la$period)


all_six_cty <- sixcty %>%
	group_by(date, period) %>%
	summarize(cases=sum(cases),deaths=sum(deaths)) %>%
	mutate(county = "Bay Area Six-County Region")

nyc_la_bayarea <- bind_rows(nyc,la, all_six_cty)


p_case_sm <- 
	ggplot(sixcty, aes(x=date, y=cases, fill=period, color=county)) +
	geom_point() +
	geom_smooth(method = lm) +
	scale_y_log10() +
	ggtitle("COVID-19 Case Totals by County for Bay Area 6-County Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
	theme_few()
	
p_death_sm <- 
	ggplot(sixcty, aes(x=date, y=deaths, fill=period, color=county)) +
	geom_smooth(method=lm) +
	geom_point() +
	scale_y_log10() +
	ggtitle("COVID-19 Death Totals by County for Bay Area 6-County Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
	theme_few()
	
p_all_cases <-
	ggplot(all_six_cty, aes(x=date, y=cases, fill=period)) +
	geom_point() +
	geom_smooth(method = lm) +
	scale_y_log10() +
	ggtitle("COVID-19 Case Totals for Bay Area 6-Country Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
	theme_few()
	
p_all_deaths <-
	ggplot(all_six_cty, aes(x=date, y=deaths, fill=period)) +
	geom_point() +
	geom_smooth(method = lm) +
	scale_y_log10() +
	ggtitle("COVID-19 Death Totals for Bay Area 6-Country Region", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
	theme_few()

p_bay_la_nyc_deaths <-
	ggplot(nyc_la_bayarea, aes(x=date, y=deaths, color=county, fill=period)) +
	geom_point() +
	geom_smooth(method = lm) +
	scale_y_log10() +
	ggtitle("COVID-19 Death Totals for Bay Area 6-Country Region, LA & NYC", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
	theme_few()

p_bay_la_nyc_cases <-
	ggplot(nyc_la_bayarea, aes(x=date, y=cases, color=county, fill=period)) +
	geom_point() +
	geom_smooth(method = lm) +
	scale_y_log10() +
	ggtitle("COVID-19 Case Totals for Bay Area 6-Country Region, LA & NYC", subtitle = "Data from NYT (https://github.com/nytimes/covid-19-data.git)") +
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