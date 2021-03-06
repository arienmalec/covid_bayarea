library(covidcast)
library(tidyverse)
     
cc_data <- suppressMessages(
    covidcast_signal(
        data_source = "fb-survey", signal = "smoothed_spent_time_1d",
                     start_day = "2020-01-19", end_day = "2021-02-19",
                     geo_type = "county"))

cc_masked <- suppressMessages(
    covidcast_signal(
        data_source = "fb-survey", signal = "smoothed_others_masked",
                     start_day = "2020-01-19", end_day = "2021-02-19",
                     geo_type = "county"))

ala_cc <-
    cc_data %>%
    filter(geo_value == "06001")

ala_masked_cc <-
    cc_masked %>%
    filter(geo_value == "06001")

la_cc <-
    cc_data %>%
    filter(geo_value == "06037")

la_masked_cc <-
    cc_masked %>%
    filter(geo_value == "06037")

la_p <-
    ggplot(la_cc, aes(x = time_value, y = value)) +
    geom_line()

la_masked_p <-
    ggplot(la_masked_cc, aes(x = time_value, y = value)) +
    geom_line()
    
ala_p <-
    ggplot(ala_cc, aes(x = time_value, y = value)) +
    geom_line()

ala_masked_p <-
    ggplot(ala_masked_cc, aes(x = time_value, y = value)) +
    geom_line()
