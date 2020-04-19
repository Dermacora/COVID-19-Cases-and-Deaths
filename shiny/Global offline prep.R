# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Get Johns Hopkins University dataset URL
jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_covid19_confirmed_global.csv", sep = "")

# Download and prepare data
df <-
    read_csv(jhu_url)

# Rename and transform variables to prepare for Shiny app
df2 <-
    df %>% 
    rename(province = "Province/State", country_region = "Country/Region") %>% 
    pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
    mutate(Date = mdy(Date)) %>%
    filter(country_region %in% c("Germany", "Italy", "Iran", "Korea, South", "United Kingdom", "China", "US", "Spain")) %>% 
    arrange(province, Date) %>% 
    group_by(province) %>% 
    mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
    ungroup()  %>% 
    group_by(country_region, Date) %>% 
    summarise(summed_cumulative_cases = sum(cumulative_cases))  %>% 
    
    mutate(
        infected_per_100000 = case_when(country_region == "China" ~ (summed_cumulative_cases/1386000000)*100000,
                                        country_region == "Germany" ~ (summed_cumulative_cases/82790000)*100000,
                                        country_region == "US" ~ (summed_cumulative_cases/327200000)*100000,
                                        country_region == "Korea, South" ~ (summed_cumulative_cases/51470000)*100000,
                                        country_region == "United Kingdom" ~ (summed_cumulative_cases/66440000)*100000,
                                        country_region == "Italy" ~ (summed_cumulative_cases/60480000)*100000,
                                        country_region == "Spain" ~ (summed_cumulative_cases/46660000)*100000,
                                        country_region == "Iran" ~ (summed_cumulative_cases/81100000)*100000,
                                        TRUE ~ NaN # This is an else statement
        )
    ) %>%
    mutate(incident_cases = c(0, diff(summed_cumulative_cases))) %>%
    
    mutate(
        inc_per_100000 = case_when(country_region == "China" ~ (incident_cases/1386000000)*100000,
                                        country_region == "Germany" ~ (incident_cases/82790000)*100000,
                                        country_region == "US" ~ (incident_cases/327200000)*100000,
                                        country_region == "Korea, South" ~ (incident_cases/51470000)*100000,
                                        country_region == "United Kingdom" ~ (incident_cases/66440000)*100000,
                                        country_region == "Italy" ~ (incident_cases/60480000)*100000,
                                        country_region == "Spain" ~ (incident_cases/46660000)*100000,
                                        country_region == "Iran" ~ (incident_cases/81100000)*100000,
                                        TRUE ~ NaN # This is an else statement
        )
    )

df3 <- df2 %>%
    data.frame()  %>% 
    mutate(country_region = ifelse(country_region == "US", "United States", country_region))  %>% 
    mutate(country_region = ifelse(country_region == "Korea, South", "South Korea", country_region))

df3 <-
df3  %>% 
    mutate(
        inc_scaling = case_when(country_region == "China" ~ 100,
        country_region == "Germany" ~ 1000,
        country_region == "United States" ~ 5000,
        country_region == "United Kingdom" ~ 500,
        country_region == "Italy" ~ 1000,
        country_region == "Spain" ~ 1000,
        country_region == "Iran" ~ 400,
        country_region == "South Korea" ~ 100,
        TRUE ~  NaN 
    ),
    cum_scaling = case_when(country_region == "China" ~ 200,
                            country_region == "Germany" ~ 2000,
                            country_region == "United States" ~ 20000,
                            country_region == "United Kingdom" ~ 5000,
                            country_region == "Italy" ~ 10000,
                            country_region == "Spain" ~ 10000,
                            country_region == "Iran" ~ 5000,
                            country_region == "South Korea" ~ 1000,
                            TRUE ~  NaN 
    )
) %>% select(-c(inc_scaling, cum_scaling))


df3 <- 
    df3  %>% 
    rename(cumulative_cases = summed_cumulative_cases)  %>% 
    mutate(infected_per_100000 = round(infected_per_100000, 3)) %>%
    mutate(inc_per_100000 = round(inc_per_100000, 3)) %>%
    mutate(mycolour = NaN)

# Get Johns Hopkins University dataset URL
jhu_url2 <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_covid19_deaths_global.csv", sep = "")

# Download and prepare data for deaths
deaths <-
    read_csv(jhu_url2) %>% 
    rename(province = "Province/State", country_region = "Country/Region") %>% 
    pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
    mutate(Date = mdy(Date)) %>%
    filter(country_region %in% c("Germany", "Italy", "Iran", "Korea, South", "United Kingdom", "China", "US", "Spain")) %>% 
    arrange(province, Date) %>% 
    group_by(province) %>% 
    mutate(incident_cases = c(0, diff(cumulative_cases))) %>% 
    ungroup()  %>% 
    group_by(country_region, Date) %>% 
    summarise(summed_cumulative_cases = sum(cumulative_cases)) 

deaths <- deaths %>%
    data.frame()  %>% 
    mutate(country_region = ifelse(country_region == "US", "United States", country_region))  %>% 
    mutate(country_region = ifelse(country_region == "Korea, South", "South Korea", country_region))  %>% 
    mutate(
        death_scaling = case_when(country_region == "China" ~ 50,
                                 country_region == "Germany" ~ 500,
                                 country_region == "United States" ~ 2000,
                                 country_region == "United Kingdom" ~ 1000,
                                 country_region == "Italy" ~ 2000,
                                 country_region == "Spain" ~ 2000,
                                 country_region == "Iran" ~ 500,
                                 country_region == "South Korea" ~ 20,
                                 TRUE ~ NaN # This is an else statement
        )
    )  %>% 
    rename(deaths = summed_cumulative_cases)

deaths <- deaths %>%
    mutate(
        deaths_per_100000 = case_when(country_region == "China" ~ (deaths/1386000000)*100000,
                                      country_region == "Germany" ~ (deaths/82790000)*100000,
                                      country_region == "United States" ~ (deaths/327200000)*100000,
                                      country_region == "South Korea" ~ (deaths/51470000)*100000,
                                      country_region == "United Kingdom" ~ (deaths/66440000)*100000,
                                      country_region == "Italy" ~ (deaths/60480000)*100000,
                                      country_region == "Spain" ~ (deaths/46660000)*100000,
                                      country_region == "Iran" ~ (deaths/81100000)*100000,
                                      TRUE ~ NaN # This is an else statement
        )
    ) %>%
    mutate(deaths_per_100000 = round(deaths_per_100000, 3))

df3$deaths <- deaths$deaths

df3$deaths_per_100000 <- deaths$deaths_per_100000


df3 <-
    df3 %>%
    mutate(
        countryShort = case_when(country_region == "China" ~ "China",
                                 country_region == "Germany" ~ "Germany",
                                 country_region == "United States" ~ "USA",
                                 country_region == "United Kingdom" ~ "UK",
                                 country_region == "Italy" ~ "Italy",
                                 country_region == "Spain" ~ "Spain",
                                 country_region == "Iran" ~ "Iran",
                                 country_region == "South Korea" ~ "S. Korea",
                                 TRUE ~ as.character(country_region) # This is an else statement
        )
    )

write.csv(df3, "df3.csv")
