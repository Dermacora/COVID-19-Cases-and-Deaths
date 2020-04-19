# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Get PHAC dataset URL
Canada_url <- paste("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv")

# Download and prepare data
data <-
  read_csv(Canada_url)

min(dmy(data$date)) # First date available
max(dmy(data$date)) # Latest date available

Canada <-
  data %>%
    filter(!prname %in% c("Repatriated travellers", "Nunavut")) %>% 
    mutate(date = dmy(date)) %>%
    arrange(prname, date) %>% 
    group_by(prname) %>% 
    mutate(incident_cases = c(0, diff(numconf)), country_region = "Canada (separate plots)") %>% 
    ungroup()    %>% 
  rename(Date = date, cumulative_cases = numconf, deaths = numdeaths)

# Rename and transform variables to prepare for Shiny app
Canada <-
  Canada %>%
  mutate(
    infected_per_100000 = case_when(prname == "Canada" ~ (cumulative_cases/37590000)*100000,
                                    prname == "Alberta" ~ (cumulative_cases/4335768)*100000,
                                    prname == "British Columbia" ~ (cumulative_cases/5031893)*100000,
                                    prname == "Manitoba" ~ (cumulative_cases/1361997)*100000,
                                    prname == "New Brunswick" ~ (cumulative_cases/772790)*100000,
                                    prname == "Newfoundland and Labrador" ~ (cumulative_cases/524053)*100000,
                                    prname == "Northwest Territories" ~ (cumulative_cases/45047)*100000,
                                    prname == "Nova Scotia" ~ (cumulative_cases/965249)*100000,
                                    prname == "Nunavut" ~ (cumulative_cases/38341)*100000,
                                    prname == "Ontario" ~ (cumulative_cases/14441694)*100000,
                                    prname == "Prince Edward Island" ~ (cumulative_cases/155111)*100000,
                                    prname == "Quebec" ~ (cumulative_cases/8429241)*100000,
                                    prname == "Saskatchewan" ~ (cumulative_cases/1170028)*100000,
                                    prname == "Yukon" ~ (cumulative_cases/40692)*100000,
                                    TRUE ~ NaN # This is an else statement
     )
  ) %>%
  mutate(
    inc_scaling = case_when(prname == "Canada" ~ 200,
                            prname == "Alberta" ~ 25,
                            prname == "British Columbia" ~ 25,
                            prname == "Manitoba" ~ 5,
                            prname == "New Brunswick" ~ 2,
                            prname == "Newfoundland and Labrador" ~ 10,
                            prname == "Northwest Territories" ~ 1,
                            prname == "Nova Scotia" ~ 5,
                            prname == "Nunavut" ~ 1,
                            prname == "Ontario" ~ 50,
                            prname == "Prince Edward Island" ~ 1,
                            prname == "Quebec" ~ 200,
                            prname == "Saskatchewan" ~ 5,
                            prname == "Yukon" ~ 1,
                            TRUE ~ NaN # This is an else statement
    )
  ) %>%
  mutate(
    cum_scaling = case_when(prname == "Canada" ~ 2000,
                            prname == "Alberta" ~ 200,
                            prname == "British Columbia" ~ 200,
                            prname == "Manitoba" ~ 25,
                            prname == "New Brunswick" ~ 10,
                            prname == "Newfoundland and Labrador" ~ 20,
                            prname == "Northwest Territories" ~ 1,
                            prname == "Nova Scotia" ~ 25,
                            prname == "Nunavut" ~ 1,
                            prname == "Ontario" ~ 500,
                            prname == "Prince Edward Island" ~ 2,
                            prname == "Quebec" ~ 1000,
                            prname == "Saskatchewan" ~ 25,
                            prname == "Yukon" ~ 1,
                            TRUE ~ NaN # This is an else statement
    )
  ) %>%
  mutate(
    death_scaling = case_when(prname == "Canada" ~ 50,
                              prname == "Alberta" ~ 5,
                              prname == "British Columbia" ~ 5,
                              prname == "Manitoba" ~ 1,
                              prname == "New Brunswick" ~ 1,
                              prname == "Newfoundland and Labrador" ~ 1,
                              prname == "Northwest Territories" ~ 1,
                              prname == "Nova Scotia" ~  1,
                              prname == "Nunavut" ~  1,
                              prname == "Ontario" ~ 20,
                              prname == "Prince Edward Island" ~  1,
                              prname == "Quebec" ~ 20,
                              prname == "Saskatchewan" ~  1,
                              prname == "Yukon" ~  1,
                              TRUE ~ NaN # This is an else statement
    )
  ) %>%
  mutate(
    inc_per_100000 = case_when(prname == "Canada" ~ (incident_cases/37590000)*100000,
                                  prname == "Alberta" ~ (incident_cases/4335768)*100000,
                                  prname == "British Columbia" ~ (incident_cases/5031893)*100000,
                                  prname == "Manitoba" ~ (incident_cases/1361997)*100000,
                                  prname == "New Brunswick" ~ (incident_cases/772790)*100000,
                                  prname == "Newfoundland and Labrador" ~ (incident_cases/524053)*100000,
                                  prname == "Northwest Territories" ~ (incident_cases/45047)*100000,
                                  prname == "Nova Scotia" ~ (incident_cases/965249)*100000,
                                  prname == "Nunavut" ~ (incident_cases/38341)*100000,
                                  prname == "Ontario" ~ (incident_cases/14441694)*100000,
                                  prname == "Prince Edward Island" ~ (incident_cases/155111)*100000,
                                  prname == "Quebec" ~ (incident_cases/8429241)*100000,
                                  prname == "Saskatchewan" ~ (incident_cases/1170028)*100000,
                                  prname == "Yukon" ~ (incident_cases/40692)*100000,
                                  TRUE ~ NaN # This is an else statement
    )
  ) %>%
  mutate(
    deaths_per_100000 = case_when(prname == "Canada" ~ (deaths/37590000)*100000,
                                  prname == "Alberta" ~ (deaths/4335768)*100000,
                                  prname == "British Columbia" ~ (deaths/5031893)*100000,
                                  prname == "Manitoba" ~ (deaths/1361997)*100000,
                                  prname == "New Brunswick" ~ (deaths/772790)*100000,
                                  prname == "Newfoundland and Labrador" ~ (deaths/524053)*100000,
                                  prname == "Northwest Territories" ~ (deaths/45047)*100000,
                                  prname == "Nova Scotia" ~ (deaths/965249)*100000,
                                  prname == "Nunavut" ~ (deaths/38341)*100000,
                                  prname == "Ontario" ~ (deaths/14441694)*100000,
                                  prname == "Prince Edward Island" ~ (deaths/155111)*100000,
                                  prname == "Quebec" ~ (deaths/8429241)*100000,
                                  prname == "Saskatchewan" ~ (deaths/1170028)*100000,
                                  prname == "Yukon" ~ (deaths/40692)*100000,
                                  TRUE ~ NaN # This is an else statement
    )
  ) %>%
  select(-c(inc_scaling, cum_scaling, death_scaling)) %>%
  select(-c(prnameFR, numtested, numtoday, numtotal, numprob, pruid, numrecover, percentrecover, ratetested, percentoday)) %>%
  mutate(prname = ifelse(prname == "Canada", "All", as.character(prname)), mycolour = prname, infected_per_100000 = round(infected_per_100000, 3)) %>%
  mutate(incident_cases = ifelse(incident_cases <0, 0, as.numeric(incident_cases))) %>%
  mutate(deaths_per_100000 = round(deaths_per_100000, 3)) %>%
  mutate(inc_per_100000 = round(inc_per_100000, 3))

Canada <-
  Canada %>%
  mutate(
    mycolour = case_when(prname == "All" ~ "CA",
                       prname == "Alberta" ~ "AB",
                       prname == "British Columbia" ~ "BC",
                       prname == "Manitoba" ~ "MB",
                       prname == "New Brunswick" ~ "NB",
                       prname == "Newfoundland and Labrador" ~ "NL",
                       prname == "Northwest Territories" ~ "NT",
                       prname == "Nova Scotia" ~ "NS",
                       prname == "Nunavut" ~ "NU",
                       prname == "Ontario" ~ "ON",
                       prname == "Prince Edward Island" ~ "PE",
                       prname == "Quebec" ~ "QC",
                       prname == "Saskatchewan" ~ "SK",
                       prname == "Yukon" ~ "YT",
                       TRUE ~ "??" # This is an else statement
    )
  ) %>%
  mutate(countryShort = ifelse(prname == "All", "Canada", "Province"))

Canada_grouped <-
Canada %>%
  filter(!prname %in% c("CA", "All")) %>%
  mutate(mycolour = mycolour) %>%
  select(-countryShort)

Canada_overall <-
  Canada %>%
  filter(prname %in% c("CA", "All")) %>%
  mutate(prname = ifelse(prname == "All", "Canada", prname)) %>%
  mutate(mycolour = mycolour) %>%
  select(-countryShort)


write.csv(Canada, "Canada.csv")
write.csv(Canada_grouped, "Canada_grouped.csv")
write.csv(Canada_overall, "Canada_overall.csv")