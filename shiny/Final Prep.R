# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

pullDate <- (paste("Last data pull attempt: ", format(Sys.time(), "%B %d, %Y, %H:%M EDT"), sep = ""))

writeLines(pullDate, "pullDate.txt")

df3 <- read.csv("df3.csv")
df3$Date <- date(df3$Date)
df3$prname <- df3$countryShort

Canada <- read.csv("Canada.csv")
Canada$Date <- ymd(Canada$Date)

df4 <- bind_rows(Canada[,-1], df3[,-1])
df4$myLabel <- df4$prname
df4$myLabel[df4$myLabel == "All"] <- "Canada"

df5 <-
  df4 %>%
  filter(countryShort != "Province") %>%
  mutate(country_region = ifelse(country_region == "All", "Canada", country_region)) %>%
  mutate(country_region = ifelse(country_region == "Canada (separate plots)", "Canada", country_region)) %>%
  mutate(mycolour = countryShort, myLabel = 'Global', mysize = NA, myshowlegend = TRUE) %>%
  select(-c(countryShort, prname, country_region))

write.csv(df4, "df4.csv")

write.csv(df5, "df5.csv")