library(rvest)
library(tidyverse)
library(dplyr)


# extracting html from the web link
html <- read_html("https://www.relianceiccrankings.com/ranking/womenodi/batting/")

# extract the html table
table <- html %>% html_table()  # this is a list


table <-table[[1]]   


# extract country name from "alt" attribute in img tags
countries <- html %>% html_elements("table td img") %>% html_attr("alt")

# set 4th column to these countries
table[4] = countries

# change name of column 1 to "Ranking" and column 4 to "Country"
colnames(table)[1]="Ranking"
colnames(table)[4] = "Country"


# as tibble

batting <- as_tibble(table)

# extracting INDIAN players(i.e their names and Ranking)
batting %>% filter(Country == "IND") %>% select(Ranking,Name)

# counts no. of players country wise
batting %>% group_by(Country) %>% summarise(Number_of_players = sum(Country == Country))

# Decs. order of average Ranking countrywise
batting %>% group_by(Country) %>% summarise(Ranking = mean(Ranking)) %>% arrange(desc(Ranking))


# vector of asian countries
asian_countries <- c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BRN", "KHM", "CHN", "IND", "IDN", "IRN", "IRQ", "ISR", "JPN", "JOR", "KAZ", "KWT", "KGZ", "LAO", "LBN", "MYS", "MDV", "MNG", "MMR", "NPL", "PRK", "OMN", "PAK", "PSE", "PHL", "QAT", "SAU", "SGP", "KOR", "LKA", "SYR", "TWN", "TJK", "THA", "TUR", "TKM", "ARE", "UZB", "VNM", "YEM")


# setting each row to be Asian or non-Asian and counting no. of Asian vs non-asian players
batting %>%
  mutate(isAsian = ifelse(Country %in% asian_countries, "Asian", "Non-Asian")) %>%
  select(isAsian,everything()) %>% group_by(isAsian) %>% summarize(count = sum(isAsian == isAsian))

# Average rating of asian vs non-asian countries
batting %>%
  mutate(isAsian = ifelse(Country %in% asian_countries, "Asian", "Non-Asian")) %>%
  select(isAsian,everything()) %>% group_by(isAsian) %>% summarize(avg_rating = mean(Rating))

