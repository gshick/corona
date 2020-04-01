
library(RCurl)
library(rvest)
library(dplyr)
library(cowplot)
library(plotly)
library(reshape2)
library(stringr)
library(lubridate)
library(purrr)

# Pull data from Johns Hopkins GitHub Repo
c0 <- data.frame(read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), header=T), stringsAsFactors=FALSE) %>% 
    mutate(country=as.character(Country.Region))

# Summarize by country and get the top 10 countries by confirmed cases
c1 <- c0 %>% 
  group_by(country) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-Lat, -Long)   

# Tidy data and convert dates
c2 <- melt(data = c1, id.vars = "country") %>% 
  rename(c_date=variable,confirmed=value) %>% 
  mutate(c_date=gsub("X","0",as.character(c_date))) %>% 
  arrange(country,c_date)

c2$c_date <- make_date(year = 2020, month = substr(c2$c_date,1,2), day = substr(c2$c_date,4,5))

c3 <- c2 %>%
  group_by(country, c_date) %>% 
  summarise(flag = min(which(confirmed > 100))) %>% 
  first(c2$country,c2$c_date)
  
  
  
  filter(min(which(confirmed > 100)))
         
# Why doesn't this work?
# c2$c_date <- as.Date(strptime(c2$c_date,format="%d.%m.%y"),format="%y-%m-%d")

# Find date when country first hit 100 confirmed cases


