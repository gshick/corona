library(RCurl)
library(rvest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(gganimate)
library(gifski)
library(ggrepel)
library(scales)


#Pull data from Johns Hopkins GitHub Repo
c0 <- data.frame(read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), header=T), stringsAsFactors=FALSE) %>% 
  mutate(state=as.character(Province_State))

#Summarize by state
c1 <- c0 %>% 
  group_by(state) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-Lat, -Long_, -UID, -code3, -FIPS) %>% 
  filter(!state %in% c("American Samoa", "Diamond Princess", "Grand Princess",
                       "Guam", "Virgin Islands", "Northern Mariana Islands"))

#Tidy data and convert dates
c2 <- melt(data = c1, id.vars = "state") %>% 
  rename(c_date=variable,confirmed=value) %>% 
  mutate(c_date=gsub("X","0",as.character(c_date)))

#Convert string to date
c2$c_date <- make_date(year = 2020, month = substr(c2$c_date,1,2), day = substr(c2$c_date,4,5))

#Order by State and Confirmed Date
c2 <- c2 %>% arrange(state,c_date)

Colorado <-c2 %>% 
  filter(state=="Colorado" & c_date >= "2020-03-01")

#Get Daily Rate for Colorado
CO_Daily <- Colorado %>% 
  group_by(state) %>% 
  mutate(daily = confirmed - lag(confirmed,1))

#Create chart for confirmed cases in Colorado
#######################################################################################
ggplot() +
  geom_line(data = CO_Daily, aes(x=c_date, y=confirmed, group=state), size=1, color="dodgerblue3") +
  geom_bar(data = CO_Daily, aes(x=c_date, y=daily, group=state), stat="identity") +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  scale_x_date(date_breaks = "1 week" , date_labels = "%m/%d/%Y") +
  labs(title = "Total confirmed cases of COVID-19 in Colorado",
       subtitle = "The number of confirmed cases is lower than the number of total cases.",
       x="Date", y="Total Confirmed Cases") + 
  transition_reveal(c_date)

#Create chart for confirmed cases in Colorado
#######################################################################################
ggplot() +
  geom_line(data = CO_Daily, aes(x=c_date, y=confirmed, group=state), size=1, color="dodgerblue3") +
  geom_bar(data = CO_Daily, aes(x=c_date, y=daily, group=state), stat="identity") +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  scale_x_date(date_breaks = "1 week" , date_labels = "%m/%d/%Y") +
  labs(title = "Total confirmed cases of COVID-19 in Colorado",
       subtitle = "The number of confirmed cases is lower than the number of total cases.",
       x="Date", y="Total Confirmed Cases") + 
  transition_reveal(c_date)

#Create chart for confirmed cases in Colorado
#######################################################################################
ggplot() +
  geom_line(data = CO_Daily, aes(x=c_date, y=confirmed, group=state), size=1, color="dodgerblue3") +
  geom_bar(data = CO_Daily, aes(x=c_date, y=daily, group=state), stat="identity") +
  theme_minimal() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  scale_x_date(date_breaks = "1 week" , date_labels = "%m/%d/%Y") +
  labs(title = "Total confirmed cases of COVID-19 in Colorado",
       subtitle = "The number of confirmed cases is lower than the number of total cases.",
       x="Date", y="Total Confirmed Cases") + 
  transition_reveal(c_date)