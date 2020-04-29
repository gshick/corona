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
  mutate(c_date=gsub("X","0",as.character(c_date))) %>% 
  arrange(state,c_date)

#Convert string to date
c2$c_date <- make_date(year = 2020, month = substr(c2$c_date,1,2), day = substr(c2$c_date,4,5))


options(scipen=10000)

#Create chart for raw confirmed cases
p <- ggplot(data=c2, aes(x=c_date, y=confirmed, group=1), stat="identity") +
  geom_line(size=1, color="dodgerblue3") +
  theme_minimal() +
  theme(text = element_text(size=8),
        legend.position = "none") + 
  facet_wrap( ~ state, ncol=4, scales = "free") +
  transition_time(c_date) + 
  ease_aes('linear')

animate(p, width = 1000, height = 2000, fps = 10, duration = 25, 
        nframes = 300, end_pause = 30, renderer = gifski_renderer())

anim_save("anim.gif")