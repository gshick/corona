
library(RCurl)
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(gganimate)

# Pull data from Johns Hopkins GitHub Repo
c0 <- data.frame(read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), header=T), stringsAsFactors=FALSE) %>% 
    mutate(country=as.character(Country.Region))

# Summarize by country and get the top 10 countries by confirmed cases
c1 <- c0 %>% 
  group_by(country) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-Lat, -Long)   

# Tidy da ta and convert dates
c2 <- melt(data = c1, id.vars = "country") %>% 
  rename(c_date=variable,confirmed=value) %>% 
  mutate(c_date=gsub("X","0",as.character(c_date))) %>% 
  arrange(country,c_date)

c2$c_date <- make_date(year = 2020, month = substr(c2$c_date,1,2), day = substr(c2$c_date,4,5))


# Get first date each country passed 100 confirmed cases
c3 <- c2 %>%
  group_by(country, c_date) %>% 
  filter(confirmed >= 100) %>% 
  summarise(flag = min(which(confirmed > 100))) %>% 
  filter(row_number() == 1) %>% 
  mutate(elap_days=Sys.Date()-c_date) %>% 
  rename(init_dt = c_date)

c4 <- c2 %>% 
  filter(c_date==max(c_date)) %>% 
  group_by(country) %>% 
  summarise(cur_confirmed=max(confirmed)) %>% 
  arrange(-cur_confirmed) %>% 
  top_n(50)
  

c5 <- left_join(c2, select(c3,country,init_dt), by.x = "country", by.y = "country") %>% 
  inner_join(.,c4, by.x = "country", by.y = "country") %>% 
  mutate(days_since_100=c_date-init_dt)

c6 <- c5 %>% 
  group_by(country,days_since_100) %>% 
  summarize(run_con=sum(confirmed)) %>%
  filter(days_since_100 >= 0)

c6$country<-as.factor(c6$country)

# Create Chart
############################################################################## 
p <- ggplot(data=c5, aes(x=days_since_100, y=run_con, color=country)) +
  geom_line() +
  theme_minimal() +
  scale_y_log10(labels = c(0, 100, '1,000', '10,000', '100,000'),
                breaks = c(0, 100, 1000, 10000, 100000)) +
  scale_x_continuous(labels = as.numeric(seq(0, 70, by=10)),
                     breaks = as.numeric(seq(0, 70, by=10))) + 
  labs(title = "Total confirmed cases of COVID-19",
       subtitle = "The number of confirmed cases is lower than the number of total cases. The main reason for this is limited testing.",
       x="Days since the 100th total confirmed case", y="Total Confirmed Cases")

anim <- p + 
  transition_states(days_since_100,
                    transition_length = 2,
                    state_length = 1)

anim
    

  
  