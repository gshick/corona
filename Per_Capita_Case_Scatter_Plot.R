

library(RCurl)
library(rvest)
library(dplyr)
library(cowplot)
library(ggplot2)
library(ggrepel)

# Pull data from Johns Hopkins GitHub Repo
c0 <- data.frame(read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-31-2020.csv"), header=T), stringsAsFactors=FALSE)

# Summarize by country and get the top 10 countries by confirmed cases
c1 <- c0 %>% 
  group_by(Country_Region) %>% 
  summarise(Confirmed= sum(Confirmed), Deaths =sum(Deaths),
            Recovered=sum(Recovered), Active=sum(Active)) %>% 
  # arrange(-Confirmed) %>% 
  # top_n(10, Confirmed) %>% 
  rename(country = Country_Region) %>% 
  mutate_if(is.factor, as.character)

c1$country[c1$country == "Korea, South"] <- "S. Korea"
c1$country[c1$country == "United Kingdom"] <- "UK"
c1$country[c1$country == "Czechia"] <- "Czech Republic"
c1$country[c1$country == "Taiwan*"] <- "Taiwan"


# Wikipedia data source
url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)'

# Read HTML code from the URL
webpage <- read_html(url)

# Bring in the table with the population by country
c2 <- html_table(html_nodes(webpage,'table'),fill = T)[[4]]

# Format Wikipedia data to match Johns Hopkins data
c3 <- c2 %>% 
  select(`Country or area`, `Population(1 July 2019)`) %>% 
  rename(country = `Country or area`,  pop_2019 = `Population(1 July 2019)`) %>%
  mutate(country = gsub("\\[.]","", country)) %>% 
  arrange(country)

c3$country[c3$country == "United States"] <- "US"
c3$country[c3$country == "United Kingdom"] <- "UK"
c3$country[c3$country == "South Korea"] <- "S. Korea"

# Calculate the rate per 100K residents
c4 <- merge(x = c1, y = c3, by = "country") %>% 
  mutate(per_con = (as.numeric(Confirmed) / as.numeric(gsub(",","",pop_2019))) * 100000) %>% 
  arrange(-Confirmed) %>% 
  slice(1:25)

c5 <- anti_join(c1, c3, by="country")

# Create chart for raw confirmed cases
ggplot(data=c4, aes(x=per_con, y=Confirmed, color= country)) +
  geom_point() +
  # geom_text(label=c4$country) + 
  geom_text_repel(label=c4$country, force = 30, 
                  direction = "both", nudge_y = 0.5, max.iter = 2000) + 
  scale_x_continuous() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Confirmed Cases per 100K Citizens", y="Total Confirmed Cases")
