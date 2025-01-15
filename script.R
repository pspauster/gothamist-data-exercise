library(tidyverse)
library(RSocrata)

#pull using API
arrest_data <- read_csv(URLencode("https://data.cityofnewyork.us/resource/uip8-fykc.csv?$limit=10000000"))

#pull historical data using an API and limit the scope to improve efficiency
arrest_data_past <- read_csv(URLencode(
  "https://data.cityofnewyork.us/resource/8h9b-rp9u.csv?$where=arrest_date > '2018-01-01'&$limit=10000000"
))

#combine ytd and historical
arrests_all <- bind_rows(arrest_data, arrest_data_past)

all_clean <- arrests_all %>% 
  mutate(arrest_year = year(arrest_date),
         arrest_month = floor_date(arrest_date, "month"),
         arrest_halfyear = floor_date(arrest_date, "6 months"))

#summarize for various cuts to get percent change numbers
year_sum <- all_clean %>% 
  filter(jurisdiction_code <3) %>% 
  group_by(arrest_year) %>% 
  summarize(n = n())

halfyear_sum <- all_clean %>% 
  filter(jurisdiction_code <3) %>% 
  group_by(arrest_halfyear) %>% 
  summarize(n = n())

halfyear_sum_system <- all_clean %>% 
  filter(jurisdiction_code <3) %>% 
  group_by(arrest_halfyear, jurisdiction_code) %>% 
  summarize(n = n())

halfyear_sum_system <- all_clean %>% 
  filter(jurisdiction_code <3) %>% 
  group_by(arrest_halfyear, jurisdiction_code) %>% 
  summarize(n = n())

year_sum_system <- all_clean %>% 
  filter(jurisdiction_code <3) %>% 
  group_by(arrest_year, jurisdiction_code) %>% 
  summarize(n = n()) 

year_sum_system %>% 
  ggplot()+
  geom_line(aes(x = arrest_year, y = n, color = as.factor(jurisdiction_code)))

month_sum_system <- all_clean %>% 
  filter(jurisdiction_code <3) %>% 
  group_by(arrest_month, jurisdiction_code) %>% 
  summarize(n = n()) 

month_sum_system %>% 
  ggplot()+
  geom_line(aes(x = arrest_month, y = n, color = as.factor(jurisdiction_code)))

month_sum_fa <- all_clean %>% 
  filter(jurisdiction_code <3, ofns_desc == "FELONY ASSAULT") %>% 
  group_by(arrest_month) %>% 
  summarize(n = n()) 

halfyear_sum_fa <- all_clean %>% 
  filter(jurisdiction_code <3, ofns_desc == "FELONY ASSAULT") %>% 
  group_by(arrest_halfyear) %>% 
  summarize(n = n()) 

halfyear_sum_fa_system <- all_clean %>% 
  filter(jurisdiction_code <3, ofns_desc == "FELONY ASSAULT") %>% 
  group_by(arrest_halfyear, jurisdiction_code) %>% 
  summarize(n = n()) 

#visualize felony assaults and output some data for a chart

month_sum_fa %>% 
  ggplot()+
  geom_line(aes(x = arrest_month, y = n))

month_sum_fa_system <- all_clean %>% 
  filter(jurisdiction_code <3, ofns_desc == "FELONY ASSAULT") %>% 
  group_by(arrest_month, jurisdiction_code) %>% 
  summarize(n = n()) 

month_sum_fa_system %>% 
  ggplot()+
  geom_line(aes(x = arrest_month, y = n, color = as.factor(jurisdiction_code)))

month_sum_fa_system %>% pivot_wider(id_cols = arrest_month, names_from = "jurisdiction_code", values_from = "n") %>% 
  write_csv("felony_assault_datawrapper.csv")


