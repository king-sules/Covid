install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

confirmed_cases_worldwide <- read_csv('coronavirus.csv')

#Lets first observe the cumulative cases of all countries by each day.
Con_cases <- confirmed_cases_worldwide %>%
  filter(type=='confirmed') %>%
  group_by(date) %>% 
  summarise(cum_cases=sum(cases))

#Now lets visualize using ggplot
plt_concase <- ggplot(data=Con_cases, aes(x=date,y=cum_cases)) + geom_line() +
  geom_point()
plt_concase

#Next we want to compare China with the rest of the world
#We see a steep incline of China in febuary
is_china <- confirmed_cases_worldwide %>%
  filter(country == "China", type=='confirmed') %>%
  mutate(cumu_cases = cumsum(cases)) %>%
  mutate(date = as.Date(date)) 

is_not_china <- confirmed_cases_worldwide %>%
  filter(country != "China", type=='confirmed') %>%
  mutate(cumu_cases = cumsum(cases))
is_not_china$country <- "Not China"

china_vs_world <- rbind(is_china,is_not_china)

china_vs_world <- china_vs_world %>%
  group_by(country, date) %>%
  summarise(cumu_cases = sum(cases)) %>% 
  ungroup %>% 
  mutate(cumu_cases = cumsum(cumu_cases)) 


plt_china_vs_world <- ggplot(china_vs_world) +  
  geom_line(aes(x=date,y=cumu_cases,group=country,color=country)) + 
  ylab("Cumulative confirmed cases") 

#lets plot china vs. world
plt_china_vs_world

#Lets create important events and attach them to the china vs world plot
who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))
plt_china_vs_world +
  geom_vline(data = who_events, aes(xintercept = date), linetype = 'dashed') +
  geom_text(data = who_events, aes(x = date,label = event), y = 1e5)


#Using china_after_feb15, draw a line plot cum_cases vs. date

china_after_feb15 <- china_vs_world %>%
  filter(country == "China", date >= "2020-02-15")

#Add a smooth trend line using linear regression
ggplot(china_after_feb15, aes(x = date, y = cumu_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")


#It seems China has not been affected post March, so lets see what countries are
countries_cum_cases <- confirmed_cases_worldwide %>%
  filter(type=='confirmed') %>%
  group_by(country) %>% 
  summarise(cum_cases=sum(cases))

#Executing this line we get top 7 countries effected
top_countries <- countries_cum_cases %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7)

#Now lets visualize the cumulative cases of these countries by date
top_countries_cum_cases <- confirmed_cases_worldwide %>%
  filter(country %in% c('Brazil','France', 'Italy','Russia','Spaine', 'United Kingdom',
                        'US'), type == 'confirmed') %>%
  group_by(date,country) %>%
  summarize(cum_cases = sum(cases))

plt_top_countries <- ggplot(top_countries_cum_cases) +  
  geom_line(aes(x=date,y=cum_cases,group=country,color=country)) + 
  ylab("Cumulative confirmed cases") 

#Visualize 
plt_top_countries
