#Import packages
library(tidyverse)
library(lubridate)
library(plotly)

#Import relevant datasets
url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
global_cases <- read_csv(url_cases)

url_deaths <-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
global_deaths <- read_csv(url_deaths)

#head(global_cases)
#head(global_deaths)

#Clean data set for India Cases
keep <- c('India')

india_covid_cases <- global_cases %>% filter((`Country/Region` == keep))
india_covid_cases <-india_covid_cases %>% select(-Lat, -Long, -`Province/State`)

india_covid_cases <- india_covid_cases %>% pivot_longer(!`Country/Region`, names_to = "date", values_to = "COVID CASES")
india_covid_cases$date <-as.Date(india_covid_cases$date, "%m/%d/%y")


india_covid_cases_cl <- india_covid_cases

india_covid_cases_cl$week <- floor_date(india_covid_cases_cl$date, "week")
india_covid_cases_cl <- india_covid_cases_cl %>%
  group_by(week) %>%
  summarize(CASES = max(`COVID CASES`))

india_covid_cases_cl$Region <- "India"

#Clean data set for Global Cases Totals excluding India
global_covid_cases <- global_cases %>% filter((`Country/Region` != keep))
global_covid_cases  <-global_covid_cases  %>% select(-Lat, -Long, -`Province/State`)
global_covid_cases <- summarise_all(global_covid_cases, ~if(is.numeric(.)) sum(.) else "Total")  

global_covid_cases  <- global_covid_cases  %>% pivot_longer(!`Country/Region`, names_to = "date", values_to = "COVID CASES")
global_covid_cases $date <-as.Date(global_covid_cases $date, "%m/%d/%y")


global_covid_cases_cl <- global_covid_cases 

global_covid_cases_cl$week <- floor_date(global_covid_cases_cl$date, "week")
global_covid_cases_cl <- global_covid_cases_cl %>%
  group_by(week) %>%
  summarize(CASES = max(`COVID CASES`))

global_covid_cases_cl$Region <- "Global Excluding India"

#Clean data set for India Covid Deaths
india_covid_deaths <- global_deaths %>% filter((`Country/Region` == keep))
india_covid_deaths <- india_covid_deaths %>% select(-Lat, -Long, -`Province/State`)
india_covid_deaths <- india_covid_deaths %>% pivot_longer(!`Country/Region`, names_to = "date", values_to = "COVID DEATHS")

india_covid_deaths$date <-as.Date(india_covid_deaths$date, "%m/%d/%y")

india_covid_deaths_cl <- india_covid_deaths

india_covid_deaths_cl$week <- floor_date(india_covid_deaths_cl$date, "week")
india_covid_deaths_cl <- india_covid_deaths_cl%>%
  group_by(week) %>%
  summarize(DEATHS= max(`COVID DEATHS`))

india_covid_deaths_cl$Region <- "India"

#Clean data set for Global Covid Deaths
global_covid_deaths <- global_deaths %>% filter((`Country/Region` != keep))
global_covid_deaths <- global_covid_deaths %>% select(-Lat, -Long, -`Province/State`)
global_covid_deaths <- summarise_all(global_covid_deaths, ~if(is.numeric(.)) sum(.) else "Total")  
global_covid_deaths <- global_covid_deaths %>% pivot_longer(!`Country/Region`, names_to = "date", values_to = "COVID DEATHS")

global_covid_deaths$date <-as.Date(global_covid_deaths$date, "%m/%d/%y")

global_covid_deaths_cl <- global_covid_deaths

global_covid_deaths_cl$week <- floor_date(global_covid_deaths_cl$date, "week")
global_covid_deaths_cl <- global_covid_deaths_cl%>%
  group_by(week) %>%
  summarize(DEATHS= max(`COVID DEATHS`))

global_covid_deaths_cl$Region <- "Global Excluding India"

#India Covid Cases Plot 
options(scipen = 999)
cases_plot_india <- ggplot(data = india_covid_cases_cl, aes(week, CASES)) + 
  geom_point(size = 2, colour = "red") + geom_line(size = 1, colour = "red") + scale_x_date(breaks = scales::breaks_pretty(10), limits = as.Date(c('2020-01-01','2021-11-30'))) + 
  theme_linedraw() + theme(plot.background = element_rect(fill = 'grey')) + 
  theme(plot.title = element_text(size=15, face="bold", hjust = .5)) + 
  ggtitle('Total Number of Covid Cases - India (Jan 2020 - Nov 2021)') + 
  theme(plot.title = element_text(size=15, face="bold", hjust = .5)) +labs(x="Date", y = 'Number of Cases') +
  theme(axis.title.x = element_text(vjust=-4)) + theme(axis.title.y = element_text(vjust= 5))  

cases_plot_india_f <- cases_plot_india + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + scale_y_continuous(labels = scales::label_comma())
p1 <- ggplotly(cases_plot_india_f) 
p1

#Global Covid Cases Plot 
options(scipen = 999)
cases_plot_global <- ggplot(data = global_covid_cases_cl, aes(week, CASES)) + 
  geom_point(size = 2, colour = "red") + geom_line(size = 1, colour = "red") + scale_x_date(breaks = scales::breaks_pretty(10), limits = as.Date(c('2020-01-01','2021-11-30'))) + 
  theme_linedraw() + theme(plot.background = element_rect(fill = 'grey')) + 
  theme(plot.title = element_text(size=15, face="bold", hjust = .5)) + 
  ggtitle('Total Number of Covid Cases - Global(Excluding India) (Jan 2020 - Nov 2021)') + 
  theme(plot.title = element_text(size=15, face="bold", hjust = .5)) +labs(x="Date", y = 'Number of Cases') +
  theme(axis.title.x = element_text(vjust=-4)) + theme(axis.title.y = element_text(vjust= 5))  

cases_plot_global_f <- cases_plot_global + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) + scale_y_continuous(labels = scales::label_comma())
p2 <-ggplotly(cases_plot_global_f)
p2 
#Side by side - Note do not use. While the patterns are clearer to see the y-axis values are extremely different 
#this will cause confusion. Use a log scale to represent the difference in pattern while keeping the integrity 
#of the underlying values
#subplot(p1, p2)


#Covid Cases Comparison Plot 
options(scipen = 999)
cases_plot_comp <- ggplot() + 
  geom_point(data = global_covid_cases_cl, aes(week, CASES, color = Region), size = 2) +
  geom_point(data = india_covid_cases_cl, aes(week, CASES, color = Region), size = 2) + 
  scale_color_manual(values = c("dark blue", "dark red")) + 
  scale_x_date(breaks = scales::breaks_pretty(10), limits = as.Date(c('2020-01-01','2021-11-30'))) +
  scale_y_continuous(trans='log10', labels = scales::comma) +
  theme_linedraw() + theme(plot.background = element_rect(fill = 'grey')) + 
  ggtitle('Total Number of Covid Cases Comparison - (Jan 2020 - Nov 2021)') + 
  theme(plot.title = element_text(size=12, face="bold", hjust = .5)) +
  theme(legend.position = "bottom") +
  labs(x="Date", y = 'Total Number (Log Scale)') +
  theme(axis.text.x = element_text(vjust = 0)) +
  theme(axis.title.x = element_text(vjust=-4)) + theme(axis.title.y = element_text(vjust= 3)) 

combined_plot_cases <- cases_plot_comp + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
p3 <- ggplotly(combined_plot_cases)
p3 %>% layout(hovermode = 'x')

#Cases that were Deaths Comparison
options(scipen = 999)
death_plot_comp <- ggplot() + 
  geom_point(data = global_covid_deaths_cl, aes(week, DEATHS, color = Region), size = 2) +
  geom_point(data = india_covid_deaths_cl, aes(week, DEATHS, color = Region), size = 2) + 
  scale_color_manual(values = c("dark blue", "dark red")) + 
  scale_x_date(breaks = scales::breaks_pretty(10), limits = as.Date(c('2020-01-01','2021-11-30'))) +
  scale_y_continuous(trans='log10', labels = scales::comma) +
  theme_linedraw() + theme(plot.background = element_rect(fill = 'grey')) + 
  ggtitle('Total Number of Covid Deaths Comparison - (Jan 2020 - Nov 2021)') + 
  theme(plot.title = element_text(size=12, face="bold", hjust = .5)) +
  theme(legend.position = "bottom") +
  labs(x="Date", y = 'Total Number (Log Scale)') +
  theme(axis.text.x = element_text(vjust = 0)) +
  theme(axis.title.x = element_text(vjust=-4)) + theme(axis.title.y = element_text(vjust= 3)) 

combined_plot_death <- death_plot_comp + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
p4 <- ggplotly(combined_plot_death)
p4 %>% layout(hovermode = 'x')

#Percentage Comparison - create needed data sets
india_covid_combined <- india_covid_deaths_cl
india_covid_combined$CASES <- india_covid_cases_cl$CASES

global_covid_combined <- global_covid_deaths_cl
global_covid_combined$CASES <- global_covid_cases_cl$CASES

#Percentage Comparison plot
options(scipen = 999)
death_plot_comp <- ggplot() + 
  geom_point(data = global_covid_combined , aes(week, DEATHS/CASES, color = Region), size = 2) +
  geom_point(data = india_covid_combined, aes(week, y = DEATHS/CASES, color = Region), size = 2) + 
  scale_color_manual(values = c("dark blue", "dark red")) + 
  scale_x_date(breaks = scales::breaks_pretty(10), limits = as.Date(c('2020-01-01','2021-11-30'))) +
  theme_linedraw() + theme(plot.background = element_rect(fill = 'grey')) + 
  ggtitle('Total Number of Covid Deaths Comparison - (Jan 2020 - Nov 2021)') + 
  theme(plot.title = element_text(size=12, face="bold", hjust = .5)) +
  theme(legend.position = "bottom") +
  labs(x="Date", y = 'Ratio of Deaths/Cases') +
  theme(axis.text.x = element_text(vjust = 0)) +
  theme(axis.title.x = element_text(vjust=-4)) + theme(axis.title.y = element_text(vjust= 3)) 

combined_plot_perc <- death_plot_comp + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
p5 <- ggplotly(combined_plot_perc)
p5 %>% layout(hovermode = 'x')



