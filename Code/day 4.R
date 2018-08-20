library(gapminder)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(tidyr)
?nycflights13
gapminder %>% 
  group_by(country) %>% 
  mutate(life_exp_gain = lifeExp - first(lifeExp)) %>% 
  top_n(3)

library(readr)
library(lubridate)
pdx_weather <- read_csv("~/Downloads/NCDC-CDO-USC00356750.txt")
as_tibble(bikecounts)
bikecounts<-bikecounts %>% 
  mutate(date=as_date(date)) %>% 
  left_join(pdx_weather,by=c("date"="DATE"))
?mutate
#Order total daily bike counts by bridge in descending order, 
#create a new column (variable) containing the rank, and then show the top 3 days for each bridge

bikecounts %>%
  group_by(name)%>%
  mutate(rank=rank(desc(total))) %>% 
  filter(rank<=3) %>% 
  arrange(name,rank)


bikecounts %>% 
  group_by(name) %>% 
  top_n(-3, total)
  
bikecounts %>% 
  arrange(desc(max)) %>% 
  mutate(rank = min_rank(avg_daily_counts(date)))#filters out by average and remove NAs

str(bikecounts)
library(stats)
bikecounts$day_of_week<-weekdays(bikecounts$date)
bikecounts$year<- year(bikecounts$date)
bikecounts$month<- month(bikecounts$date,label=TRUE)

bikecounts_df <- bikecounts %>%
  summarize(sum=sum(total))

ggplot(data=bikecounts,
       geom_point(mapping = aes(x = date, y = sum)+
  geom_smooth(mapping = aes(date,TMIN*100),color = "BLUE") +
  geom_smooth(mapping = aes(date,TMAX*100),color = "RED") +
  geom_smooth(mapping = aes(date,PRCP*10000))+
  scale_y_continuous(sec.axis=sec_axis(trans =(~./100), name="TEMP")))

ggplot(bikecounts %>%
  group_by(year)%>%
  summarize(sum_daily_counts=sum(total,Na.rm=TRUE))+
  geom_point(mapping = aes(x = month, y = avg_daily_counts)))

  
  geom_smooth(mapping = aes(x = year,y = TMIN), sec_axis(trans = (~.-5000), name = ("TMIN")))

                       
scale_y_continuous(sec.axis = sec_axis(~.+10),name="TMIN"))

bikecounts %>%
  # Filter countries that start with "A" or "Z"
  filter(substr(name, start = 1, stop = 1) %in% c("A", "Z")) %>%
  # Make the plot
  ggplot(aes(x = year, y = total, color = name)) +
  geom_line() +
  facet_wrap( ~ country)

ggplot(bikecounts) +
  geom_point(aes(x=date,y=total, color=name), position = "jitter")
