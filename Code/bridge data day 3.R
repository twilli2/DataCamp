library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
Hawthorne<- read_excel("~/Downloads/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
                       sheet="Hawthorne",skip = 1)
Steel<- read_excel("~/Downloads/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
                       sheet="Steel",
                       col_types = c("date", "numeric", "numeric","numeric",
                                     "numeric"), skip = 1)
Tilikum<- read_excel("~/Downloads/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
                   sheet="Tilikum",
                   col_types = c("date", "numeric", "numeric",
                                 "numeric"), skip = 1)
Hawthorne$name<-("Hawthorne")
Steel$name<-("Steel")
Tilikum$name<-("Tilikum")
names(Hawthorne)<-names(Tilikum) #force names of one sheet to the other
names(Steel)<-c("date","lower","westbound","eastbound","total","name")
bikecounts<-bind_rows(Hawthorne,Tilikum,Steel %>% select(-lower)) #put all the sheets into one

bikecounts %>%
  group_by(name)%>%
  summarize(avg_daily_counts=mean(total,Na.rm=TRUE)) #filters out by average and remove NAs

bikecounts %>%
  group_by(year)%>%
  summarize(sum_daily_counts=sum(total,Na.rm=TRUE))

utils::View(bikecounts %>%
  group_by(year,month)%>%
  summarize(sum_daily_counts=sum(total,Na.rm=TRUE)))

pdx_weather <- read_csv("~/Downloads/NCDC-CDO-USC00356750.txt")

bikecounts<-left_join(bikecounts %>% mutate(date=as_date(date)), #force column into date format
                      pdx_weather, by=c("date"="DATE"))
bikecounts %>%
  group_by(year)%>%
  summarize(sum_daily_counts=sum(total,Na.rm=TRUE))

utils::View(bikecounts)

library(ggplot2)

ggplot(data=bikecounts)+
  geom_point(mapping = aes(x = TMIN, y = total, color = name), position = "jitter")
ggplot(data = bikecounts) + 
  geom_point(mapping = aes(x = Month, y = total, color = Name)) +
  geom_smooth(mapping = aes(x = Month, y = TMAX))

bikecounts$day_of_week<-weekdays(bikecounts$date)
bikecounts$year<- year(bikecounts$date)
bikecounts$month<- month(bikecounts$date,label=TRUE)
bikecountsdate<-bikecounts %>%
  group_by(year,month)%>%
  summarize(counts=mean(total,Na.rm=TRUE)) #filters out by average and remove NAs
bikecountsdate<-left_join(bikecountsdate %>% mutate(date=as_date(date)), #force column into date format
                      pdx_weather, by=c("date"="DATE"))
bikecountsdate$day_of_week<-weekdays(bikecountsdate$date)
bikecountsdate$year<- year(bikecountsdate$date)
ggplot(bikecounts %>%
  group_by(year,month)%>%
  summarize(sum_daily_counts=sum(total,Na.rm=TRUE))) + 
  geom_point(mapping = aes(x = month, y = sum_daily_counts, fill = year, color = "RED"))

as_tibble(bikecountsdate)
bikecounts %>% 
  filter(total<10)

m<-ggplot(bikecounts,aes(x=Name,y=counts,fill=field))+
  geom_boxplot(fill="blue", color="goldenrod2",size=.05,alpha = 0.7,
               outlier.color = "#1F3552", outlier.shape = 20,notch=FALSE)+
  scale_x_discrete(name="% of Fertilizer")+
  scale_y_continuous(name="LOG N2O FLUX\n(ng N cm-2 h-1)",#line break in axis title
                     breaks = seq(1.5, 3.5, .25),
                     limits=c(1.5, 3.5))+ 
  labs(caption="Boxplot of flux by % fertilizer by field")+
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(size=0.25, colour = "lightgray"),
        panel.grid.minor = element_line(size=0.1,color="lightgray"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))+
  facet_grid(.~field)
m
