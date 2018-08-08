install.packages("tidyverse")
library(nycflights13)
library(tidyverse)
mpg
ggplot(data=mpg)+
geom_point(mapping = aes(x = class, y = drv))
library(readxl)
Hawthorne<-Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_ <- read_excel("Data/Hawthorne Tilikum Steel daily bike counts 073118 (1).xlsx",
sheet = "Hawthorne", col_types = c("date","numeric", "numeric", "numeric"),skip = 1)
View(Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_)
Tilikum<-Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_ <- read_excel("Data/Hawthorne Tilikum Steel daily bike counts 073118 (1).xlsx",
        sheet = "Tilikum", col_types = c("date","numeric", "numeric", "numeric"), skip = 1)
View(Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_)
Steel<-Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_ <- read_excel("Data/Hawthorne Tilikum Steel daily bike counts 073118 (1).xlsx",
        sheet = "Steel", col_types = c("date", "numeric", "numeric", "numeric", "numeric"), skip = 1)
View(Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_)
bridgedata<-Hawthorne_Tilikum_Steel_daily_bike_counts_073118_1_
utils::View(Hawthorne)
utils::View(Tilikum)
utils::View(Steel)
summary(Hawthorne)
library(dplyr)
tally(Hawthorne)
Hawthorne$Name<-("Hawthorne")
Steel$Name<-("Steel")
Tilikum$Name<-("Tilikum")
names(Hawthorne)<-names(Tilikum) #force names of one sheet to the other
names(Steel)<-c("date","lower","westbound","eastbound","total","Name")
bikecounts<-bind_rows(Hawthorne,Tilikum,Steel %>% select(-lower)) #put all the sheets into one
bikecounts %>%
  group_by(Name)%>%
  summarize(avg_daily_counts=mean(total,Na.rm=TRUE)) #filters out by average and remove NAs
library(lubridate) #
bikecounts %>%
  group_by(Name,month(date))%>%
  summarize(avg_daily_counts=mean(total,Na.rm=TRUE))
bikecounts %>%
  group_by(Name,month(date))%>%
  summarize(avg_daily_counts=mean(total,Na.rm=TRUE/distinct(month)))
utils::View(bikecounts %>%
              group_by(Name,ym=floor_date(date,"month"))%>%
              summarize(total_monthly_counts=sum(total),counts=n())%>%
              group_by(Name,month(ym))%>%
              summarize(avg_montly_count=mean(total_monthly_counts)))#control shift m for pipe
              
input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118_1.xlsx"
bridge_name <- "Hawthorne"

library(readxl)
library(lubridate)
# define a funtion that load bike counts data
load_data <- function(input_file, bridge_name) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1)
  bikecounts$name <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "name")

# combine all three data frame for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

# average daily bike counts by bridge
bikecounts %>% 
  group_by(Name) %>% 
  summarize(avg_daily_counts=mean(total, na.rm=TRUE))
utils::View(bikecounts)
