library(rio)
library(lubridate)
library(tidyverse)
library(leaflet)
library(sf)

###########Plot 1###########
burke_table<-read_csv("https://github.com/BGoldgeier/543-Dashboard/raw/main/burke_table.csv")

summer<-burke_table%>%filter(month %in% c('May', 'Jun', 'Jul', 'Aug'))
summer_total<-sum(summer$percent)*100
max_label<-max(burke_table$percent)*100

burke_table$month<-factor(burke_table$month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun',
                                                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

bar<-ggplot(burke_table)+
  geom_bar(aes(month,percent), stat = 'identity', fill = "lightgreen")+
  labs(title = "Use of the Burke Gilman Trail Peaks in July",
       subtitle =paste0(summer_total,"% of Yearly Use Occurs May-Aug") ,
       x = NULL,
       y = NULL,
       caption = "Data collected at NE 70th St; 2014-2017 
Data from data.seattle.gov,
Assignment 1")+ 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))+
  #geom_text(aes(month,percent,label = if_else(burke_table$month == "Jul", paste0(max_label,"%"), "")),
  #  position = position_dodge(width = .9), vjust = 0)+
  theme_minimal()

saveRDS(bar, file = "BurkeBar.rds")


#######Plot 2########
burke_sum_filt<-read_csv("https://github.com/BGoldgeier/543-Dashboard/raw/main/burke_sum_filt.csv")

ped_avg<-round(mean(burke_sum_filt$ped_total),0)
bike_avg<-round(mean(burke_sum_filt$bike_total),0)

point<-ggplot(burke_sum_filt)+
  geom_point(aes(ped_total,bike_total))+
  labs(title = "The Burke Gilman Trail sees more Bikes than Pedestrians per Day",
       subtitle = "1 Dot = 1 Day", 
       caption = "Data collected at NE 70th St; 2014-2017 
Data from data.seattle.gov,
Assignmnet 2",
       x = 'Total Pedestrians',
       y = 'Total Bikes')+
  theme_minimal()+
  geom_vline(xintercept = ped_avg, linetype= "dashed", color = "red")+
  geom_hline(yintercept = bike_avg, linetype = "dashed", color = "red")+
  annotate(geom = 'label', label = paste0("Average bikes per day: ",bike_avg,"
","Average pedestrians per day: ",ped_avg), 
           x = 2500, y = 3000, size = 4, hjust = .5, color = "red")

saveRDS(point, file = "BurkePoint.rds")


#######Plot 3########

burke_pivot<-read_csv("https://github.com/BGoldgeier/543-Dashboard/raw/main/burke_pivot.csv")

burke_pivot$month<-factor(burke_pivot$month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun',
                                                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

pop_range <- range(burke_pivot$pyr_Value)
pretty(pop_range)
pop_range_breaks <- pretty(pop_range, n = 4)

pop<-ggplot(data = burke_pivot)+
  geom_point(aes(x = month, y = pyr_Value, color = Direction), shape = 5, size = 2)+
  geom_segment(aes(x = month, y = 0, 
                   yend = pyr_Value,
                   xend = month, 
                   color = Direction))+
  labs(title = "Pedestrian Use of the Trail is Less Variable Across the Year Than Bike Usage",
       x = "Month",
       y = "Average Count",
       caption = "Data collected at NE 70th St; 2014-2017 
Data from data.seattle.gov,
Assignment 3")+
  facet_grid(~mode) +
  scale_y_continuous(breaks  = pop_range_breaks,
                     labels = abs(pop_range_breaks))+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  guides(size = "none")



saveRDS(pop, file = "BurkePop.rds")

#########Map#########

