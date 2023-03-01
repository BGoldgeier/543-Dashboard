library(rio)
library(lubridate)
library(tidyverse)
library(leaflet)
library(sf)




ggplot(burke_table)+
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

summer<-burke_table%>%filter(month %in% c('May', 'Jun', 'Jul', 'Aug'))
summer_total<-sum(summer$percent)*100
max_label<-max(burke_table$percent)*100