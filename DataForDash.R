library(rio)
library(lubridate)
library(tidyverse)
library(leaflet)
library(sf)

setwd("/Users/briangoldgeier/Documents/Classes/Grad School/22-23/Winter/Visual Analytics/Viz/543-Dashboard")

########Read Burke Data#####
csv_link <- "https://github.com/BGoldgeier/543-Dashboard/raw/main/Burke.csv"


burke<-import(csv_link)


#######General Cleanup##########
burke$Date<-mdy(burke$Date)

burke <- burke %>% replace(is.na(.), 0)

#######Plot 1###########

burke_sum<-burke%>%group_by(Date)%>%
  summarise(Total = sum(`BGT North of NE 70th Total`),
            Ped_south = sum(`Ped South`),
            Ped_north = sum(`Ped North`),
            Bike_south = sum(`Bike South`),
            Bike_north = sum(`Bike North`))

burke_sum<-burke_sum%>%mutate(month = month(Date),
                              year = year(Date),
                              ped_total = Ped_south + Ped_north,
                              bike_total = Bike_south + Bike_north)

burke_sum<-burke_sum%>%filter(year %in% c(2014,2015,2016,2017))

burke_sum<-burke_sum%>%mutate(month = case_when(month == 1 ~ "Jan",
                                                month == 2 ~ "Feb",
                                                month == 3 ~ "Mar",
                                                month == 4 ~ "Apr",
                                                month == 5 ~ "May",
                                                month == 6 ~ "Jun",
                                                month == 7 ~ "Jul",
                                                month == 8 ~ "Aug",
                                                month == 9 ~ "Sep",
                                                month == 10 ~ "Oct",
                                                month == 11 ~ "Nov",
                                                month == 12 ~ "Dec"))


statVal<-summary(burke_sum$Total, digits = 3)
statVal<-statVal%>%as.vector()
upper<-((statVal[5]-statVal[2])*1.5)+statVal[5]

burke_sum_filt<-burke_sum%>%filter(Total <= upper)


burke_table<-burke_sum_filt%>%group_by(month)%>%
  summarise(total = sum(Total, na.rm = TRUE))


burke_table<-burke_table%>%mutate(percent = round(total / sum(total),2))

burke_table$month<-factor(burke_table$month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun',
                                                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

write_csv(burke_table, 'burke_table.csv')
write_csv(burke_sum_filt, 'burke_sum_filt.csv')



#######Plot 3########

burke_sumDi<-burke_sum_filt%>%group_by(month)%>%
  summarise(Bike_south = mean(Bike_south),
            Bike_north = mean(Bike_north),
            Ped_south = mean(Ped_south),
            Ped_north = mean(Ped_north))

burke_sumDi$month<-factor(burke_sumDi$month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May', 'Jun',
                                                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

burke_pivot<-burke_sumDi%>%pivot_longer(cols = c(Ped_south, Ped_north, Bike_south,Bike_north),
                                        names_to = 'type',
                                        values_to = 'Value')

burke_pivot<-burke_pivot%>%mutate(mode = if_else(grepl("Bike", type,), "Bike", "Pedestrian"))
burke_pivot<-burke_pivot%>%mutate(Direction = if_else(grepl("south", type,), "Southbound", "Northbound"))

burke_pivot<-burke_pivot%>%mutate(pyr_Value = if_else(Direction == "Southbound", -Value, Value))

pop_range <- range(burke_pivot$pyr_Value)
pretty(pop_range)
pop_range_breaks <- pretty(pop_range, n = 4)


write_csv(burke_pivot, "burke_pivot.csv")




