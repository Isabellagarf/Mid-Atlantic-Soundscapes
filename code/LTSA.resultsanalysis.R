library(dplyr)
library(tidyverse)
library(lubridate)


DB01.ltsa <- read.csv("Annotations/DailyAnnotations.202301DB01.csv")
DB01.ltsa2 <- read.csv("Annotations/DailyAnnotations.202306DB01.csv")

DB01 <- rbind(DB01.ltsa, DB01.ltsa2)

DB01.time <- DB01 %>%
  select(comment, start, end, freqMin, freqMax) %>% #columns I want to keep in new dataframe
  filter(comment == "Fish chorus") %>%
  mutate(start_datetime = ymd_hms(start)) %>% #changing time and date format for start time
  mutate(end_datetime = ymd_hms(end)) %>% #changing time and date format for end time
  mutate(freqavg = ((freqMin + freqMax)/2)) #Creating a new column with the frequency average values 


ggsave("Annotations/Summary_plots/DB03.png", plot = point, width= 14, height= 8, units= "in")


# -- time -- #


DB01.time2 <- DB01.time %>%
  mutate(start_time = format(as.POSIXct(DB01.time$start_datetime), format = "%H")) %>%
  mutate(end_time = format(as.POSIXct(DB01.time$end_datetime), format = "%H")) %>%
  mutate(start_date =format(as.Date(DB01.time$start_datetime, format="%d/%m/%Y"),"%m"))

          