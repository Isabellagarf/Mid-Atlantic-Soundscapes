library(here)
library(PAMscapes)
library(dbplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(suntools)
library(sonicscrewdriver) #produces a nice looking diel plot of twilight times
library(stringr)

#### ---- loading in deployments --- #####

##first deployment
#nrs11Folder <- ('/Users/isabella.garfield/Documents/GitHub/Mid-Atl-Soundscapes/data/DBs/202306DB03')
#nrsFiles <- list.files(nrs11Folder, pattern='nc$', full.names=TRUE)
#nrsData1 <- loadMultiscapeData(nrs11Folder, timeBin = "1minute")

#second deployment
nrs11Folder2 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202404_ES01/NC')
nrsFiles2 <- list.files(nrs11Folder2, pattern='nc$', full.names=TRUE)
nrsData2 <- loadMultiscapeData(nrs11Folder2, timeBin = "1hour")

#third deployment
nrs11Folder3 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202404_ES02/NC')
nrsFiles3 <- list.files(nrs11Folder3, pattern='nc$', full.names=TRUE)
nrsData3 <- loadMultiscapeData(nrs11Folder3, timeBin = "1hour")

#fourth deployment
nrs11Folder4 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202404_ES03/NC')
nrsFiles4 <- list.files(nrs11Folder4, pattern='nc$', full.names=TRUE)
nrsData4 <- loadMultiscapeData(nrs11Folder4, timeBin = "1hour")

#fifth deploymet
nrs11Folder5 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_VA/NEFSC_VA_202405_CB01/NC')
nrsFiles5<- list.files(nrs11Folder5, pattern='nc$', full.names=TRUE)
nrsData5 <- loadMultiscapeData(nrs11Folder5, timeBin = "1hour")

#### --- joining deployments --- #####

nrsData <- nrsData2 %>%
 full_join(nrsData3) #%>%
# full_join(nrsData3) %>%
# full_join(nrsData4) %>%
# full_join(nrsData5) 


#### suntools ####
help(suntools)

location <- matrix(c(-74.82, 38.71), nrow = 1)
date_Time <- as.POSIXct("2023-09-16", tz = "UTC")

Sunrise <- sunriset(crds = location, dateTime = date_Time, direction = "sunrise", POSIXct.out = TRUE)
sunrise_dechour <- Sunrise %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))


Sunset <- sunriset(crds = location, dateTime = date_Time, direction = "sunset", POSIXct.out = TRUE)
sunset_dechour <- Sunset %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

civil_dawn <- crepuscule(crds = location, dateTime = date_Time, solarDep = 6, direction = "dawn", POSIXct.out = TRUE)
civil_dawn_dechour <- civil_dawn %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

nautical_dawn <- crepuscule(crds = location, dateTime = date_Time, solarDep = 12, direction = "dawn", POSIXct.out = TRUE)
nautical_dawn_dechour <- nautical_dawn %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

astronomical_dawn <- crepuscule(crds = location, dateTime = date_Time, solarDep = 18, direction = "dawn", POSIXct.out = TRUE)
astronomical_dawn_dechour <- astronomical_dawn %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))


civil_dusk <- crepuscule(crds = location, dateTime = date_Time, solarDep = 6, direction = "dusk", POSIXct.out = TRUE)
civil_dusk_dechour <- civil_dusk %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

nautical_dusk <- crepuscule(crds = location, dateTime = date_Time, solarDep = 12, direction = "dusk", POSIXct.out = TRUE)
nautical_dusk_dechour <- nautical_dusk %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))

astronomical_dusk <- crepuscule(crds = location, dateTime = date_Time, solarDep = 18, direction = "dusk", POSIXct.out = TRUE)
astronomical_dusk_dechour <- astronomical_dusk %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(decimal_hour = hour + (min/60) + (sec/3600))



#### --- Plotting Diel Trends --- ####


#### extract octave levels ####

nrsData.oct <- createOctaveLevel(nrsData, type='ol') 

# restructuring data to extract specific band during a specific month, ordered by hour of the day to analyze diel patterns #

nrsData.oct.mhd <- nrsData.oct %>%
  mutate(hour = substr(format(UTC,"%Y:%m:%d %H:%M:%S"), 12, 13)) %>% #lubridate doesn't recognize hour 00, so this is extracting the characters themselves
  mutate(hour = as.numeric(hour)) %>%
  filter(month(UTC) == 9) %>% #adjust month
  mutate(day = day(UTC)) %>%
  select(UTC, OL_500, day, hour) #adjust freq extracted

# calculating median, .1 and .9 quantile values for each hour #

nrsData.medoct <- nrsData.oct.mhd %>%
  group_by(hour) %>%
  summarise(
    avg_ol = median(OL_500), #change freq if desired
    q1_ol = quantile(OL_500, 0.1), #0.1 quantile SPL
    q9_ol = quantile(OL_500, 0.9)  #0.9 quantile SPL
  ) %>%
  arrange(hour)

# basic plot with individual lines for every day of the month #

plot400 <- ggplot(nrsData, aes(hour, HMD_400, group = day)) +
  geom_line(aes(color=day,  alpha = 0.3)) +
  theme_minimal()

plot400

# summary plot with median and quantile values #

plot2000.med <- ggplot(nrsData.medoct, aes(x = hour, y = avg_ol)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 22, by = 2)) +
  geom_ribbon(aes(x = hour, ymin = q1_ol, ymax = q9_ol), alpha = 0.2, fill = "gray") +
  geom_vline(xintercept = civil_dusk_dechour$decimal_hour, color = "gray40", linetype = "dashed") +
  geom_vline(xintercept = nautical_dusk_dechour$decimal_hour, color = "gray20", linetype = "dashed") +
  geom_vline(xintercept = astronomical_dusk_dechour$decimal_hour, color = "gray1", linetype = "dashed") +
  geom_vline(xintercept = civil_dawn_dechour$decimal_hour, color = "gray40", linetype = "dashed") +
  geom_vline(xintercept = nautical_dawn_dechour$decimal_hour, color = "gray20", linetype = "dashed") +
  geom_vline(xintercept = astronomical_dawn_dechour$decimal_hour, color = "gray1", linetype = "dashed") +
  geom_vline(xintercept = sunrise_dechour$decimal_hour, color = "gray60", linetype = "solid", width =  2) +
  geom_vline(xintercept = sunset_dechour$decimal_hour, color = "gray60", linetype = "solid", width = 2) +
  #scale_color_manual(name = "Key", values = c(Sunrise = "goldenrod3", Sunset = "darkmagenta")) +
  #theme_minimal() +
  labs(title="OL 2000: Diel fluctuations during September 2023 and 2024 at DB01", x="Hour", y = "dB")

plot2000.med

# save figure #

ggsave(here("figs/OL_plots/DB01", "line500ol.sepDB01.png"), plot500.med, width= 7, height= 4, units= "in")


# --- sunset colors? #
plot500.med <- ggplot(nrsData.medoct, aes(x = hour, y = avg_ol)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 22, by = 2)) +
  geom_ribbon(aes(x = hour, ymin = q1_ol, ymax = q9_ol), alpha = 0.2, fill = "gray") +
  geom_vline(xintercept = civil_dusk_dechour$decimal_hour, color = "#F76218", linetype = "solid",size =  1, alpha = .5) +
  geom_vline(xintercept = nautical_dusk_dechour$decimal_hour, color = "#B10065", linetype = "solid",size =  .7, alpha = .5) +
  geom_vline(xintercept = astronomical_dusk_dechour$decimal_hour, color = "#740580", linetype = "solid",size =  .4, alpha = .5) +
  geom_vline(xintercept = civil_dawn_dechour$decimal_hour, color = "#F76218", linetype = "solid",size =  1, alpha = .5) +
  geom_vline(xintercept = nautical_dawn_dechour$decimal_hour, color = "#B10065", linetype = "solid",size =  .7, alpha = .5) +
  geom_vline(xintercept = astronomical_dawn_dechour$decimal_hour, color = "#740580", linetype = "solid",size =  .4, alpha = .5) +
  geom_vline(xintercept = sunrise_dechour$decimal_hour, color = "#F2C447", linetype = "solid", size =  2, alpha = .5) +
  geom_vline(xintercept = sunset_dechour$decimal_hour, color = "#F2C447", linetype = "solid", size = 2, alpha = .5) +
  theme_bw() +
  labs(title="OL 500: Diel fluctuations during September 2023 and 2024 at DB01", x="Hour (UTC)", y = "dB")

plot500.med

