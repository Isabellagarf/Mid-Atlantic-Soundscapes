library(here)
library(PAMscapes)
library(dbplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

##first deployment
#nrs11Folder <- ('/Users/isabella.garfield/Documents/GitHub/Mid-Atl-Soundscapes/data/DBs/202306DB03')
#nrsFiles <- list.files(nrs11Folder, pattern='nc$', full.names=TRUE)
#nrsData1 <- loadMultiscapeData(nrs11Folder, timeBin = "1minute")

#second deployment
nrs11Folder2 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_DE/NEFSC_DE_202306_DB01/NC')
nrsFiles2 <- list.files(nrs11Folder2, pattern='nc$', full.names=TRUE)
nrsData2 <- loadMultiscapeData(nrs11Folder2, timeBin = "1hour")

#third deployment
nrs11Folder3 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_DE/NEFSC_DE_202301_DB03/NC')
nrsFiles3 <- list.files(nrs11Folder3, pattern='nc$', full.names=TRUE)
nrsData3 <- loadMultiscapeData(nrs11Folder3, timeBin = "1hour")

#fourth deployment
nrs11Folder4 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_DE/NEFSC_DE_202306_DB03/NC')
nrsFiles4 <- list.files(nrs11Folder4, pattern='nc$', full.names=TRUE)
nrsData4 <- loadMultiscapeData(nrs11Folder4, timeBin = "1hour")

#fifth deploymet
nrs11Folder5 <- ('//nefscdata/PassiveAcoustics/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/Raw/NEFSC_DE/NEFSC_DE_202306_DB02/NC')
nrsFiles5<- list.files(nrs11Folder5, pattern='nc$', full.names=TRUE)
nrsData5 <- loadMultiscapeData(nrs11Folder5, timeBin = "1hour")

nrsData <- nrsData2 %>%
  mutate(UTC = ymd_hms(UTC)) %>%
  filter(month(UTC) == 7) %>% #adjust month
  mutate(hour = hour(UTC)) %>%
  mutate(day = day(UTC)) %>%
  select(UTC, HMD_400, day, hour) #adjust freq extracted

nrsData.median <- nrsData %>%
  group_by(hour) %>%
  summarise(
    avg_hmd = median(HMD_400), #change freq if desired
    q1_hmd = quantile(HMD_400, 0.1), #0.1 quantile SPL
    q9_hmd = quantile(HMD_400, 0.9)  #0.9 quantile SPL
  ) %>%
  arrange(hour)

#nrsData <- nrsData2 %>%
  # full_join(nrsData2) %>%
  # full_join(nrsData3) %>%
  # full_join(nrsData4) %>%
  # full_join(nrsData5) 
  
# --- experimental plotting ---- #

plot400 <- ggplot(nrsData, aes(hour, HMD_400, group = day)) +
  geom_line(aes(color=day,  alpha = 0.3)) +
  theme_minimal()


plot400.med <- ggplot(nrsData.median, aes(x = hour, y = avg_hmd)) +
  geom_line(color = "darkslategray", size = 1) +
  geom_point(color = "darkslategray") +
  geom_ribbon(aes(x = hour, ymin = q1_hmd, ymax = q9_hmd), alpha = 0.2, fill = "skyblue", color = "darkslategray") +
  theme_minimal() +
  labs(title="HMD freq 400: Diel fluctuations during July at DB01", x="Hour", y = "dB")
  

plot400.med




plot400 <- ggplot(nrsData, aes(hour, HMD_400, group = day)) +
  geom_line(aes(color=day, alpha = 0.5)) +
  theme_minimal()


plot1669

plot400

ggsave(here("figs/HMD_lines/DB01", "line400.julyDB01med.png"), plot400.med, width= 7, height= 4, units= "in")

ggsave(here("figs/HMD_lines/DB01", "line400.july.DB01.png"), plot400, width= 14, height= 8, units= "in")


  
  
  # --- PAMscapes Plot exploration --- #
  
  runSoundscapeExplorer(nrsData)

HMD400 <- plotTimeseries(nrsData, column="HMD_400", q=0, style="line", by="platform")
fall.plat <- plotPSD(nrsData, style="quantile", by="platform", q=0)
HMD400
fall.plat

HMD1669 <- plotTimeseries(nrsData, column="HMD_1669", q=0, style="line", by="platform")
HMD1669



ggsave(here("figs/PAMscapes/mixed.plat", "202306.400timeseries.DBs.png"), HMD400, width= 14, height= 8, units= "in")

ggsave(here("figs/PAMscapes/mixed.plat", "202306.PSD.DBs.png"), fall.plat, width= 14, height= 8, units= "in")