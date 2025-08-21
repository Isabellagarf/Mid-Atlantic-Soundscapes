# Created by Bella Garfield, 7/9/25; edited by Annie Smith 8/21/2025
library(ggplot2)
library(ggalt)
library(readxl)
library(ggalt)

devtools::install_github("hrbrmstr/ggalt")
# Read in spreadsheet with deployment dates 
Deployment_data <- read.csv("data/Deployment_metadata_mid-Atl.csv")




# set order of site names 
Deployment_data$SITE_NAME <- factor(Deployment_data$SITE_NAME, levels = c("DB01","DB02","DB03","ES01","ES02", "ES03", "CB01", "CB02","CB04", "CB03"))

# order factor levels
Status_order <- c("Processed Sucessfully","ERROR", "Omitted")

# plot - used by Annie
deployment_timelines <- ggplot() +
  geom_dumbbell(size=3, data = Deployment_data,
                aes(y = factor(SITE_NAME),
                    x = as.Date(START_USABLE_DATE_EST, format = "%m/%d/%Y"),
                    xend = as.Date(END_USABLE_DATE_EST, format = "%m/%d/%Y"), 
                    colour = factor(Status, level = Status_order)))+
  scale_color_manual(name = "", values = c("Processed Sucessfully" = "aquamarine3",
                                           "Omitted" ="slategray3",
                                           "ERROR"= "#CC0000")) +
  
  ylab("SITE NAME") + xlab("DEPLOYMENT PERIOD (Month/Year)") + 
  ggtitle("SoundTrap deployments included in Mid-Atlantic Soundscape Analysis")+
  scale_x_date (date_breaks = "5 months", date_labels = "%m/%y")+
  scale_y_discrete(limits=rev)+
  theme(axis.text = element_text(size = 12), # format axis font 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size=12), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text=element_text(size=12)) 

ggsave("figs/Deployment.timeline/plot.png", plot =deployment_timelines , width = 220, height = 120, units = "mm", dpi = 700)      

plot(deployment_timelines)
# define colours
# myColours <- c('coral1','dodgerblue2','coral3','aquamarine3','dodgerblue4','lightsalmon2')
# names(myColours) <- levels(factor(Deployment_data$DEPLOYMENT_STATUS))
# custom_colours <- scale_colour_manual(name = "Status", values = myColours)

# plot 
plot.1 <- ggplot() +
  geom_dumbbell(size= 3, data = Deployment_data, 
                aes(y = factor(SITE_NAME, level = Site_order), # site goes on the y axis
                    x = as.Date(START_USABLE_DATE_EST, format = "%m/%d/%Y"), 
                    xend = as.Date(END_USABLE_DATE_EST, format = "%m/%d/%Y"), 
                    colour = factor(Status, level = Status_order)))+ # colour of each deployment can be changed based on a "status", or comment out if unwanted
                #    size= 0.1)) +
 # custom_colours + 
  
  # define colours for each status 
  scale_color_manual(name = "", values = c("Processed Sucessfully" = "aquamarine3",
                                           "Omitted" ="slategray3",
                                           "ERROR"= "#CC0000")) +
  
  ylab("SITE NAME") + xlab("DEPLOYMENT PERIOD (Month/Year)") + 
  ggtitle("SoundTrap deployments included in Mid-Atlantic Soundscape Analysis") +
  
  scale_y_discrete(limits=rev) + # reverse y axis
  
  scale_x_date (date_breaks = "5 months", date_labels = "%m/%y") + 
  
  # option to modify legend
  #scale_colour_discrete(name = "Instrument deployed",
  #                     labels = c("Soundtrap","Soundtrap and FPOD")) +
  
  # remove background grid etc
  theme(axis.text = element_text(size = 12), # format axis font 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size=12), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text=element_text(size=12)) 
  
  # legend point size 
  #guides(colour = guide_legend(override.aes = list(size=3)),
   #      size = FALSE) # remove size legend 

dev.off()

plot.1

ggsave("figs/Deployment.timeline/plot.png", plot = plot.1, width = 220, height = 120, units = "mm", dpi = 700)


# option to set order of site names 
Site_order <- as.character(unique(Deployment_data$SITENAME))

# order factor levels
Status_order <- c("Demographics analyzed","Presence analyzed","Ready to validate","To be processed","To be retrieved")
                  
# plot 
plot <- ggplot() +
  #facet_wrap(~REGION) +
  geom_dumbbell(size= 3, data = Deployment_data, 
                aes(y = factor(SITE_NAME), # site goes on the y axis
                    x = as.Date(START_USABLE_DATE_EST, format = "%m/%d/%Y"), 
                    xend = as.Date(END_USABLE_DATE_EST, format = "%m/%d/%Y"), 
                    colour = factor(Status, level = Status_order)))+
  #    size= 0.1)) +
  # custom_colours + 
  
  # define colours for each status 
  scale_color_manual(name = "", values = c("Processed Sucessfully" = "aquamarine3",
                                           "Omitted" ="slategray3",
                                           "ERROR"= "#CC0000")) +
  
  ylab("SITE NAME") + xlab("DEPLOYMENT PERIOD (Month/Year)") + 

  scale_y_discrete(limits=rev) + # reverse y axis
  
  scale_x_date (date_breaks = "5 months", date_labels = "%m/%y") + 

  # remove background grid etc
  theme(axis.text = element_text(size = 12), # format axis font 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size=12), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text=element_text(size=12)) 
plot
