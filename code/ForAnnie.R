
# read in data
CB01 <- read.csv(here("data/OL/CB01_OL.csv"))

# Review format
CB01$UTC[1] # no hour included at 00:00:00 when data is read in which means this has to be manually added
class(CB01$UTC[1]) # character
hour(CB01$UTC[1]) # hour is recognized by hour function
ymd_hms(CB01$UTC[1]) # but not by this function
as.POSIXct(CB01$UTC[1], format = "%Y-%m-%d %H:%M:%S") # or this function

# option what I do is separate the date and hour into different columns 
# test extracting date and hour from UTC column
str_split(CB01$UTC[2]," ", simplify = T)[,1] # date
str_split(CB01$UTC[2]," ", simplify = T)[,2] # hour

# Separate date and hour
CB01 <- CB01 %>%
  mutate(UTC_date = str_split(UTC," ", simplify = T)[,1]) %>%
  mutate(UTC_hour =  str_split(UTC," ", simplify = T)[,2]) %>%
  mutate(UTC_fixed = NA) # add empty column to fill in

# review empty rows (should be midnight / dates without a HH:MM:SS in UTC column)
CB01 %>% filter(UTC_hour == "")
CB01 %>% filter(UTC_hour == "") %>% nrow # 843 rows having missing hour

# Find empty rows in new columns and fill in 
for (c in 1:nrow(CB01)) { # for each row
  if (CB01$UTC_hour[c] == "") { # if the hour column was left empty
    CB01$UTC_hour[c] <- "00:00:00" # manually add midnight
  }
}

# Check again - should be now rows left empty now
CB01 %>% filter(UTC_hour == "")

# Merge date and hour into new column
CB01$UTC_fixed <- paste(CB01$UTC_date, CB01$UTC_hour, sep = " ")

# convert that column to po
CB01$UTC_fixed[1] 
class(CB01$UTC_fixed[1]) # character

# one way to convert to datetime 
ymd_hms(CB01$UTC_fixed[1]) # hour doesn't appear in console
hour(ymd_hms(CB01$UTC_fixed[1])) # but hour is recognized/ found in the string

# another way to convert to datetime
as.POSIXct(CB01$UTC_fixed[1], format = "%Y-%m-%d %H:%M:%S") # hour doesn't appear in console
hour(as.POSIXct(CB01$UTC_fixed[1], format = "%Y-%m-%d %H:%M:%S")) # hour is recognized/ found in the string

# convert whole column
CB01$UTC_fixed <- as.POSIXct(CB01$UTC_fixed, format = "%Y-%m-%d %H:%M:%S")
# new format
class(CB01$UTC_fixed) #"POSIXct" "POSIXt" 

# review your dataframe to make sure it looks good

# test using newly defined column format
plot(CB01$UTC_fixed, CB01$Latitude) # no problem parsing timestamps

plot(ymd_hms(CB01$UTC_fixed), CB01$Latitude) # failed to parse 843 timestamps

plot(as.POSIXct(CB01$UTC_fixed,format = "%Y-%m-%d %H:%M:%S"), CB01$Latitude) # no problem parsing timestamps

runSoundscapeExplorer(CB01)
ggplot(CB01 [CB01$UTC_hour == "22:00:00",], aes(x= UTC_fixed, y= OL_500))+
  geom_line()+
  scale_x_datetime(date_breaks = "3 months", date_labels= "%b %Y", minor_breaks = NULL)+
  theme_bw()

