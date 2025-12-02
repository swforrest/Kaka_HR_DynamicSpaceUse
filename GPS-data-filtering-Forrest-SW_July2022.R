#### GPS data filtering using Shimada et al (2012) method
#### Scott Forrest
#### 25th July 2022

library(tidyverse)
library(SDLfilter)

data(turtle)

# setwd("~/Desktop/OneDrive - University of Otago/MSc - Scott Forrest/DATA/GPS Data/Data - FINAL/CSV Files")
setwd("~/Library/CloudStorage/OneDrive-QueenslandUniversityofTechnology/MSc - Scott Forrest/DATA/GPS Data/Data - FINAL/CSV Files")

# GPS data files which already have NAs removed
# this import could easily be made into a function

T45505 <- read.csv("T45505_2021_02_11_no_na.csv")
T45506 <- read.csv("T45506_2021_01_21_no_na.csv")
T45507 <- read.csv("T45507_2021_03_01_no_na.csv")
T45508 <- read.csv("T45508_2021_03_02_no_na.csv")
T45509 <- read.csv("T45509_2021_01_05_no_na.csv")
T45510 <- read.csv("T45510_2021_01_05_no_na.csv")
T45511 <- read.csv("T45511_2021_05_12_no_na.csv")
T45512 <- read.csv("T45512_2021_02_03_no_na.csv")
T45513 <- read.csv("T45513_2021_02_03_no_na.csv")
T45514 <- read.csv("T45514_2021_02_11_no_na.csv")

select <- dplyr::select # setting functions from specific packages
rename <- dplyr::rename

filter_function <- function(df, ID) {

  # you will need to change the names to match your column names - you need at least DateTime, Latitude, Longitude, Satellites
  # but you might want to keep other columns if they have covariates (temperature, activity, etc...)
  
  # select columns
  DF <- df %>% dplyr::select(DateTimeGMT, 
                             Latitude, 
                             Longitude, 
                             `Altitude.m.`, 
                             HDOP, 
                             Temperature.C., 
                             Satellites)  
  
  # change the id variable to a id name that is inputted into the function as a variable
  DF$id <- ID
  
  # rename columns to match SDLfilter requirements
  # this could include an NA remove step if necessary
  DF <- DF %>% dplyr::rename(DateTime = DateTimeGMT, 
                             lat = Latitude, 
                             lon = Longitude, 
                             alt = `Altitude.m.`, 
                             temp = Temperature.C., 
                             qi = Satellites) %>% 
    relocate(id) # move id to first column
  
  # ensure DateTime is a POSIXct format and set the timezone (this won't change the time zone so make sure it matches what it's already in)
  DF$DateTime <- as.POSIXct(DF$DateTime, "GMT")
  
  # calculate Vmax for 6 or more satellites at 99th percentile
  DF_vmax <- vmax(DF, qi = 6, prob = 0.99) # qi is the number of satellites
  
  # print the Vmax value to the console
  print(DF_vmax)
  
  # run the speed filter 
  # method 1 removes a location if the speed from a previous AND to a subsequent location exceeds vmax (will keep more points)
  # method 1 removes a location if the speed from a previous OR to a subsequent location exceeds vmax (will remove more points)
  
  filtered_speed_6 <- ddfilter_speed(DF, vmax = DF_vmax, method = 1) # speed method only

}


T05 <- filter_function(T45505, "45505") # call function
write.csv(T05, "T45505_dd_speed_6.csv", row.names = T) # write as csv to use in other analyses

T06 <- filter_function(T45506, "45506")
write.csv(T06, "T45506_dd_speed_6.csv", row.names = T)

T07 <- filter_function(T45507, "45507")
write.csv(T07, "T45507_dd_speed_6.csv", row.names = T)

T08 <- filter_function(T45508, "45508")
write.csv(T08, "T45508_dd_speed_6.csv", row.names = T)

T09 <- filter_function(T45509, "45509")
write.csv(T09, "T45509_dd_speed_6.csv", row.names = T)

T10 <- filter_function(T45510, "45510")
write.csv(T10, "T45510_dd_speed_6.csv", row.names = T)

T11 <- filter_function(T45511, "45511")
write.csv(T11, "T45511_dd_speed_6.csv", row.names = T)

T12 <- filter_function(T45512, "45512")
write.csv(T12, "T45512_dd_speed_6.csv", row.names = T)

T13 <- filter_function(T45513, "45513")
write.csv(T13, "T45513_dd_speed_6.csv", row.names = T)

T14 <- filter_function(T45514, "45514")
write.csv(T14, "T45514_dd_speed_6.csv", row.names = T)



### ADDITIONAL STUFF TO CALCULATE VMAX FOR DIFFERENT SATELLITE THRESHOLDS ###
# If you want to check how the vmax changes with different satellite thresholds (i.e. 3 or more satellites, 4 or more, etc)
# It produces a plot below which can be helpful to visualise how vmax changes with satellite numbers

### Clean the data ###
# firstly clean the data (basically ensuring the columns are correctly labelled - this could include an NA remove step), which is the same as the function above just without running the vmax function. This could easily precede the vamx function, and then have separate vmax and iterative vmax functions

data_clean_function <- function(df, ID) { 
  
  DF <- df %>% dplyr::select(DateTimeGMT, 
                             Latitude, 
                             Longitude, 
                             `Altitude.m.`, 
                             HDOP, 
                             Temperature.C., 
                             Satellites) 
  
  DF$id <- ID
  
  DF <- DF %>% dplyr::rename(DateTime = DateTimeGMT, 
                             lat = Latitude, 
                             lon = Longitude, 
                             alt = `Altitude.m.`, 
                             temp = Temperature.C., 
                             qi = Satellites) %>% 
    relocate(id) %>% # move id to first column
    mutate(DateTime = as.POSIXct(DF$DateTime, "GMT"))
  
}


T05_clean <- data_clean_function(T45505, "45505")
T06_clean <- data_clean_function(T45506, "45506")
T07_clean <- data_clean_function(T45507, "45507")
T08_clean <- data_clean_function(T45508, "45508")
T09_clean <- data_clean_function(T45509, "45509")
T10_clean <- data_clean_function(T45510, "45510")
T11_clean <- data_clean_function(T45511, "45511")
T12_clean <- data_clean_function(T45512, "45512")
T13_clean <- data_clean_function(T45513, "45513")
T14_clean <- data_clean_function(T45514, "45514")


#### to iterate over the number of satellites (i.e Vmax for 3 or more satellites, 4 or more, etc) ####

##### Creating a function to iterate over the number of satellites and calculate vmax #####

vmax3to9 <- function(DF) {
  
  output.vector <- vector("double", 6) # creates vector to store results (6 values for 3 to 8 satellites)

  for (i in c(3:8)) { # iterates through 3:8 satellites (or more), stores as indices 1:6 in vector
    
    output.vector[[i-2]] <- vmax(DF, qi = i, prob = 0.99)
    
    } 
    
  output.vector
    
  }


T05vmax <- vmax3to9(T05_clean) # using cleaned data frames
T06vmax <- vmax3to9(T06_clean)
T07vmax <- vmax3to9(T07_clean)
T08vmax <- vmax3to9(T08_clean)
T09vmax <- vmax3to9(T09_clean)
T10vmax <- vmax3to9(T10_clean)
T11vmax <- vmax3to9(T11_clean)
T12vmax <- vmax3to9(T12_clean)
T13vmax <- vmax3to9(T13_clean)
T14vmax <- vmax3to9(T14_clean)

sats <- c(3:8)
vmax.df <- as.data.frame(cbind(sats, T05vmax, T06vmax, T07vmax, T08vmax, T09vmax, T10vmax, T11vmax, T12vmax, T13vmax, T14vmax))


# Need to reshape data frame to long format so it can be plotted etc

vmax.long <- vmax.df %>% reshape(varying = c("T05vmax", "T06vmax", "T07vmax", "T08vmax", "T09vmax", "T10vmax", "T11vmax", "T12vmax", "T13vmax", "T14vmax"), 
                                 v.names = "v.max", 
                                 timevar = "tag", 
                                 times = c("T05", "T06", "T07", "T08", "T09", "T10", "T11", "T12", "T13", "T14"), 
                                 direction = "long") %>% 
  
  mutate(id = NULL) # column not required

# varying is the actual row names
# v.names is the name given to the values
# timevar is the name given to the variable describing the different times or metrics
# times is the names of the variables within the columns


##### Plotting #####

# you could add covariates such as age or sex if that was interesting

vmax.long$tagf <- as.factor(vmax.long$tag)

vmax.long %>% ggplot(aes(sats, v.max, colour = tag)) +
  geom_line(size = 0.5) +
  labs(x = "Satellites", y = "Maximum Velocity (km/hour)") +
  scale_color_viridis_d("Tag ID") + # for colour
  # scale_colour_grey("Tag ID") + # for greyscale
  theme_classic() # +
  # theme(legend.position = "none") # to remove legend

ggsave(paste0("vmax_", Sys.Date(), ".png"), width=180, height=120, units="mm", dpi = 300)
# ggsave("vmax_greys_29thJune21.png", width=180, height=120, units="mm", dpi = 300)
 
