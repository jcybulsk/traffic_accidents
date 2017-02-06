# Read in the motor vehicle data sets, and 
# make a simple plot or two to summarize 
# the data

library(lubridate)
library(plyr)
library(readr)
library(ggplot2)
library(ggmap)

ny <- read_csv("~/Documents/fun/car_accident/NYPD_Motor_Vehicle_Collisions.csv", 
               col_types = cols(DATE = col_date(format = "%m/%d/%Y"), 
                                TIME = col_character()))

camb <- read_csv("~/Documents/fun/car_accident/Cambridge_Police_Department_Crash_Data_-_Updated.csv", 
                 col_types = cols(`Date Time` = col_character()))

denver <- read_csv("~/Documents/fun/car_accident/denver_traffic_accidents.csv", 
                   col_types = cols(FIRST_OCCURRENCE_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
View(ny)
View(camb)
View(denver)

# Read in the Cambridge_updated set as "camb"
# Read in the NYPD_Motor_Vehicle set as "ny"
camb_dt <- as.POSIXct(camb$`Date Time`, format="%m/%d/%Y %I:%M:%S %p")
ny_datetime <- paste(ny$DATE, ny$TIME)
ny_dt <- as.POSIXct(ny_datetime, format="%Y-%m-%d %H:%M")
denver_dt <- gsub(" UTC", "", as.POSIXct(denver$FIRST_OCCURRENCE_DATE, format="%Y-%m-%d %H:%M:%S"))

hrs_denver <- hour(denver_dt)
hrs_camb <- hour(camb_dt)

denver$hrs <- hour(denver_dt)
camb$hrs <- hour(camb_dt)
ny$hrs <- hour(ny_dt)

ny$Day.Of.Week <- weekdays(ny_dt)
denver$Day.Of.Week <- weekdays(denver$FIRST_OCCURRENCE_DATE)

ny_weekend <- ny[(ny$Day.Of.Week == "Saturday" | ny$Day.Of.Week == "Sunday"), ]
ny_weekday <- ny[!(ny$Day.Of.Week == "Saturday" | ny$Day.Of.Week == "Sunday"), ]
camb_weekend <- camb[(camb$`Day of Week` == "Saturday" | camb$`Day of Week` == "Sunday"), ]
camb_weekday <- camb[!(camb$`Day of Week` == "Saturday" | camb$`Day of Week` == "Sunday"), ]
denver_weekend <- denver[(denver$Day.Of.Week == "Saturday" | denver$Day.Of.Week == "Sunday"), ]
denver_weekday <- denver[!(denver$Day.Of.Week == "Saturday" | denver$Day.Of.Week == "Sunday"), ]

all_weekday_hrs <- c(ny_weekday$hrs, camb_weekday$hrs, denver_weekday$hrs)
all_weekend_hrs <- c(ny_weekend$hrs, camb_weekend$hrs, denver_weekend$hrs)

hist(all_weekday_hrs)
hist(all_weekend_hrs)

h_weekday <- hist(all_weekday_hrs,  # Save histogram as object
          breaks = 12, 
          freq = FALSE,
          col = scales::alpha("red",.5), 
          main = "Weekday (Red) and Weekend (Blue) Car Accident \nFrequencies By Hour (NYC + Denver + Cambridge)",
          xlab = "Hour", border = FALSE)

h_weekend <- hist(all_weekend_hrs,  # Save histogram as object
                  breaks = 12,  # "Suggests" 24 bins
                  freq = FALSE,
                  col = scales::alpha("steelblue3",.5), 
                  border = FALSE, 
                  add = TRUE)

h_weekend <- hist(all_weekend_hrs,  # Save histogram as object
          breaks = 12,  # "Suggests" 24 bins
          xlim = c(0,24), 
          freq = FALSE,
          col = "steelblue3", # Or use: col = colors() [626]
          main = "Combined Weekend (Sat & Sun) Accident Frequencies\nBy Hour",
          xlab = "Hour")

# Now try making a map of NYC with a heatmap overlay showing the density of accidents
ny_lat <- na.omit(ny$LATITUDE)
ny_long <- na.omit(ny$LONGITUDE)

# Try to manually set a lat/long range 
latr <- c(-74.05, -73.85)
longr <- c(40.63, 40.84)

theme_set(theme_bw(16))
#nyc <- get_map(location = "New York City", zoom = 12, color = "bw")
nyc <- get_map(location = c(lon = mean(ny$LONGITUDE, na.rm = TRUE), 
                            lat = mean(ny$LATITUDE, na.rm = TRUE)), 
               zoom = 12, color = "bw")
nyc <- ggmap(nyc)

nyc +
  stat_density2d(
    aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..),
    size = 1, bins = 4, data = ny,
    geom = "polygon", color="darkred") + 
  ggtitle("Surface Density of Total Accidents\nin New York (07/2012-01/2017)")

# Now for Denver:
denv <- get_map(location = c(lon = mean(denver$GEO_LON, na.rm = TRUE), 
                            lat = mean(denver$GEO_LAT, na.rm = TRUE)), 
               zoom = 12, color = "bw")
denvmap <- ggmap(denv)

denvmap +
  stat_density2d(
    aes(x = GEO_LON, y = GEO_LAT, fill = ..level.., alpha = ..level..),
    size = 1, bins = 4, data = denver,
    geom = "polygon", color="darkred") + 
  ggtitle("Surface Density of Total Accidents\nin Denver (01/2012-01/2017)")


