
# install.packages("plyr")
# install.packages("readr")
# install.packages("data.table")
# install.packages("crosstalk")
# install.packages("plotly")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("zoo")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("hrbrthemes")
# install.packages("lubridate")
# install.packages("reshape2")
# install.packages("ggmap")
# install.packages("maps")
# install.packages("ggthemes")
# install.packages("viridis")
# install.packages("highcharter")
# install.packages("UsingR")
# install.packages("MASS")
# install.packages("cowplot")
# install.packages("spatial")

library(plyr)
library(readr)
library(MASS)
library(ggplot2)
library(tidyr)
library(data.table)
library(crosstalk)
library(plotly)
library(shiny)
library(leaflet)
library(zoo)
library(tidyverse)
library(tidyr)
library(hrbrthemes)
library(ggthemes)
library(lubridate)
library(reshape2)
library(ggmap)
library(maps)
library(viridis)
library(cluster)
library(highcharter)
library(UsingR)
library(cowplot)
library(dplyr)

# Returns string without leading white space
trim.leading <- function (x)  sub("^\\s+", "", x)

# Returns string without trailing white space
trim.trailing <- function (x) sub("\\s+$", "", x)

# Returns string without leading or trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#-------------------------------#
#          IMPORT DATA          #
#-------------------------------#
crime = "C:/Users/jarma/Desktop/JCU - Master of Data science/MA5800 - Foundations of Data science/Assignment 5 - Capstone/Import Files/Crimes/"

import_crime <- list.files(path=crime, pattern="*.csv", full.names=TRUE)
import_crime

crime_csv <- ldply(import_crime, read_csv)

postcode_lat_long <- read.csv("C:/Users/jarma/Desktop/JCU - Master of Data science/MA5800 - Foundations of Data science/Assignment 5 - Capstone/Import Files/au_postcodes.csv"
                              ,header = TRUE
                              ,sep=",")

names(postcode_lat_long)
head(postcode_lat_long)

liqour_lic <- read.csv("C:/Users/jarma/Desktop/JCU - Master of Data science/MA5800 - Foundations of Data science/Assignment 5 - Capstone/Import Files/Liquor_License.csv"
                       ,header = TRUE
                       ,sep=",")

names(liqour_lic)
head(liqour_lic)

#-------------------------------#
#          CLEAN DATA           #
#-------------------------------#

#Give headings to the column for CRIME DATA
names(crime_csv) <- c("reported_date", "incident_suburb", "incident_postcode", "offence_desc_level1"
                      ,"offence_desc_level2", "offence_desc_level3","offence_count")

#Give columns a data type - factor, numeric, date for CRIME DATA
crime_csv$reported_date       <- as.Date(crime_csv$reported_date,'%d/%m/%Y')
crime_csv$incident_suburb     <- as.character(toupper(crime_csv$incident_suburb))
crime_csv$incident_postcode   <- as.character(crime_csv$incident_postcode)
crime_csv$offence_desc_level1 <- as.character(crime_csv$offence_desc_level1)
crime_csv$offence_desc_level2 <- as.character(crime_csv$offence_desc_level2)
crime_csv$offence_desc_level3 <- as.character(crime_csv$offence_desc_level3)
crime_csv$offence_count       <- as.numeric(crime_csv$offence_count)

#Create a new variable for year and month of crime
crime_csv$year                <- year(crime_csv$reported_date)
crime_csv$month               <- month(crime_csv$reported_date, label=TRUE, abbr=FALSE)
crime_csv$weekday             <- wday(crime_csv$reported_date, label=TRUE, abbr=FALSE)
crime_csv$weekday_num         <- wday(crime_csv$reported_date)
crime_csv$month_year          <- as.yearmon(crime_csv$reported_date, "%Y %m")

#Give columns a data type - factor, numeric, date for CRIME DATA
# postcode_lat_long$postcode       <- as.character(postcode_lat_long$postcode)
# postcode_lat_long$place_name     <- as.character(toupper(postcode_lat_long$place_name))
# postcode_lat_long$state_name     <- as.character(postcode_lat_long$state_name)
# postcode_lat_long$state_code     <- as.character(postcode_lat_long$state_code)
# postcode_lat_long$latitude       <- as.numeric(postcode_lat_long$latitude)
# postcode_lat_long$longitude      <- as.numeric(postcode_lat_long$longitude)
# postcode_lat_long$accuracy       <- as.numeric(postcode_lat_long$accuracy)

liqour_lic$state_code            <- as.character(trim.trailing(toupper(liqour_lic$PN_Name)))
liqour_lic$LIC_PremisesAddress1  <- as.character(trim.trailing(toupper(liqour_lic$LIC_PremisesAddress1)))
liqour_lic$LIC_PremisesAddress2  <- as.character(trim.trailing(toupper(liqour_lic$LIC_PremisesAddress2)))
liqour_lic$LIC_PremisesTown      <- as.character(trim.trailing(toupper(liqour_lic$LIC_PremisesTown)))
liqour_lic$LIC_PremisesPostCode  <- as.character(liqour_lic$LIC_PremisesPostCode)
liqour_lic$LIC_PremisesState     <- as.character(trim.trailing(toupper(liqour_lic$LIC_PremisesState)))
liqour_lic$Liquor_Status         <- as.character(trim.trailing(toupper(liqour_lic$Liquor_Status)))
liqour_lic$Gaming_Status         <- as.character(trim.trailing(toupper(liqour_lic$Gaming_Status)))


#removing missing records and only keeping data from 01-01-2011 to 31-12-2019 for reliable number of occurances
crime_csv <- crime_csv %>%
    filter(!is.na(year)
           ,!is.na(incident_postcode)
           ,"2011-01-01" <= reported_date
           ,reported_date < "2020-01-01")

#only keeping SA
# postcode_lat_long <- postcode_lat_long %>%
#     filter(state_code == "SA")

liqour_lic$Liquor_Status_num = ifelse(liqour_lic$Liquor_Status == "L CURRENT",1,0)
liqour_lic$Gaming_Status_num = ifelse(liqour_lic$Gaming_Status == "G CURRENT",1,0)

#Only keeping current liquor lic
liqour_lic <- liqour_lic %>%
    filter( Liquor_Status == "L CURRENT"
            ,LIC_PremisesState == "SA") %>%
    group_by(LIC_PremisesTown, LIC_PremisesPostCode) %>%
    summarise(Liquor_Status_count = sum(Liquor_Status_num)
              ,Gaming_Status_Count = sum(Gaming_Status_num)) %>%
    select(LIC_PremisesTown
           ,LIC_PremisesPostCode
           ,Liquor_Status_count
           ,Gaming_Status_Count)

# joins the data

#crime_join2 <- left_join(crime_join1, liqour_lic, by=c("incident_postcode" = "LIC_PremisesPostCode","incident_suburb"="LIC_PremisesTown"))

#-------------------------------#
#         data visualisation    #
#-------------------------------#
#--------Transposing months to visualise
#crime_csv_x3 <- crime_csv_x2 %>%
#                   pivot_wider(names_from = "month", values_from = "offence_count")

#--------visualisation tiles month by year ----------#
crime_titles <- crime_csv %>%
    filter(offence_desc_level2 =="THEFT AND RELATED OFFENCES") %>%
    group_by(month,year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month,year,offence_count)

ggplot(crime_titles, aes(month,as.factor(year) , fill = offence_count)) +
    geom_tile(size = 1, color = "white") +
    scale_fill_viridis()  +
    geom_text(aes(label=offence_count), color='white') +
    ggtitle("Crime frequency by Month and Year") +
    xlab('Month') +
    ylab('Year') +
    theme_minimal()+
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

#--------bar plot 2019----------#
crime_barplot <- crime_csv %>%
    filter(year == "2019") %>%
    group_by(offence_desc_level2) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(offence_desc_level2,offence_count)

ggplot(data=crime_barplot, aes(x = reorder(offence_desc_level2, offence_count), y = offence_count)) +
    geom_bar(stat = 'identity', width = 0.5) +
    geom_text(aes(label = offence_count), stat = 'identity', data = crime_barplot, hjust = -0.1, size = 3.5) +
    coord_flip() +
    xlab('Major Crime Indicators') +
    ylab('Number of offences') +
    ggtitle('Major Crime Indicators South Australia 2019') +
    theme_minimal() +
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

#--------Time series - Total ----------#

crime_timeSeries_total <- crime_csv %>%
    group_by(month_year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month_year,offence_count)

ggplot(data=crime_timeSeries_total, aes(x=month_year, y=offence_count)) +
    geom_line( color="steelblue") +
    geom_point() +
    xlab("Time Period") +
    ylab("Offence Count") +
    ggtitle("Time series of offences over a period of 10 years") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=90, hjust=1))

crime_timeSeries_2019 <- crime_csv %>%
    filter(year == "2019") %>%
    group_by(month_year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month_year,offence_count)

ggplot(data=crime_timeSeries_2019, aes(x=month_year, y=offence_count)) +
    geom_line( color="steelblue") +
    geom_point() +
    xlab("Time Period") +
    ylab("Offence Count") +
    ggtitle("Time series of offences for 2019") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=90, hjust=1))

#--------Time series - Theft----------#

crime_timeSeries1 <- crime_csv %>%
    filter(year == "2019", offence_desc_level2 == "THEFT AND RELATED OFFENCES") %>%
    group_by(month_year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month_year,offence_count)

timeSeries1<- ggplot(data=crime_timeSeries1, aes(x=month_year, y=offence_count)) +
    geom_line( color="steelblue") +
    geom_point() +
    xlab("Time Period") +
    ylab("Offence Count") +
    ggtitle("Theft") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=90, hjust=1))

#--------Time series - Property damage ----------#

crime_timeSeries2 <- crime_csv %>%
    filter(year == "2019", offence_desc_level2 == "PROPERTY DAMAGE AND ENVIRONMENTAL") %>%
    group_by(month_year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month_year,offence_count)

timeSeries2 <- ggplot(data=crime_timeSeries2, aes(x=month_year, y=offence_count)) +
    geom_line( color="steelblue") +
    geom_point() +
    xlab("Time Period") +
    ylab("Offence Count") +
    ggtitle("Property Damage") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=90, hjust=1))

#--------Time series - Acts intended to cause injury ----------#

crime_timeSeries3 <- crime_csv %>%
    filter(year == "2019", offence_desc_level2 == "ACTS INTENDED TO CAUSE INJURY") %>%
    group_by(month_year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month_year,offence_count)

timeSeries3 <- ggplot(data=crime_timeSeries3, aes(x=month_year, y=offence_count)) +
    geom_line( color="steelblue") +
    geom_point() +
    xlab("Time Period") +
    ylab("Offence Count") +
    ggtitle("Acts intended to cause injury") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=90, hjust=1))

#--------Time series - serious criminal trespassing ----------#

crime_timeSeries4 <- crime_csv %>%
    filter(year == "2019", offence_desc_level2 == "SERIOUS CRIMINAL TRESPASS") %>%
    group_by(month_year) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(month_year,offence_count)

timeSeries4 <- ggplot(data=crime_timeSeries4, aes(x=month_year, y=offence_count)) +
    geom_line( color="steelblue") +
    geom_point() +
    xlab("Time Period") +
    ylab("Offence Count") +
    ggtitle("Serious criminal trespassing") +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=90, hjust=1))

p <- plot_grid(timeSeries1, timeSeries2, timeSeries3, timeSeries4, labels = "AUTO")
timeSeries_title <- ggdraw() + draw_label("Time series of incidents in 2019", fontface='bold')
plot_grid(timeSeries_title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

crime_top20 <- crime_csv %>%
    filter(year == "2019") %>%
    group_by(incident_suburb) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(incident_suburb,offence_count) %>%
    arrange(desc(offence_count))

crime_top20 <- data.frame(head(crime_top20, n = 20))

ggplot(data=crime_top20, aes(x = incident_suburb, y = offence_count)) +
    geom_bar(stat = 'identity', width = 0.5) +
    geom_text(aes(label = offence_count), stat = 'identity', data = crime_barplot, hjust = -0.1, size = 3.5) +
    xlab('Neighbourhoods') +
    ylab('Number of offences') +
    ggtitle('Top 20 Neighbourhoods with highest crime 2019') +
    theme_minimal() +
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

#-------------------------------#
#         Data Sub-setting      #
#-------------------------------#

#filtering to keep only 2019 and Theft
crime_csv_x1 <- crime_csv %>%
    filter(year =="2019"
           ,offence_desc_level2 == "THEFT AND RELATED OFFENCES") %>%
    group_by(incident_suburb, incident_postcode) %>%
    summarise(offence_count = sum(offence_count)) %>%
    select(incident_suburb,incident_postcode, offence_count)

crime_csv_x2 <- left_join(crime_csv_x1, liqour_lic, by = c("incident_suburb" = "LIC_PremisesTown","incident_postcode"="LIC_PremisesPostCode"))

# crime_csv_x2$offence_count_norm <- rnorm(crime_csv_x2$offence_count)
# crime_csv_x2$liquor_lic_norm <- rnorm(crime_csv_x2$Liquor_Status_count)

# crime_csv_x2 <- na.omit(crime_csv_x2)

# dist = euclidean_distance(offence_count, Liquor_Status_count)
# E_Dist <- as.matrix(dist(crime_csv_x2[,c("offence_count","Liquor_Status_count")], method = "euclidean"))

# Correlation between # liquor lic places and offences
#
# library("ggpubr")
# names(crime_csv_x2)
# ggscatter(crime_csv_x2, x = "Liquor_Status_count", y = "offence_count",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Current Liquor licensed facilities", ylab = "Number of offences")
#
# hist(crime_csv_x2$Liquor_Status_count)
# hist(crime_csv_x2$offence_count)


#-------------------------------#
# Bring in SHP file for postcode lat, long
library(raster)

shp <- shapefile("C:/Users/jarma/Desktop/JCU - Master of Data science/MA5800 - Foundations of Data science/Assignment 5 - Capstone/Import Files/SA_LOCALITY_POLYGON_SHP-GDA2020.shp")
head(shp)

detach("package:raster", unload = TRUE)
#-------------------------------#
summary(shp@data)
shp@data <- left_join(shp@data, crime_csv_x2, by = c("NAME" = "incident_suburb"))
head(shp@data, 15)

#passes the spatial data as a data.frame rather than a spatial object
shp@data$id <- rownames(shp@data)
south_aus_shp <- fortify(shp)
south_aus_shp     <- join(south_aus_shp,shp@data, by="id")
class(south_aus_shp)

crime_shp <- south_aus_shp %>%
    filter(offence_count > 100) %>%
    select(lat,long,offence_count) %>%
    group_by(lat,long) %>%
    summarise(offence_count  = sum(offence_count))

#-------------------------------#

#-------------------------------#
#         Spatial analysis      #
#-------------------------------#
# plot(shp)
# title("Map of South Australia")

# ggplot() +
#     geom_polygon(data = south_aus_shp,
#                  aes(x = long, y = lat, group = group),
#                  fill="white", colour = "black") +
#     ggtitle("Map of South Australia") +
#     xlab("Longitude") +
#     ylab("Latitude") +
#     theme_bw()+
#     theme(axis.line=element_blank(),
#           axis.text.x=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks=element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           panel.background = element_blank())

ggplot()+
    geom_polygon(data = south_aus_shp,
                 aes(x = long, y = lat, group = group),
                 fill="white", colour = "black")+
    geom_point(data = crime_shp,
               aes(x = long, y = lat, color = "red"))+
    ggtitle("Crime hotspots in South Australia") +
    xlab("") +
    ylab("") +
    theme_bw()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())

#-------------------------------#
#         Dist clusters         #
#-------------------------------#

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
    matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

library(car)
crime_csv_x2_noNulls <- filter(south_aus_shp,offence_count>100)
crime_csv_x2_noNulls$log_odds <- logit(crime_csv_x2_noNulls$offence_count)
shapiro.test(fullMerge2$DeathsPerYear)
hist(crime_csv_x2_noNulls$log_odds)


BF.dists <- as.matrix(dist(cbind(BF_malaria_data$longitude, BF_malaria_data$latitude)))
