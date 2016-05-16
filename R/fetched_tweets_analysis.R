#########################################################################################################
#   fetched_tweets_analysis.R
#   author:  Rachel Blumhagen, rachel.blumhagen@ucdenver.edu
#
#   For:  Final Project in R and Python Course - search API from Twitter using tag word ZIKA
#         
#   data created:  May 7, 2016
#########################################################################################################
options(stringsAsFactors = FALSE)

# HOME
path<-"/Users/rachelblumhagen/"
#WORK
#path<-"C:/Users/blumhagr/"

# Set working directory
setwd(paste(path,"Google Drive/SCHOOL_work/R_Python", sep=""))

###### RESOURCES
# http://www.r-bloggers.com/setting-up-the-twitter-r-package-for-text-analytics/
# https://cran.r-project.org/web/packages/twitteR/twitteR.pdf

# library loads needed
library(ggplot2)
library(ggmap)
library(sp)
library(rworldmap)

# Read in Python pandas dataframe via csv
load(file = "Final_Project/data/fetched_tweets_dataframe.RData")

# Source functions for summarizing counts 
source(file = "Final_Project/R/functions.R")

#####################################################################################################
# General summary statistics
# Top 5 languages
table(tweets$lang)[order(table(tweets$lang), decreasing = TRUE)][1:5]
prop.table(table(tweets$lang))[order(table(tweets$lang), decreasing = TRUE)][1:5]

# Proportion with geo_enabled data
# Format geo_enabled variable
tweets$geo_enabled <- ifelse(tweets$geo_enabled %in% c("No geo data", "False"), FALSE, TRUE)
mywidetable(tweets$geo_enabled, "Geo Enabled")

# Proportion with country data
tweets$country_provided <- ifelse(tweets$country == "No country information", FALSE, TRUE)
mywidetable(tweets$country_provided, "Country Information Provided")

# Proportion with user location information available
tweets$location_provided <- ifelse(tweets$location == "<NA>", FALSE, TRUE)
mywidetable(tweets$location_provided, "User Location Information Provided")

# Top 5 countries
countries_total <- tweets$country[tweets$country_provided == TRUE]
top5_total <- table(countries_total)[order(table(countries_total), decreasing = TRUE)][1:5]
mylongtable(factor(countries_total[countries_total %in% names(top5_total)]))

# Format time variable for subsetting
tweets$date_time <- substr(tweets$created_at, start = 1, stop = 19)
tweets$date <- substr(tweets$created_at, start = 1, stop = 10)

# Number of tweets missing time information
sum(date_time == "")
# 78 don't have time information
tweets$date <- factor(tweets$date, 
               levels = c("Sun Apr 24", "Mon Apr 25", "Tue Apr 26", "Wed Apr 27", "Thu Apr 28", 
                          "Fri Apr 29", "Sat Apr 30", "Sun May 01", "Mon May 02", ""), 
               labels = c("Sun Apr 24", "Mon Apr 25", "Tue Apr 26", "Wed Apr 27", "Thu Apr 28", 
                          "Fri Apr 29", "Sat Apr 30", "Sun May 01", "Mon May 02", "Unknown")) 
# Subset data to top 5 countries and 7 full days of data
full_week <- c("Mon Apr 25", "Tue Apr 26", "Wed Apr 27", "Thu Apr 28", 
               "Fri Apr 29", "Sat Apr 30", "Sun May 01")
tweets_top5 <- subset(tweets, (date %in% full_week) & (country %in% names(top5_total)))

# Plot tweets in top 5 countries of the week 
# First count number of tweets per day by country
tweets_top5$date <- factor(tweets_top5$date)
tweets_top5$country <- factor(tweets_top5$country, levels = levels(factor(tweets_top5$country))[c(1,5,2:4)])
tweets_top5_counts <- as.data.frame(table(tweets_top5$date, tweets_top5$country))
names(tweets_top5_counts) <- c("Time", "Country", "Freq")

# Figure 1 (plot frequency of tweets in top 5 countries over week with complete data)
ggplot(tweets_top5_counts, aes(x = Time, y = Freq, group = Country, color = Country)) + 
           geom_line(size = 2) + 
           theme_bw() +
           ylab("Frequency") + 
           xlab("Date") + 
           theme(axis.text = element_text(size = 14),
                 axis.title = element_text(size = 16),
                 legend.text = element_text(size = 14),
                 legend.title = element_blank(),
                 legend.position = "top",
                 legend.key = element_blank())
ggsave(file = "Final_Project/time_series_top5countries.pdf", width = 9, height = 6.21)

########## Randomly sample 5,000 tweets for geocoding
set.seed(7794416)

# 67,462 missing location information, 131,802 with location information
## Retrieve geocode information using package ggmap
missing_location <- which(is.na(tweets$location) == TRUE)
have_location <- which(is.na(tweets$location) == FALSE)

geocode_sample_idx <- sample(have_location, size = 5000, replace = FALSE)

# Have to separate into two pulls using Google API
# geocode_sample_1 <- geocode(tweets$location[geocode_sample_idx[1:2500]])
# geocode_sample_2 <- geocode(tweets$location[geocode_sample_idx[2501:5000]])

# Combine results with tweets and save file 
# geocode_sample_tweets <- cbind(tweets[geocode_sample_idx,], rbind(geocode_sample_1, geocode_sample_2))
# save(geocode_sample_tweets, file = "Final_Project/data/geocode_sample_tweets.RData")

# Once data is pulled will combine into one randomly geocoded sample
geocode_sample <- rbind(geocode_sample_1, geocode_sample_2)
# 276 did not result in geocoding information

# Number of locations resulting in geocode information
table(is.na(geocode_sample_tweets$lat) == TRUE)

# Reverse geocoding - want country names from latitude and longitude
# http://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r
# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # convert our list of points to a SpatialPoints object
    
    # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    
    #setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    # return the ADMIN names of each country
    indices$ADMIN  
    #indices$ISO3 # returns the ISO3 code 
    #indices$continent   # returns the continent (6 continent model)
    #indices$REGION   # returns the continent (7 continent model)
}

# Apply coords2country function on tweets with latitude/longitude information
# Loop through geocode sample and apply coords2country where latitude/longitude is available
country <- NULL
for(i in 1:dim(geocode_sample)[1]) {
    if ((is.na(geocode_sample[i,1]) == TRUE)){
        country[i] <- NA
    }
    if ((is.na(geocode_sample[i,1]) == FALSE)){
        country[i] <- as.character(coords2country(geocode_sample[i,]))
    }
}

# Most common countries
head(table(country)[order(table(country), decreasing = TRUE)])

# Combine geocode information with original tweet data for sample of 5000 tweets
country_sample_df <- cbind(tweets[geocode_sample_idx,], geocode_sample, Country = country)

# Check to see how well the coords2country did 
table(is.na(country_sample_df$lon), is.na(country_sample_df$Country))

# Subset to only those tweets within the full week
country_sample_df_week <- subset(country_sample_df, date %in% full_week)

############ HEATMAP OVER THE WEEK 
# Make a world map showing locations of randomly sampled tweets 
# Increasing red color denotes later days
pdf("Final_Project/world_map_geocode_sample.pdf")
par(mar=c(0,0,0,0))
newmap <- getMap(resolution = "low")
plot(newmap)
heat_colors <- rev(heat.colors(n = 7))
for (i in 1:7){
    points(country_sample_df_week$lon[country_sample_df_week$date == full_week[i]], 
           country_sample_df_week$lat[country_sample_df_week$date == full_week[i]], 
           col = heat_colors[i], 
           cex = .5, 
           pch = 19)
}
legend(x = -115, y = -100, legend = full_week, col = heat_colors, pch = 19, border = FALSE, cex = 0.4, horiz = TRUE)
dev.off()

####### Summary data for sample of geocoded tweets
# Top 5 countries
top5_sample <- table(country_sample_df$Country)[order(table(country_sample_df$Country), decreasing = TRUE)][1:5]
prop.table(table(country_sample_df$Country, useNA = "always"))[order(table(country_sample_df$Country), decreasing = TRUE)][1:5]

# Can we look at the agreement in geocoded information for where tweet was created and user location
country_corr <- subset(country_sample_df, (is.na(Country) == FALSE) & country != "No country information")
# Look at two variables for 82 with both information
country_corr[,c("country", "Country")]

# Format country variable to resemble Country
country_corr$country[country_corr$country == "United States"] <- "United States of America"
country_corr$country[country_corr$country == "Brasil"] <- "Brazil"
country_corr$country[country_corr$country == "Republika ng Pilipinas"] <- "Philippines"
country_corr$country[country_corr$country == "Italia"] <- "Italy"
country_corr$country[country_corr$country == "México"] <- "Mexico"
country_corr$country[country_corr$country == "Deutschland"] <- "Germany"

# List those with disagreement in country information
country_corr[which(country_corr$country != country_corr$Country),c("Country", "country")]
# Proportion in disagreement
length(which(country_corr$country != country_corr$Country))/length(country_corr$Country)

# ********************************* EXPLORATORY *******************************************#
############ Regular Expressions 
# Can we subset the tweets to those including words such as "virus"
virus_text <- grep("virus", tweets$text, ignore.case = TRUE)

############ Tweets over time
date_time <- substr(tweets$created_at, start = 1, stop = 19)
date <- substr(tweets$created_at, start = 1, stop = 10)

sum(date_time == "")
# 78 don't have time information

date <- factor(date, 
               levels = c("Sun Apr 24", "Mon Apr 25", "Tue Apr 26", "Wed Apr 27", "Thu Apr 28", 
                                "Fri Apr 29", "Sat Apr 30", "Sun May 01", "Mon May 02", ""), 
               labels = c("Sun Apr 24", "Mon Apr 25", "Tue Apr 26", "Wed Apr 27", "Thu Apr 28", 
                          "Fri Apr 29", "Sat Apr 30", "Sun May 01", "Mon May 02", "Unknown")) 

# Barchart of tweets by day (Monday - Sunday, full days)
ggplot(data.frame(date = date[!(date %in% c("Sun Apr 24", "Mon May 02", "Unknown"))]), aes(x = date)) + 
    geom_bar(fill = "lightblue") + 
    theme_bw() + 
    ggtitle("Volume of tweets over time")
    
########## Changes in tweets from Brazil over time
brazil_time <- date[tweets$country == "Brasil"]
ggplot(data.frame(date = brazil_time[!(brazil_time %in% c("Sun Apr 24", "Mon May 02", "Unknown"))]), aes(x = date)) + 
    geom_bar(fill = "lightblue") + 
    theme_bw() + 
    ggtitle("Volume of tweets over time in Brazil")


