require(ggplot2)
require(dplyr)
require(scales)
require(googleVis)
require(RCurl)
require(zipcode)
require(ggmap)
require(lubridate)
require(ggvis)
data(zipcode)

Consumer_Complaints <- tbl_dt(Consumer_Complaints)
Consumer_Complaints$Date.received <- as.Date(Consumer_Complaints$Date.received, "%m/%d/%Y")
Consumer_Complaints$Date.sent.to.company <- as.Date(Consumer_Complaints$Date.received, "%m/%d/%Y")
tempData <- tbl_df(Consumer_Complaints)

tempData$Year.received <- year(tempData$Date.received)
tempData$Month.received <- months(tempData$Date.received)
tempData$Month.received.num <- month(tempData$Date.received)

# Pre adding dates to data 


# Fields
map <- get_map(location='united states', zoom = 4, maptype = "terrain",
               source='google',color='color')

# Functions
plotObervation <- function(dataframe){
  
  colnames(dataframe)[1] <- 'zip'
  
  dataframe <- merge(dataframe, zipcode, by.x = 'zip', by.y = 'zip')
  
  ggmap(map) + geom_point(
    aes(x = longitude, y = latitude, show_guide = TRUE, colour = cases), 
    data = dataframe, alpha = .5, na.rm = T)  + 
    scale_color_gradient(low = "beige", high = "blue")
}

issueFrame <- function(issue){
  
  tempData %>%
    select(ZIP.code, Issue) %>%
    na.omit() %>%
    filter(Issue == issue) %>%
    group_by(ZIP.code, Issue) %>%
    summarise(cases = n()) %>%
    summarise(cases = n())

}

# Data for products according to zip codes

# Group by zip code and get the total number of times zip code is in list



# Analyziz Identity theft
plotObervation(issueFrame("Identity theft / Fraud / Embezzlement"))
  
# Visully analyze debpts not owed
plotObervation(issueFrame("Cont'd attempts collect debt not owed"))

# Visual Analyze fruad/scams
plotObervation(issueFrame("Fraud or scam"))

#

monthData <- tempData %>%
                arrange(Month.received.num) %>%
                select(Month.received) %>%
                group_by(month = Month.received) %>%
                summarise(cases = n()) %>%
                ggvis(x = ~month, y = ~cases) %>% layer_bars()
            
monthData <- tempData %>%
                arrange(Month.received.num) %>%
                select(Month.received.num) %>%
                group_by(month = Month.received.num) %>%
                summarise(cases = n()) %>%
                ggvis(x = ~month, y = ~cases) %>% layer_bars()

yearData <- tempData %>%
                select(Year.received) %>%
                group_by(year = Year.received) %>%
                summarise(cases = n()) %>%
                ggvis(x = ~year, y = ~cases) %>% layer_bars()


