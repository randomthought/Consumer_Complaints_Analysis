require(ggplot2)
require(dplyr)
require(scales)
require(googleVis)
require(RCurl)
require(zipcode)
require(ggmap)
data(zipcode)

tempData <- tbl_df(Consumer_Complaints)

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

issueFrame <- function(issue, type){
  
  tempData %>%
    select(ZIP.code, type) %>%
    na.omit() %>%
    filter(type == issue) %>%
    group_by(ZIP.code, type) %>%
    summarise(cases = n()) %>%
    summarise(cases = n())

}

# Data for products according to zip codes

# Group by zip code and get the total number of times zip code is in list



# Analyziz Identity theft
plotObervation(issueFrame("Identity theft / Fraud / Embezzlement", Issue))
  
# Visully analyze debpts not owed
plotObervation(issueFrame("Cont'd attempts collect debt not owed", Issue))

# Visual Analyze fruad/scams
plotObervation(issueFrame("Fraud or scam", "Issue"))




