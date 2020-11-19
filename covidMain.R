library(tidyverse)
library(lubridate)
library(gridExtra)
library(scales)

source('covidDataCollect.R')
source('covidStatesData.R')
source('covidGraph.R')


##################################################################

# Define standard variables for date range
startDate <- as.Date("2020-03-01")  # default to start of data availability
endDate <- today() - 1              # End date is always day before today
#endDate <- as.Date('2020-10-12')
sCovidUSCSV <- "covidus.csv"        # default name of source file with prior data

# Identify the location of the source files
sPrefix <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
sSuffix <- ".csv"

# Define standard variables for states of interest
cStAbb <- c('WA', 'CO', 'MN', 'CA', 'MT')
cStName <- c('Washington', 'Colorado', 'Minnesota', 'California', 'Montana')


##################################################################
# Attempt to load source file with prior data

# Call the function that reads in the CSV file
covidusFile <- readData(sCovidUSCSV)

# If we could read in a CSV file, we need to update the start date.
if (!is.null(covidusFile)) {
  # Determine the most recent date stored in the CSV file
  startDate <- max(covidusFile$Last_Update)
  
  # As a point of cleanup, we will strip out all rows associated
  # with the last date, and use that as our start date
  covidusFile <- covidusFile %>%
    filter(Last_Update != startDate)
}
# If we could not read in the file, then leave the start date as is.

# Call the function from our DataCollect file
# to go get new data from the web
covidusNew <- getNewData(startDate, endDate, sPrefix, sSuffix)

# If we had a source file at the start, we append the new data
# to the end of the source file
if (!is.null(covidusFile)) {
  # Append the new data on to the data loaded from the CSV
  covidus <- rbind(covidusFile, covidusNew)
} else {
  # if we did not have a source file, just use what
  # we just collected as our total data set
  covidus <- covidusNew
}

# ALL State data is now in a data frame called covidus
# Write that as the new CSV file, overwriting the old
write_csv(covidus, path="covidus.csv", append=FALSE)

# Call the function from the covidStates file
# To generate State specific data
# Call the function, passing in the desired list of States
covidSt <- getStates(covidus, cStAbb, cStName)
#covidSt <- getStates(covidus)

charts <- buildGraphs(covidSt, length(cStName))
#charts <- buildGraphs(covidSt)

print(charts[1])
print(charts[2])
print(charts[3])

