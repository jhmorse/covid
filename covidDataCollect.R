library(tidyverse)
library(lubridate)

# Source data collected by Johns Hopkins University.
# Stored on GitHub at this location:
# https://github.com/CSSEGISandData/COVID-19


# NOTES in regards to this file
# Goals of this data set are to leverage a saved CSV file.
# Prior loads of data are stored in covidus.csv.
# This iteration will load the file, then generate
# additional data from the online source files.
# The new data is appended to the older data set.

readData <- function(sFile) {
  # create a safe version of the read_csv function to handle
  # errors if the CSV file does not exist or can not be read
  safe_read <- possibly(read_csv, otherwise=NULL)
  df <- safe_read(sFile)
}

getNewData <- function(startDate, endDate, locPrefix, locSuffix, sCountry='US') {
  # Create a safe_mutate function
  safe_mutate <- safely(mutate)
  
  #################################################################
  # Loop through the source files collecting the data of interest
  
  # Loop through the dates
  indx <- as.integer(endDate - startDate)
  for (i in 0:indx) {
    # Format the next date for the file name
    sdate <- format(startDate + i, format="%m-%d-%Y")
    
    # Source the next file
    loc <- paste(locPrefix, sdate, locSuffix, sep = "")
    covid <- suppressWarnings(read_csv(loc))
    
    #Rename and reorder columns
    check <- safe_mutate(covid, Province_State = `Province/State`)
    if (!is.null(check$result))
      covid <- check$result
    check <- safe_mutate(covid, Country_Region = `Country/Region`)
    if (!is.null(check$result))
      covid <- check$result
    check <- safe_mutate(covid, Last_Update = `Last Update`)
    if (!is.null(check$result))
      covid <- check$result
    # Convert Last_Update to date
    check <- safe_mutate(covid, Last_Update = as.Date(as.character(Last_Update), tryFormats = c("%m/%d/%y", "%Y-%m-%d")))
    if (!is.null(check$result))
      covid <- check$result
    # Check for incorrect date assignment in record
    # based on known data issues
    if (sdate == "03-13-2020") {
      covid$Last_Update <- as.Date(sdate, format = "%m-%d-%Y")
    }
    
    check <- safe_mutate(covid, Latitude = `Lat`)
    if (!is.null(check$result))
      covid <- check$result
    if (is.null(covid$Latitude))
      covid <- covid %>% mutate(Latitude = NA)
    
    check <- safe_mutate(covid, Longitude = `Long_`)
    if (!is.null(check$result))
      covid <- check$result
    if (is.null(covid$Longitude))
      covid <- covid %>% mutate(Longitude = NA)
    
    ###################################
    # For more recent files, get Admin2
    # We will use this to replace Province if not NA
    check <- safe_mutate(covid, Admin = Admin2)
    if (!is.null(check$result))
      covid <- check$result
    if(is.null(covid$Admin))
      covid <- covid %>% mutate(Admin = NA)
    
    covid <- covid %>% 
      select(Admin, Province_State, Country_Region, Last_Update, Latitude, Longitude, 
             Confirmed, Deaths, Recovered)
    
    # filter to just US data
    covid <- covid %>%
      filter(Country_Region == sCountry)
    
    # if this is NOT the first loop, bind the new data set to the existing
    if (i > 0)
      covidus <- rbind(covidus, covid)
    else
      covidus <- covid
  }
  
  covidus <- separate(covidus, Province_State, into = c("Province", "St"), sep = ", ")
  
  # All NEW State data is now in a data frame called covidus
}


