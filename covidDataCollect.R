library(tidyverse)

# https://github.com/CSSEGISandData/COVID-19


# NOTES in regards to source files
# 3/9: This file contains only 1 case actually dated 3/9.
#      all other cases are 3/8 or otherwise. Total manually calculated
#      for 3/8 matches total for 3/8 file.
#     recommend removing 3/9 row as obvious anomaly
# 3/12: 3/12 is accurate. However, 3/11 was originally incorect,
#      showing too high of a value, resulting in negative
#      difference for 3/12. See below.
# 3/11: Data file from 3/11 shows 366, 29, 1
#      I was getting 568, 37, 1.
#      The file for 3/13 had all entries dated 3/11.
#      Thus, the 3/13 file overwrote 3/11 data.
#      Forcing 3/13 file to be 3/13 in date assignment.
# 3/13: See above. All records dated 3/11. Assuming file incorrect.
#      Forcing all records in this file to be 3/13.
# 3/14: Validated results of 572, 37, 1, which is a small
#      increase of only 4 cases taht day.
# 3/18: Confirmed total of 1014, 55, 0. This is decrease of 62
#      from prior day. Confirmed 3/17 with 1076, 55, 1.
#      assuming this was a correction of results and data
#      is as provided.
# 3/21: Totals of 1793, 94, 0 in source confirmed.
# 3/22: The date for entries on this were 3/22/20 23:45.
#      for some reason, the date conversion previously used
#      turned this into 3/23, thus losing the numbers for that date
#      In the update, we see a significatn drop in numbers from 3/21.
#      NOTE: 3/22 is first file to introduce counties.
# Total for Washington calculated at 1997, 97, 0. Code had
# 1040, 75, 0. During the dedup process, there were no unique rows.
# As a result, the records were getting grouped and the max value
# used. In fact, values shown matched the value for King County only.


# Create a safe_mutate function
safe_mutate <- safely(mutate)

# Define standard variables for date range
startDate <- as.Date("2020-03-01")
endDate <- as.Date("2020-08-01")
sCountry <- 'US'

# Identify the location of the source files
#locPrefix <- "./csse_covid_19_data/csse_covid_19_daily_reports/"
locPrefix <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
locSuffix <- ".csv"

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
  #check <- safe_mutate(covid, Last_Update = as.Date(Last_Update, format = "%m/%d/%y"))
  check <- safe_mutate(covid, Last_Update = as.Date(as.character(Last_Update), tryFormats = c("%m/%d/%y", "%Y-%m-%d")))
  if (!is.null(check$result))
    covid <- check$result
  # Check for incorrect date assignment in record
  # based on know data issues
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

# All State data is now in a data frame called covidus
write_csv(covidus, path="covidus.csv", append=FALSE)
