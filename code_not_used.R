

safe_mutate <- safely(mutate)

startDate <- as.Date("2020-03-09")
endDate <- as.Date("2020-03-09")

locPrefix <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
locSuffix <- ".csv"

# Loop through the dates
indx <- as.integer(endDate - startDate)
#for (i in 0:indx) {

  # Format the next date for the file name
  sdate <- format(startDate + indx, format="%m-%d-%Y")
  
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
  #check <- safe_mutate(covid, Last_Update = as.POSIXct(Last_Update, tz="", format = "%m/%d/%y %H:%M"))
  #check <- safe_mutate(covid, Last_Update = as.Date(Last_Update, format = "%m/%d/%y"))
  check <- safe_mutate(covid, Last_Update = as.Date(as.character(Last_Update), tryFormats = c("%m/%d/%y", "%Y-%m-%d")))
  #check <- safe_mutate(covid, Last_Update = as.character(Last_Update))
  if (!is.null(check$result))
    covid <- check$result
  #covid <- mutate(covid, Last_Update = as.Date(Last_Update))
  
  
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
  
  covid <- covid %>% 
    select(Province_State, Country_Region, Last_Update, Latitude, Longitude, 
           Confirmed, Deaths, Recovered)
  
#}

  





################################################################
# Get the first set of data based on the start date
sdate <- format(startDate, format="%m-%d-%Y")
loc <- paste(locPrefix, sdate, locSuffix, sep = "")
covid1 <- read_csv(loc)

#Rename and reorder columns
check <- safe_mutate(covid1, Province_State = `Province/State`)
if (!is.null(check$result))
  covid1 <- check$result
check <- safe_mutate(covid1, Country_Region = `Country/Region`)
if (!is.null(check$result))
  covid1 <- check$result
check <- safe_mutate(covid1, Last_Update = `Last Update`)
if (!is.null(check$result))
  covid1 <- check$result
# Convert Last_Update to date
check <- safe_mutate(covid1, Last_Update = as.POSIXct(Last_Update, tz="", format = "%m/%d/%y %H:%M"))
if (!is.null(check$result))
  covid1 <- check$result

check <- safe_mutate(covid1, Latitude = `Lat`)
if (!is.null(check$result))
  covid1 <- check$result
if (is.null(covid1$Latitude))
  covid1 <- covid1 %>% mutate(Latitude = NA)

check <- safe_mutate(covid1, Longitude = `Long_`)
if (!is.null(check$result))
  covid1 <- check$result
if (is.null(covid1$Longitude))
  covid1 <- covid1 %>% mutate(Longitude = NA)

covid1 <- covid1 %>% 
  select(Province_State, Country_Region, Last_Update, Latitude, Longitude, 
         Confirmed, Deaths, Recovered)


# filter to just US data
covid2 <- covid1 %>%
  filter(`Country_Region` == 'US')

covid2 <- rbind(covid, covid2)
