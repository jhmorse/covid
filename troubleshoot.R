
library(tidyverse)
library(gridExtra)


# Create a safe_mutate function
safe_mutate <- safely(mutate)
safe_diff <- possibly(diff, otherwise = 0)

startDate <- as.Date("2020-03-22")
endDate <- as.Date("2020-03-22")

sSt <- 'NY'
sState <- 'New York'
sCountry <- 'US'
cSt <- c('WA', 'CA', 'NY')
cState <- c('Washington', 'California', 'New York')

locPrefix <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
locSuffix <- ".csv"

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

# Filter down to just Washington State
covidwa <- covidus %>%
  separate(Province_State, into = c("Province", "St"), sep = ", ") %>%
  filter(St == sSt | Province == sState)
covidwa <- filter(covidwa, St == sSt | is.na(St))

#########################################
# Adjust the Province and State info if Admin2 was present

# Row by row, cehck to see if St was NA, which occurs above in the newer files
covidwa <- covidwa %>%
  rowwise %>%
  mutate(St = ifelse(is.na(St), sSt, St)) %>%    # replace the St value
  mutate(Province = ifelse(Province == sState, Admin, Province))  # replace the Admin value
# Drop the Admin, as no longer necessary
covidwa <- select(covidwa, -Admin)


# Eliminate duplicates that may appear for the same Province/St/Date
# Take the max value of the duplicates
dedup <- covidwa %>%
  group_by(Province, St, Country_Region, Last_Update) %>%
  summarize(Latitude = max(Latitude),
            Longitude = max(Longitude),
            Confirmed = max(Confirmed),
            Deaths = max(Deaths),
            Recovered = max(Recovered))

# Group by date, getting totals
# This gives us day by day totals of cases
ttl <- dedup %>%
  group_by(Last_Update) %>%
  summarize(confirmed = sum(Confirmed, na.rm = TRUE),
            deaths = sum(Deaths, na.rm = TRUE), 
            recovered = sum(Recovered, na.rm = TRUE)) %>%
  arrange(Last_Update)

# Now we need the daily total for each day
# confirmed, deaths, recovered are all cummualtive totals
# we need to get a daily difference for a daily total
cdaily <- diff(ttl$confirmed)          # Calcualte the differences and store separately            
cdaily <- c(ttl$confirmed[1], cdaily)  # add the first day in as the first value
ttl <- cbind(ttl, cdaily)      # bind the results on to the main data set
# ELiminate negative values, as those are assumed bad data sets
ttl <- ttl %>%
  mutate(daily_confirmed = ifelse(cdaily >= 0, cdaily, 0)) %>%
  select(-cdaily)

p1 <- ggplot(ttl, aes(x = Last_Update)) +
  geom_bar(mapping = aes(y = daily_confirmed), stat = "identity", colour="red")
p2 <- ggplot(ttl, aes(x = Last_Update)) +
  #scale_y_log10() +
  geom_line(mapping = aes(y = confirmed)) + 
  geom_line(mapping = aes(y = deaths), color = "red")

grid.arrange(p1, p2)
