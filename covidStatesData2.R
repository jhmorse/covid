library(tidyverse)

#########################################
# Filter the states down to just those of interest

# Default state for the function is Washington
getStates <- function(df, cSt = c('WA'), cState = c('Washington')) {
  
  # Loop through each desired State and separate out the results
  for (i in 1:length(cState)) {
    
    # Filter down to just the State
    # Note that State will be in either the St column
    # or the Province column
    covidst <- filter(covidus, St == cSt[i] | Province == cState[i])
    covidst <- filter(covidst, St == cSt[i] | is.na(St))
    
    # Adjust the Province and State info if Admin was present
    # Row by row, check to see if St was NA, which occurs in more recent source files
    covidst <- covidst %>%
      rowwise %>%
      mutate(St = ifelse(is.na(St), cSt[i], St)) %>%    # replace the NA value with a State
      mutate(Province = ifelse(Province == cState[i], Admin, Province))  # replace the Province value with the City/location info in Admin
    # Drop the Admin, as no longer necessary
    covidst <- select(covidst, -Admin)
    
    # At this point St column has a State abbreviation
    # & Province has a City/location within the State
    
    # Eliminate duplicates that may appear for the same Province/St/Date
    # Take the max value of the duplicates
    dedup <- covidst %>%
      group_by(Province, St, Country_Region, Last_Update, Latitude, Longitude) %>%
      summarize(Confirmed = max(Confirmed),
                Deaths = max(Deaths),
                Recovered = max(Recovered))
    
    
    # Group by date, getting totals across all city/locations in the state
    # This gives us day by day totals of cases for the state
    state_ttl <- dedup %>%
      group_by(St, Last_Update) %>%
      summarize(confirmed = sum(Confirmed, na.rm = TRUE),
                deaths = sum(Deaths, na.rm = TRUE), 
                recovered = sum(Recovered, na.rm = TRUE)) %>%
      arrange(St, Last_Update)
    
    # Now we need the daily total for each day for confirmed cases.
    # The values in confirmed, deaths, recovered are all cummualtive totals.
    # We need to get a daily difference for a daily total.
    
    # The diff function will calculate differences for each row,
    # but eleiminates the first row, becasue there is not difference for it.
    cdaily <- diff(state_ttl$confirmed)          # Calcualte the differences and store separately            
    # Now we add the first row back in, wihtout a cummulative difference.
    cdaily <- c(state_ttl$confirmed[1], cdaily)
    
    # With all the differences calculated,
    # we can add this as a new column to the data set using cbind.
    state_ttl <- cbind(state_ttl, daily_confirmed = cdaily)
    
    # Eliminate negative values, as those are assumed bad data sets
    state_ttl <- state_ttl %>%
      mutate(daily_confirmed = ifelse(daily_confirmed >= 0, daily_confirmed, 0))
    

    # We can now repeat the same process for the daily deaths.
    ddaily <- diff(state_ttl$deaths)  # Calcualte the differences and store separately
    ddaily <- c(state_ttl$deaths[1], ddaily)  # add back the first day as the first value
    state_ttl <- cbind(state_ttl, daily_deaths = ddaily)    # bind the results on to the main data set
    # Eliminate negative values, as those are assumed bad data sets
    state_ttl <- state_ttl %>%
      mutate(daily_deaths = ifelse(daily_deaths >= 0, daily_deaths, 0))
    
    
    # NOTE: At this time (8/16/2020), we are seeing no values
    # for the recovered column, so daily values are nto calculated.
    # This is a gap in the data.
    
    # Take this state and add it to a total data set
    if (i == 1)
      covid_ttl <- state_ttl
    else
      covid_ttl <- rbind(covid_ttl, state_ttl)    
  }
  
  # At this point, all States data is in covid_ttl
  return(covid_ttl)
}


