library(tidyverse)

# Define standard vairables for states of interest
cSt <- c('WA', 'CO', 'MN', 'CA', 'MT')
cState <- c('Washington', 'Colorado', 'Minnesota', 'California', 'Montana')

cSt <- c('WA')
cState <- c('Washington')


#########################################
# Filter the states down to just those of interest

# Loop through each desired State and separate out the results
for (i in 1:length(cState)) {
  
  # Filter down to just the State
  covidst <- filter(covidus, St == cSt[i] | Province == cState[i])
  covidst <- filter(covidst, St == cSt[i] | is.na(St))
  
  
  # Adjust the Province and State info if Admin2 was present
  # Row by row, cehck to see if St was NA, which occurs above in the newer files
  covidst <- covidst %>%
    rowwise %>%
    mutate(St = ifelse(is.na(St), cSt[i], St)) %>%    # replace the St value
    mutate(Province = ifelse(Province == cState[i], Admin, Province))  # replace the Admin value

  # Drop the Admin, as no longer necessary
  covidst <- select(covidst, -Admin)
  
  
  # Eliminate duplicates that may appear for the same Province/St/Date
  # Take the max value of the duplicates
  dedup <- covidst %>%
    group_by(Province, St, Country_Region, Last_Update, Latitude, Longitude) %>%
    summarize(Confirmed = max(Confirmed),
              Deaths = max(Deaths),
              Recovered = max(Recovered))
  
  # Group by date, getting totals
  # This gives us day by day totals of cases
  state_ttl <- dedup %>%
    group_by(St, Last_Update) %>%
    summarize(confirmed = sum(Confirmed, na.rm = TRUE),
              deaths = sum(Deaths, na.rm = TRUE), 
              recovered = sum(Recovered, na.rm = TRUE)) %>%
    arrange(St, Last_Update)
  
  # Now we need the daily total for each day for confirmed cases
  # confirmed, deaths, recovered are all cummualtive totals
  # we need to get a daily difference for a daily total
  cdaily <- diff(state_ttl$confirmed)          # Calcualte the differences and store separately            
  cdaily <- c(state_ttl$confirmed[1], cdaily)  # add the first day in as the first value
  state_ttl <- cbind(state_ttl, daily_confirmed = cdaily)      # bind the results on to the main data set
  # ELiminate negative values, as those are assumed bad data sets
  state_ttl <- state_ttl %>%
    mutate(daily_confirmed = ifelse(daily_confirmed >= 0, daily_confirmed, 0))
  
  # Now we need the daily total for each day for deaths
  # confirmed, deaths, recovered are all cummualtive totals
  # we need to get a daily difference for a daily total
  ddaily <- diff(state_ttl$deaths)          # Calcualte the differences and store separately            
  ddaily <- c(state_ttl$deaths[1], ddaily)  # add the first day in as the first value
  state_ttl <- cbind(state_ttl, daily_deaths = ddaily)      # bind the results on to the main data set
  # ELiminate negative values, as those are assumed bad data sets
  state_ttl <- state_ttl %>%
    mutate(daily_deaths = ifelse(daily_deaths >= 0, daily_deaths, 0))
  
  if (i == 1)
    covid_ttl <- state_ttl
  else
    covid_ttl <- rbind(covid_ttl, state_ttl)
}

# The data of interest is now in covid_ttl