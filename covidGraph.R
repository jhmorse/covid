library(tidyverse)
library(gridExtra)
library(scales)

#############################################################################
# Graph the results

# This funciton builds three graph objects.
# 1. A bar chart with daily cases.
# 2. A bar chart with daily deaths.
# 3. A line chart with cummulative cases and deaths.

# The firt parameter is the data set.
# The second parameter is the number of states to plot.
buildGraphs <- function(df, numRows = 1) {

  # Assumes the data of interest is in covid_ttl
  
  pc <- ggplot(df, aes(x = Last_Update)) +
    geom_bar(mapping = aes(y = daily_confirmed, fill=St), stat = "identity") +
    labs(title='Daily Cases', y='# Cases', x='Day') +
    geom_smooth(mapping = aes(y = daily_confirmed), se = FALSE) +
    facet_wrap(~ St, nrow=numRows, scales = 'free_y')
  
  pd <- ggplot(df, aes(x = Last_Update)) +
    geom_bar(mapping = aes(y = daily_deaths, fill=St), stat = "identity") +
    labs(title='Daily Deaths', y='# Deaths', x='Day') +
    geom_smooth(mapping = aes(y = daily_deaths), se = FALSE) +
    facet_wrap(~ St, nrow=numRows, scales = 'free_y')
  
  pcumm <- ggplot(df, aes(x = Last_Update)) +
    scale_y_log10(breaks = 10^(0:10), labels=trans_format("log10", math_format(10^.x))) +
    geom_line(mapping = aes(y = confirmed, color=St), size=1) +
    geom_line(mapping = aes(y = deaths, color=St), size=1, linetype=2) +
    labs(title='Confirmed Cases: Cummulative', y='Total Cases (Log Scale compares Rate of increase)', x='Day')
  
  # Return all three chart objects in a list
  return(list(pc, pd, pcumm))

}


