library(tidyverse)
library(gridExtra)
library(scales)

#############################################################################
# Graph the results

# specify the number of graphs expected
numRows = length(cState)


# Assumes the data of interest is in covid_ttl

pc <- ggplot(covid_ttl, aes(x = Last_Update)) +
  geom_bar(mapping = aes(y = daily_confirmed, fill=St), stat = "identity") +
  labs(title='Daily Cases', y='# Cases', x='Day') +
  geom_smooth(mapping = aes(y = daily_confirmed), se = FALSE) +
  facet_wrap(~ St, nrow=numRows, scales = 'free_y')
pd <- ggplot(covid_ttl, aes(x = Last_Update)) +
  geom_bar(mapping = aes(y = daily_deaths, fill=St), stat = "identity") +
  labs(title='Daily Deaths', y='# Deaths', x='Day') +
  geom_smooth(mapping = aes(y = daily_deaths), se = FALSE) +
  facet_wrap(~ St, nrow=numRows, scales = 'free_y')
pcumm <- ggplot(covid_ttl, aes(x = Last_Update)) +
  scale_y_log10(breaks = 10^(0:10), labels=trans_format("log10", math_format(10^.x))) +
  geom_line(mapping = aes(y = confirmed, color=St), size=1) +
  geom_line(mapping = aes(y = deaths, color=St), size=1, linetype=2) +
  labs(title='Confirmed Cases: Cummulative', y='Total Cases (Log Scale compares Rate of increase)', x='Day')
# geom_line(mapping = aes(y = deaths), color = "red")

