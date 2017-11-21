#Filtering rows
# Load the dplyr package
library(dplyr)

# Print the votes dataset
votes

# Filter for votes that are "yes", "abstain", or "no"
votes %>%
filter(vote <= 3)

#Adding year column
votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945)

#Adding country column
# Load the countrycode package
library(countrycode)

# Convert country code 100
countrycode(100, "cown", "country.name")

# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945) %>%
  mutate(country = countrycode(ccode, "cown", "country.name"))

#Summarising dataset
# Print votes_processed
votes_processed

# Find total and fraction of "yes" votes
votes_processed %>%
summarize(total = n(), percent_yes = mean(vote == 1))

#Summarize by year

votes_processed %>%
group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Summarize by country: by_country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

#Sorting by percentage of yes votes
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Print the by_country dataset
by_country

arrange(by_country, percent_yes)

arrange(by_country, desc(percent_yes))

# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>%
  filter(!total < 100)

#Plotting line graph
# Define by_year
by_year <- votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Load the ggplot2 package
library(ggplot2)

# Create line plot
ggplot(by_year,aes(x=year, y=percent_yes)) +
geom_line()


# Change to scatter plot and add smoothing curve
ggplot(by_year, aes(year, percent_yes)) +
  geom_point() +
  geom_smooth()

# Group by year and country: by_year_country
by_year_country <- votes_processed %>%
  group_by(year,country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

#Analysing UK data
# Create a filtered version: UK_by_year
UK_by_year <- by_year_country %>%
filter(country == "United Kingdom")

# Line plot of percent_yes over time for UK only
ggplot(UK_by_year, aes(x=year, y=percent_yes)) +
geom_line()

#Plotting multiple countries
# Vector of four countries to examine
countries <- c("United States", "United Kingdom",
               "France", "India")

# Filter by_year_country: filtered_4_countries
filtered_4_countries <- by_year_country %>%
filter(country %in% countries)

# Line plot of % yes in four countries
ggplot(filtered_4_countries, aes(x=year, y=percent_yes, color = country)) +
geom_line()

#Faceting with six countries
# Vector of six countries to examine
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries <- by_year_country %>%
filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(x=year, y=percent_yes)) +
geom_line() +
facet_wrap(~country)

#Faceting with free y axis
# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")

#Faceting with more countries
# Add three more countries to this list
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India", "Russia", "China", "Canada")

# Filtered by_year_country: filtered_countries
filtered_countries <- by_year_country %>%
  filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_countries, aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")

#Linear Regression on US Data
# Percentage of yes votes from the US by year: US_by_year
US_by_year <- by_year_country %>%
  filter(country == "United States")

# Print the US_by_year data
US_by_year

# Perform a linear regression of percent_yes by year: US_fit
US_fit <- lm(percent_yes ~ year, US_by_year)

summary(US_fit)

#Using broom package to tidy
# Load the broom package
library(broom)

# Call the tidy() function on the US_fit object
tidy(US_fit)

#Combining models from diffrent countries
# Fit model for the United Kingdom
UK_by_year <- by_year_country %>%
  filter(country == "United Kingdom")
UK_fit <- lm(percent_yes ~ year, UK_by_year)

# Create US_tidied and UK_tidied
US_tidied <- tidy(US_fit)
UK_tidied <- tidy(UK_fit)

# Combine the two tidied models
bind_rows(US_tidied, UK_tidied)

# Load the tidyr package
library(tidyr)

# Nest all columns besides country
by_year_country %>%
nest(-country)

# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

# Print the nested data for Brazil to view
nested$data[[7]]

#Performing linear regression on nested dataset
library(purrr)

# Perform a linear regression on each item in the data column
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes~year, data = .)))
  
# Add another mutate that applies tidy() to each model
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .))) %>%
  mutate(tidied = map(model,tidy))

## Add one more step that unnests the tidied column
country_coefficients <- by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
tidied = map(model, tidy)) %>%
unnest(tidied)         

# Print the resulting country_coefficients variable
country_coefficients

# Filter for only the slope terms
slope_terms <- country_coefficients %>%
  filter(term == "year")

# Add p.adjusted column, then filter
slope_terms %>%
mutate(p.adjusted = p.adjust(p.value)) %>%
filter(p.adjusted < .05)

# Filter by adjusted p-values
filtered_countries <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)

# Sort for the countries increasing most quickly
filtered_countries %>%
arrange(desc(estimate))

# Sort for the countries decreasing most quickly
filtered_countries %>%
arrange(estimate)

# Load dplyr package
library(dplyr)

# Print the votes_processed dataset
votes_processed

# Print the descriptions dataset
descriptions

# Join them together based on the "rcid" and "session" columns
votes_joined <- votes_processed %>%
inner_join(descriptions, by = c("rcid", "session"))

#Visualizing colonialism votes
# Filter, then summarize by year: US_co_by_year
US_co_by_year <- votes_joined %>%
  filter(country == "United States", co == 1) %>%
  group_by(year) %>%
  summarize(percent_yes = mean(vote == 1))

# Graph the % of "yes" votes over time
ggplot(US_co_by_year, aes(year, percent_yes)) +
  geom_line()

#Using gather to tidy dataset
# Gather the six me/nu/di/hr/co/ec columns
votes_joined %>%
gather(topic, has_topic, me:ec)

# Perform gather again, then filter
votes_gathered <- votes_joined %>%
gather(topic, has_topic, me:ec) %>%
filter(has_topic == 1)

#Recoding by topic
# Replace the two-letter codes in topic: votes_tidied
votes_tidied <- votes_gathered %>%
  mutate(topic = recode(topic,
                        me = "Palestinian conflict",
                        nu = "Nuclear weapons and nuclear material",
                        di = "Arms control and disarmament",
                        hr = "Human rights",
                        co = "Colonialism",
                        ec = "Economic development"))

# Print votes_tidied
votes_tidied

# Summarize the percentage "yes" per country-year-topic
by_country_year_topic <- votes_tidied %>%
group_by(country,year,topic) %>%
summarize(total = n(), percent_yes = mean(vote == 1)) %>%
ungroup()

# Print by_country_year_topic
by_country_year_topic

# Filter by_country_year_topic for just the US
US_by_country_year_topic <- by_country_year_topic %>%
filter(country == "United States")

# Visualize % yes over time for the US, faceting by topic
ggplot(US_by_country_year_topic, aes(x= year, y = percent_yes)) +
geom_line() +
facet_wrap(~topic)


# Fit model on the by_country_year_topic dataset
country_topic_coefficients <-  by_country_year_topic %>%
  nest(-country, -topic) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Print country_topic_coefficients
country_topic_coefficients

# Create country_topic_filtered
country_topic_filtered <- country_topic_coefficients %>%
filter(term == "year") %>%
mutate(p.adjusted = p.adjust(p.value)) %>%
filter(p.adjusted < .05)

# Examining an obscure country
vanuatu_by_country_year_topic <- by_country_year_topic %>%
filter(country == "Vanuatu")

# Plot of percentage "yes" over time, faceted by topic
ggplot(vanuatu_by_country_year_topic, aes(x = year , y = percent_yes)) +
geom_line() +
facet_wrap(~topic)

