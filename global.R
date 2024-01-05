# Load libraries ------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(purrr)
library(ggplot2)
library(plotly)
library(stringr)
library(shiny)
library(shinymanager)
library(shinydashboard)
library(wordcloud)
library(RColorBrewer)
library(rsconnect)

# Import Data ---------------------------------------------------------------

sightings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
stop_words_en <- read.table("stopwords-en.txt", fill = TRUE, 
                            stringsAsFactors = FALSE )

# Exploratory Data Analysis -------------------------------------------------

# check the structure of sightings and places
# glimpse(sightings)
# glimpse(places)

# calculate the number of unique values in each column of sightings and places
unique_values_sightings <- sightings %>%
  sapply(function(x) length(unique(x))) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

unique_values_places <- places %>%
  sapply(function(x) length(unique(x))) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

# calculate the number of NA values in each column of sightings and places
na_values_sightings <- sightings %>%
  sapply(function(x) sum(is.na(x))) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

na_values_places <- places %>%
  sapply(function(x) sum(is.na(x))) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

# analize the top 10 countries sightings
top_countries <- sightings %>%
  group_by(country_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# get the most common shape for each country
most_common_shape <- sightings %>%
  group_by(country_code) %>%
  count(shape) %>%
  slice_max(order_by = n, n = 1, with_ties = TRUE) %>%
  arrange(desc(n)) %>%
  head(10)

# group by country / shape, counting the number of sightings in US
common_shapes <- sightings %>%
  group_by(country_code, shape) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  head(10)

# identify outliers of reported_duration
sightings$duration_seconds <- as.numeric(as.character(sightings$duration_seconds))
sightings$duration_seconds <- na.omit(sightings$duration_seconds)

Q1 <- quantile(sightings$duration_seconds, 0.25, na.rm = TRUE)
Q3 <- quantile(sightings$duration_seconds, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

sightings_without_outliers <- sightings %>%
  filter(duration_seconds >= (Q1 - 1.5 * IQR) & duration_seconds <= (Q3 + 1.5 * IQR))

# check for unique day_part values
sightings %>%
  count(day_part, sort = TRUE)

# Data Wrangling ------------------------------------------------------------

# create a new column with the location
sightings <- sightings %>%
  mutate(location = paste(city, state, country_code, sep = ", "))

places <- places %>%
  mutate(location = paste(city, state, country_code, sep = ", "))

# remove rows with missing values and filter by country_code and shape
sightings <- na.omit(sightings) %>%
  filter(country_code == "US") %>%
  filter(shape != "unknown") %>%
  distinct(location, .keep_all = TRUE)

places <- na.omit(places)  %>%
  filter(country_code == "US") %>%
  select(-city, -country_code, -alternate_city_names, -state, -timezone)%>%
  distinct(location, .keep_all = TRUE)

# inner join of the data sets
UFO_sightings <- sightings %>%
  inner_join(places, by = "location")

# remove columns
UFO_sightings <- select(UFO_sightings, -reported_date_time, 
                        -posted_date, -reported_duration, -has_images)

# convert time to time format
UFO_sightings <- UFO_sightings %>% 
  mutate(year = year(reported_date_time_utc),
         month = month(reported_date_time_utc),
         day = day(reported_date_time_utc),
         hour = hour(reported_date_time_utc),
         minute = minute(reported_date_time_utc))

# replace values of day_part
evening <- c("civil dusk", "nautical dusk", "astronomical dusk")
sunrise <- c("astronomical dawn", "nautical dawn", "civil dawn")

UFO_sightings <- UFO_sightings %>%
  mutate(day_part = tolower(day_part),
         day_part = case_when(
           day_part %in% evening ~ "evening",
           day_part %in% sunrise ~ "sunrise",
           is.na(day_part) ~ "unknown",
           TRUE ~ day_part
         ))

# process the words in the summary column of UFO_sightings
process_words <- function(summary) {
  summary %>%
    str_remove_all("[[:punct:]]") %>%  # eliminate punctuation
    str_remove_all("[0-9]") %>%  # eliminate numbers
    trimws() %>%  # eliminate spaces
    str_split(" ") %>%  # convert words to vectors
    unlist() %>%  # unlist the result
    .[. != ""] %>%  # eliminate blank spaces
    str_to_lower()  # turn into lowercase
}

words <- process_words(sightings$summary)

# convert the stop words to character
stop_words_en <- as.character(stop_words_en$V1)

# create a frequency table for the words and remove stop words
tabla <- as.data.frame(table(words))
tabla <- tabla[order(tabla$Freq, decreasing = T),]
tabla <- tabla[!(tabla$words %in% stop_words_en),]

# calculate the total number of words and the number of unique words
total_words <- length(words)
unique_words <- length(unique(words))

# create a data frame with the 
data_map <- UFO_sightings %>%
  select(location, latitude, longitude) %>%
  group_by(location, latitude, longitude) %>%
  summarise(sightings = n(), .groups = "drop")

# Credentials -----------------------------------------------------------------

# login
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin", "user"),
  stringsAsFactors = FALSE
)

set_labels(
  language = "en",
  "Please authenticate" = "UFO Sightings Shiny App",
  "Username:" = "User:",
  "Password:" = "Password:"
)

# Define connect to shinyapp.io ------------------------------------------------
rsconnect::setAccountInfo(name='gastigk',
                          token='3576E765C09566C42AAC32D431EEA25D',
                          secret='1lFV0zTH58ArEOXC5/RVdeFZgzbKI5vw2/ikEyfJ')