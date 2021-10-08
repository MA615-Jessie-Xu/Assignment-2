# Prerequisites
library(knitr)
library(tidyverse)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)

# STEP 1: Read excel file
br0 <- read.csv("https://raw.githubusercontent.com/MA615-Jessie-Xu/Assignment-2/main/birthrate.csv") 
ncol(br0)  #> 217

sy0 <- read.csv("https://raw.githubusercontent.com/MA615-Jessie-Xu/Assignment-2/main/schoolyear_female.csv")
# the mean years of school attended by woman (25 years and older), 1799-2014
ncol(sy0)  #>  41


# STEP 2: Retain data for years that appear in both datasets
br0 <- br0[ ,c(1,172:211)]
# view(br0) # check the data


# STEP 3: Turn to tidy data
br1 <- br0 %>%
  pivot_longer(!country, names_to = "years", values_to = "birthrate")   
sy1 <- sy0 %>%
  pivot_longer(!country, names_to = "years", values_to = "schoolyears")


# STEP 4: Using left join
tidy <- br1 %>%
  left_join(sy1)
tidy$year <- gsub("X", "", tidy$years) 
tidy$year <- as.numeric(tidy$year)

# Other tidy process
# colnames(tidy)
# delete the "years" which has X before the year
tidy<-tidy[,-2]

# summary(tidy) 
# Delete the lines containing NA
finetidy <- na.omit(tidy)
summary(finetidy)

# add a column having corresponding continent name in 'Continent' column
## according to :
## https://stackoverflow.com/questions/51258448/aggregate-list-of-countries-by-continents
library(countrycode)
finetidy <- finetidy %>%
mutate(continent = countrycode(country, 'country.name', 'continent'))

# aggregate birthrate based on Continent 
finetidy %>%
  group_by(continent) %>%
  summarise(birthrate_Mean = mean(birthrate))
# aggregate the mean schoolyears of women based on Continent 
finetidy %>%
  group_by(continent) %>%
  summarise(birthrate_Mean = mean(schoolyears))

