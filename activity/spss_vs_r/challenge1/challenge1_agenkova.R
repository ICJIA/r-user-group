
#### R Group Exercise
rm(list = ls())

# Load packages (or install before loading)

library(tidyverse) #dplyr, ggplot and other pseudo standard R tools
library(data.table) #large data and efficient syntax
library(foreign) #  for reading data

#import data using the "foreign" package
getwd()
setwd("C:/Users/agenkova/Documents/R Group exercises")
chri_sim_data <- read.csv("chri_sim_data.csv")  
str(chri_sim_data)

## calculate prior arrests##
chri_sim_data$p.arrests <- ave(chri_sim_data$EventNumerical, 
                          chri_sim_data$Identifier, 
                          FUN=cumsum)


## Filter convictions and delete EventNunerical
### Produces a table with prior arrests at each conviction for each person

Conv.Table=filter(chri_sim_data, Event=="Conviction")
Conv.Table$EventNumerical=NULL


