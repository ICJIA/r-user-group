library(tidyverse)

#Import txt file
data <- read.table(file = 'rusergroup_dataset.txt', sep = '\t', header = TRUE)

#Clean dataset
data <- data %>% 
  group_by(Identifier, Event) %>% 
  mutate(PriorArrests = sequence(n())) %>% 
  mutate(PriorArrests = replace(PriorArrests, Event == 'Conviction', NA)) %>% 
  group_by(Identifier) %>% 
  fill(PriorArrests)

head(data, 20)