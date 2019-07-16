# Re-Sampling Age of Callers using Age of Clients-----

# The goal is to:
#
# randomly select 60 samples of 58841 "callers" using the caller distribution
# then randomly assign each "caller" a primary date based on a random selection
# of dates from InfoNet clients with the same age. then calculate means for each sample



# for most purposes we want -1 in InfoNet to become NA,
# in this case however, that is not desirable because when
# we randomly select values of callers, their age may often be NA,
# in this case, we would want to select primary 

#Step 1: pick a random case f
# describe(sample(AllSACalls$Age, 90000, replace=TRUE))
# 
# # this is an example function created to test bootstrap
# # it works because it
# bootmean <- function(data, i){
#   d <- data[i]
#   return(mean(d, na.rm = TRUE))  
# }

# the below bootstrap works with simple function, however what I need is a
# function that 


#save a bootstrap
# boottest <- filter(AllSACalls,ClientType=='Victim')%>%
#   #sample_n(58841, replace=TRUE) %>% #redundant with boot more or less
#   select('Age') %>%
#   .$Age %>%
#   boot(statistic=bootmean,R=900)

#returns the CI for bootstrap
# boot.ci(boottest, conf = 0.95, type='perc')

#same as a single bootstrap from Victim Calls
# filter(AllSACalls,(ClientType=='Victim'))%>%
#   sample_n(58841, replace=TRUE) %>% #redundant with boot more or less
#   select('Age') %>%
#   .$Age %>%
#   mean(na.rm=TRUE)

#### attempt to do the above with a 'replace with probabilistic value'
# first we create a convenience subset with Victim filter and only 
# "callers" over 12
# VictimCallerswithAge <- filter(AllSACalls,(ClientType=='Victim' & Age >= 12 )) %>%
#                         select(Age)
# describe(VictimCallerswithAge$Age) #everyone under 12 excluded
# 
# 
# 
# # then do the same with Clients in InfoNet
# ClientsAgeover12 <- filter(ClientAgeXDate,Age>=12)
# 
# mean(ClientsAgeover12$Age)
# describe(ClientAgeXDate$Age)
# 
# # filter on sample because it selects by age, then sample 1 row
# 
# # this almost works, unable to complete a loop that builds the sample properly
# 
# SampleX <- filter(ClientsAgeover12,Age==sample(VictimCallerswithAge$Age,1)) %>%
#   sample_n(1)
# 
# 
# SampleX <- bind_rows(SampleX,
#   
# filter(ClientsAgeover12,Age==sample(VictimCallerswithAge$Age,1)) %>%
# sample_n(1))
# 
# filter(AllSACalls,between(Age,18,24)&ClientType=='Victim') %>% count()
# filter(AllSACalls,Age==25&ClientType=='Victim') %>% count()
# filter(AllSACalls,between(Age,0,17)&ClientType=='Victim') %>% count()
# filter(ClientAgeXDate,between(Age,0,17)) %>% count()
# 