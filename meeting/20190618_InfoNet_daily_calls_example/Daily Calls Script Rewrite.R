# Load Libraries-------------------------------

library(TSA) #fourier
library(tidyverse) #because generally useful
library(lattice) #arguably better histograms, etc.
library(psych) #contains various descriptive stats like im used to
library(lubridate) #handling dates
library(sjstats) #eta squared and ANOVA stats, nice to have
library(pwr) #required by sjstats
library(DescTools) #has a version of Lambda for chi^2
library(prophet) #facebook seasonal forecasting, contains good holiday data
library(boot) #bootstrapping stuff
library(effsize) #other version of cohen.d
library(MASS) # bunch of stats, including studentized residuals
select <- dplyr::select #use tidy/dplyr version of select, not MASS
# Load Source File----------------------------
source("X://R Scripts/Karl.R")

# Load and Process Data-----------------------
AllSACalls <- Hotline_Folder('SACalls.csv') %>% 
              read_csv(col_types='iDfidiffffff') %>%
              holidize('Date') %>%
              mutate(Age=na_if(Age, -1)) %>%
              # Call Duration needed/desired to remove outliers from Call Time
              add_column( Call_duration=case_when(
                          .$TotalTime %>% between(0,.24)  ~ 0,
                          .$TotalTime %>% between(.25, .49) ~ .25,
                          .$TotalTime %>% between(.5, .74)  ~ .5,
                          .$TotalTime %>% between(.75, .99)   ~ .75,
                          .$TotalTime %>% between(1, 1.24)   ~ 1,
                          .$TotalTime %>% between(1.25, 1.49)   ~ 1.25,
                          .$TotalTime %>% between(1.5, 1.74)   ~ 1.5,
                          .$TotalTime %>% between(1.74, 1.99)   ~ 1.75,
                          .$TotalTime %>% between(2, 2.24)   ~ 2,
                          .$TotalTime >2.25 ~ 2.25
                          )
                           )



DailySACalls <- #start with straight dates to eventually have zeros for call counts
                tibble(Date = 
                         seq(as.Date("2002-07-01"), 
                             as.Date("2019-04-30"), 
                             by="days")
                       ) %>%
                
                left_join( #Instead of loading counts from SQL we load raw data
                  Hotline_Folder('SACalls.csv') %>% 
                  read_csv(col_types='iDfidiffffff') %>%
                  filter(ClientType=="Victim")%>%  
                  group_by(Date) %>%
                  count(Date), by=c("Date")
                  ) %>%
                
                rename(Calls=n) %>%              
                mutate(Calls=replace_na(Calls,0)) %>%         
  
                left_join( #add column for Temps
                  Public_Folder("IL Mean Temp Aurora.csv") %>% read_csv(),
                  by=c("Date")
                  ) %>%
                
                left_join( #add DJIA
                  Public_Folder("DJIA.csv") %>% read_csv(),
                  by=c("Date")
                  ) %>%
                
                holidize('Date') %>%
                
                arrange(Date) %>% #reorder file by date
                
                rowid_to_column("DayID") #create a DayID by date

Male_Female_Callers <- Hotline_Folder('Male and Female Callers and Clients.csv') %>%
                       read_csv() %>%
                       select(X1,Male,Female)          %>%
                       Convert_to_Contigency_Table('X1')

Calls_by_Center_by_Weekday <- Hotline_Folder('calls by center by day.csv') %>%
                              read_csv() %>%
                              Convert_to_Contigency_Table('Center')

ClientAgeXDate <- Hotline_Folder('SA Clients Age and PrimDate for Resample.csv') %>%
                  read_csv()

# Fourier Analysis (Inactive)----------------------

## Run the analyis
#Fourier.SACalls <- periodogram(DailySACalls$Calls)

## Builds a dataframe allowing quick inspection
## 1/freq returns frequency to source units, e.g. 3.5 days
## high spec is relatively higher strength of signal
#Cycles_Calls <- data.frame(1/Fourier.SACalls$freq, Fourier.SACalls$spec)

## below code is correct for creating variables to test sin/cos wave in regression
## base on a period of 6.5 
#Harass.FakeMonth$sin6.5FMonth <- sin(2*pi*Harass.FakeMonth$fakemonth/6.5)
#Harass.FakeMonth$cos6.5FMonth <- cos(2*pi*Harass.FakeMonth$fakemonth/6.5)

# Model Building-------------------------------

#  Model 1 is Year as a factor
lm(Calls ~ as.numeric(year(Date)), data=DailySACalls) %>% test_lm()

# Model 2 is Time as an integer
lm(Calls ~ DayID, data=DailySACalls) %>% test_lm()

# Model 3 is Time plus Month as factor
lm(Calls ~ DayID+Month, data=DailySACalls) %>% test_lm()

##Tukey's HSD for Model 3 Monthes
#Model3HSD<-aov(Calls~DayID+Month,data=DailySACalls)
#TukeyHSD(Model3HSD,"Month",ordered=FALSE)
#describeBy(DailySACalls$Calls, group= DailySACalls$Month)

# Model 4 is time plus Temperature
lm(Calls ~ DayID+Fahrenheit, data=DailySACalls) %>% test_lm()

# Model 4b is time plus daily % change on the Dow
lm(Calls ~ DayID+ get('Change %')  , data=DailySACalls) %>% test_lm()

# Model 5 is time plus day of the week
lm(Calls ~ DayID+Weekday, data=DailySACalls) %>% test_lm()

# Model 6 is time plus day of week and whether it was a holiday
lm(Calls ~ DayID+Weekday+holiday, data=DailySACalls) %>% test_lm()

# Model 7 is time plus categorical type of day,
#type of day is hybrid of holiday vs normal day
lm(Calls ~ DayID+TypeofDay, data=DailySACalls) %>% test_lm()

# Model 8 is time plus workday
lm(Calls ~DayID+WorkDay, data=DailySACalls) %>% test_lm()

# Model 9 tested an interaction term between DayID and Workday
# tests showed negligible value
DailySACalls$DayXWorkday <- DailySACalls$DayID*DailySACalls$Workday
lm(Calls ~DayID+Workday+DayXWorkday, data=DailySACalls) %>% test_lm()

#we store model 8 which is the best model of above
#because it is parsimonious and clearly interpretable

Model8 <- lm(Calls ~DayID+WorkDay, data=DailySACalls)


# Save Residuals from best Model(s)------------

# CallsG6 remains because it was used extensively in coding
# actual dates, it is very close to Model 8
NumericYears_X_WorkDay    <- lm(Calls ~ year(Date)+ WorkDay, data=DailySACalls)
NumericYears_X_WorkDay %>% test_lm()
DailySACalls$Cooks        <- cooks.distance(NumericYears_X_WorkDay)
DailySACalls$GLMResid     <- residuals(NumericYears_X_WorkDay)
DailySACalls$GLMPredicted <- predict(NumericYears_X_WorkDay)

# Storing the best predictions
DailySACalls$Cooks8 <- cooks.distance(Model8)
DailySACalls$GLMResid8 <- residuals(Model8)
DailySACalls$GLMPredicted8 <- predict(Model8)
DailySACalls$StResM8      <- studres(Model8) #saves Studentized Residuals
DailySACalls$RAINNP       <- 100*(DailySACalls$Calls - DailySACalls$GLMPredicted8) / DailySACalls$GLMPredicted8 #saves percent increase over predicted for comparison with RAINN statistics

# export it 
# write.csv(DailySACalls,"DailySACallsOut.csv")

# Descriptives----------------------------------

mean(DailySACalls$Calls)
sd(DailySACalls$Calls)
summary(DailySACalls$Calls)
describeBy(DailySACalls$Calls, group=DailySACalls$Weekday)
describeBy(DailySACalls$Calls, group=DailySACalls$Month)
describeBy(DailySACalls$Calls, group=year(DailySACalls$Date))
# Plots of Call by Day---------------------------------

# Histogram of Calls
ggplot(data=DailySACalls, aes(Calls)) + 
      geom_histogram(breaks=seq(0,45, by=1), colour="black", fill="white") +
      labs(title = "Histogram of Calls", x="Calls per Day", y="Number of Days")



# "Scatter Plot" of all data with Model Predictions
ggplot(data=DailySACalls, aes(x=DayID,y=Calls))+
      geom_hex(bins=45)+
      scale_fill_gradient(low = "light gray", high = "black")+
      geom_point(aes(y=DailySACalls$GLMPredicted8))+
      geom_point(aes(y=DailySACalls$GLMPredicted8))+
      ggtitle("Sexual Assault Hotline Calls")
    

# Box Plot Call by Month
ggplot(data=DailySACalls, aes(x=Month,y=Calls))+
      geom_boxplot(outlier.shape = NA)+
      scale_y_continuous(limits = c(0,30))+
      ggtitle("Distribution of Monthly Calls")

# Bar Graph of Calls by Month
ggplot(data=DailySACalls, aes(x=Month,y=Calls))+
      geom_col()+
      ggtitle("Total Calls by Month")

# Box Plot of Calls by Year
ggplot(data=DailySACalls, aes(x=year(Date),group=year(Date),y=Calls))+
      geom_boxplot()+
      ggtitle("Distribution of Calls by Year")+
      labs(x='Year')

# Box Plot of Calls by Day of Week
ggplot(data=DailySACalls, aes(x=Weekday,y=Calls))+
      geom_boxplot(outlier.shape = NA)+
      scale_y_continuous(limits = c(0,30))+
      ggtitle("Calls by Day of Week")

# Box Plot of Calls by Holiday
 

# Box Plot of Type of Day
DailySACalls %>%
  mutate(TypeofDay = fct_reorder(TypeofDay, Calls)) %>%
  ggplot(
    aes(x=TypeofDay,y=Calls)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Holidays")+
  ggtitle("Comparison of Holidays and Weekdays")

# Box Plot of Residuals by Month
ggplot(data=DailySACalls, aes(x=Month,y=GLMResid8))+
      geom_boxplot()








# Analysis of Gender ------

# Chi Square Test whether Male victims more likely to call than be client
chisq.test(Male_Female_Callers)
cramer(Male_Female_Callers)
Male_Female_Callers
#While Men do seem to talk for shorter amount of time, the difference is trivial
#in the data for real amounts

filter(AllSACalls,(Sex=="Male"|Sex=="Female")&TotalTime<=3&ClientType=="Victim") %>%
ggplot(aes(x=Sex, y=TotalTime))+
geom_boxplot()

lm(TotalTime ~ Sex,data=filter(AllSACalls,(Sex=="Male"|Sex=="Female")&TotalTime<=1&ClientType=="Victim") ) %>% test_lm()
describeBy(AllSACalls$TotalTime, group=AllSACalls$Sex)
describeBy(AllSACalls$TotalTime, group=AllSACalls$Call_duration)




Filtered_call_times <- filter(AllSACalls,(Sex=="Male"|Sex=="Female")&TotalTime<=3&ClientType=="Victim")
Filtered_call_times$Sex<-factor(Filtered_call_times$Sex)
describeBy(Filtered_call_times$TotalTime,group = Filtered_call_times$Sex)
t-test(Filtered_call_times$TotalTime ~ Filtered_call_times$Sex)
cohen.d(Filtered_call_times$TotalTime ~ Filtered_call_times$Sex)
ks.test(ClientAgeXDate$Age, filter(AllSACalls,ClientType=='Victim') %>% .$Age )



# AllSACallsTEMP exists only for syntax reasons, i couldn't figure out how to
# user filter and table etc
AllSACallsTEMP <- filter(AllSACalls,(Sex=="Male"|Sex=="Female")&ClientType=="Victim")
AllSACallsTEMP$Sex <- factor(AllSACallsTEMP$Sex)
chisq.test(table(AllSACallsTEMP$Sex, AllSACallsTEMP$Call_duration))
cramer(table(AllSACallsTEMP$Sex, AllSACallsTEMP$Call_duration))

chisq.test(table(AllSACallsTEMP$Sex, AllSACallsTEMP$ReferralMade))
cramer(table(AllSACallsTEMP$Sex, AllSACallsTEMP$ReferralMade))


# Analysis of Centers -----

# Chi Square Test whether Centers have different weekly patterns,
# this should probably be redone with just weekday vs weekend
chisq.test(Calls_by_Center_by_Weekday)
cramer(Calls_by_Center_by_Weekday)

cramer(table(AllSACalls$CenterName,wday(AllSACalls$Date, label = TRUE)))
cramer(table(AllSACalls$CenterName, AllSACalls$WorkDay))

# Analysis of Age-----

# Victim Callers
filter(AllSACalls,Age>1 & ClientType=='Victim') %>%
ggplot(aes(Age)) + 
  geom_histogram(breaks=seq(1.5,80.5, by=1), colour="black", fill="white") +
  labs(title = "Age of Callers", x="Age in Years", y="Number of Callers")

# Clients
filter(ClientAgeXDate, Age>1)%>%
  ggplot(aes(Age)) + 
  geom_histogram(breaks=seq(1.5,80.5, by=1), colour="black", fill="white") +
  labs(title = "Age of Clients", x="Age in Years", y="Number of Clients")

# Combined Plot of Distribution
bind_rows(
  #Client Data
  filter(ClientAgeXDate,between(Age,1,100)) %>%
    add_column('Type') %>%
    mutate(Type='Client') %>%
    select(Age,Type),  
  #Caller Data
    filter(AllSACalls,between(Age,1,100) & ClientType=='Victim') %>%
  add_column('Type') %>%
  mutate(Type='Caller') %>%
  select(Age,Type)
) %>%
  ggplot(aes(x = Age, fill = Type)) + geom_density(alpha = 0.5) +
  labs(title = "Age Distribution of Clients and Callers", x="Age in Years", y="Density")

#KS non-parametric test of different distributions
ks.test(ClientAgeXDate$Age, filter(AllSACalls,ClientType=='Victim') %>% .$Age )
            
# describe age at assault           
filter(ClientAgeXDate,
       between(ClientAgeXDate$Age,2,100)
       &between(ClientAgeXDate$AgeatPrim,2,100)
       ) %>% mutate(Diff=.$Age-.$AgeatPrim) %>%
  describe()

# plot age at assault
filter(ClientAgeXDate,
       between(ClientAgeXDate$Age,2,100)
       &between(ClientAgeXDate$AgeatPrim,2,100)
   
) %>% mutate(Diff=.$Age-.$AgeatPrim) %>%
  ggplot(aes(x = AgeatPrim)) + geom_density(alpha = 0.5) +
  labs(title = "Distribution of Estimated Age at Assault", x="Age in Years", y="Density")+
  scale_x_continuous(breaks=c(0,1,5,10,15,18,22,30,40,50,60,70,80,90,100))
            
#histogram
filter(ClientAgeXDate,
       between(ClientAgeXDate$Age,2,100)
       &between(ClientAgeXDate$AgeatPrim,2,100)
       
) %>% mutate(Diff=.$Age-.$AgeatPrim) %>%
  ggplot(aes(x = AgeatPrim)) +
geom_histogram(breaks=seq(2,80, by=1), colour="black", fill="white") +
  labs(title = "Estimated Age at Assault", x="Age in Years", y="Clients")+
  scale_x_continuous(breaks=c(2,5,10,12,14,16,18,20,22,25,30,35,40,50,60,70,80))  
            
            

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
