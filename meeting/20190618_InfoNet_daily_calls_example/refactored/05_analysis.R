
# gender ----

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


# centers ----

# Chi Square Test whether Centers have different weekly patterns,
# this should probably be redone with just weekday vs weekend
chisq.test(Calls_by_Center_by_Weekday)
cramer(Calls_by_Center_by_Weekday)

cramer(table(AllSACalls$CenterName,wday(AllSACalls$Date, label = TRUE)))
cramer(table(AllSACalls$CenterName, AllSACalls$WorkDay))


# Analysis of Age ----

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
