install.packages("synthpop")

library(synthpop)
library(MatchIt)

##synpop to create a made-up dataset. 
dd_syn<- syn(DD, visit.sequence=c(2,1,3,4,5,6,7,8,9,10,11,12))
D<-DDsyn


##### PSM

##unadjusted regression
M_unadjusted <- glm(RecidivismEventYN	~ Age	+ AgeFirstUse.1	+ GenderNew	+ OUDDiagnosis	+ OtherDiagnosis	+ 
                      PriorArrests	+ RaceBlack	+ RaceOther	+ PriorTXEpisodes.1 + Group , data = D, family = binomial())

summary(M_unadjusted)
exp(M_unadjusted$coefficients)
##Plain old logistic regression to estimate propensity scores
##use interactions if you have a lot of potential matches or 
##if balance problems appear.
##matchit doesn't need you to run this regression beforehand

psLR	<- glm(Group	~ Age	+ AgeFirstUse.1	+ GenderNew	+ OUDDiagnosis	+ OtherDiagnosis	+ 
              PriorArrests	+ RaceBlack	+ RaceOther	+ PriorTXEpisodes.1,
            data	=D,	family	= binomial())

summary(psLR)

##attach the propensity score to the data
D$prop_score <- predict(psLR,type="response")

##see distribution of scores by treamtment
##histbackback is from package Hmisc
histbackback(split(D$prop_score,D$Group))


##nearest neighbors match, most commonly used
PS_NN <- matchit(Group~Age	+ AgeFirstUse.1	+ GenderNew	+ OUDDiagnosis	+ OtherDiagnosis	+ 
                   PriorArrests	+ RaceBlack	+ RaceOther	+ PriorTXEpisodes.1,
                 data=D, 
                 method="nearest", 
                 ratio = 1, 
                 m.order="random",
                 discard="control"
                 ##reestimate=TRUE
)

summary(PS_NN, interactions=TRUE) ##if you want to see interactions
summary(PS_NN, interactions=TRUE,standardize = TRUE) 


##plot of distribution of propensity scores
plot(PS_NN, type="jitter")



##data frame of matched records
matched_D <- match.data(PS_NN)

##unless you reestimate the propensity model, 
##distance and the prop_score should be identical

histbackback(split(matched_D$distance,matched_D$Group))

##adjusted regression, treatment effect is diminished considerably
M_adjusted <- glm(RecidivismEventYN	~ Age	+ AgeFirstUse.1	+ GenderNew	+ OUDDiagnosis	+ OtherDiagnosis	+ 
                    PriorArrests	+ RaceBlack	+ RaceOther	+ PriorTXEpisodes.1 + Group, data =matched_D, family = binomial())
