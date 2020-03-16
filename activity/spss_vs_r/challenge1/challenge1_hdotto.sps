*Begin by separating the arrests from the convictions while maintaining the Identifier and Date order.
SORT CASES BY EventNumerical (D) Identifier (A) Date (A).
EXECUTE.

*Select out the arrests into a separate dataset.
SELECT IF (EventNumerical=1). 
EXECUTE.
DATASET ACTIVATE DataSet2. 
DATASET ACTIVATE Arrests.

*Calculate cumulative arrest counts in the separate dataset.
IF ($casenum=1) OR lag(Identifier) NE Identifier PriorArrests=1. 
EXECUTE.

IF (Identifier = lag(Identifier)) PriorArrests = sum(lag(PriorArrests),1). 
EXECUTE.

*Merge the new prior arrest cumulative count variable into the dataset with the convictions.
MATCH FILES /FILE=*
  /FILE='Arrests'
  /RENAME (Date Event EventNumerical Identifier = d0 d1 d2 d3)
  /DROP= d0 d1 d2 d3.
EXECUTE.

*Return the cases to their original order with convictions. 
SORT CASES BY Identifier (A) Date (A).
EXECUTE.

*
IF (Identifier=lag(Identifier) AND EventNumerical NE 1) PriorArrests=lag(PriorArrests).
EXECUTE.

*When the first case for an ID is a conviction, there is a missing value so this replaces the missing values
with a 0. This is probably not very important in real life where arrests usually come before convictions. 
RECODE PriorArrests (SYSMIS)=0.
EXECUTE. 
