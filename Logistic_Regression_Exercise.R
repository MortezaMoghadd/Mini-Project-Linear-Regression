##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
str(NH11) # check stucture of NH11
str(NH11$everwrk)# check stucture of everwrk
str(NH11$r_maritl)
summary(NH11$age_p)
levels(NH11$r_maritl)
library(dplyr)
NH11 <- NH11 %>% filter(r_maritl != "9 Unknown marital status") #remove the unknow marital status
levels(NH11$everwrk) # check levels 
# collapse all missing values to NA - include only "yes" "no" answers
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))
# run our logistic regression model
Model1 <- glm(everwrk ~ age_p + r_maritl,
               data=NH11, family="binomial")
summary(Model1)
#Just to get the coefficients
coef(summary(Model1))
plot(allEffects(Model1))

##   2. Predict the probability of working for each level of marital
##      status.
Model1.tab<- coef(summary(Model1)) 
Model1.tab[, "Estimate"] <- exp(coef(Model1)) #convert the coefficient(estimate column)to exp so u can read the column
Model1.tab #read the new converted coeffecient
