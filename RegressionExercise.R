## Exercise: least squares regression

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   Start by examining the data to check for problems.
str(sts.ex.sat)
# summary of energy and metro columns, all rows
sts.metro.energy <- subset(states.data, select = c("metro", "energy"))
sts.metro.energy <- na.omit(sts.metro.energy)
summary(sts.metro.energy)
# convert the metro from 
# Per capita consumption is calculated by dividing the consumption total by the population of the group 
# person = metro*pop
# the comsumption per person = pop*energy
cor(sts.metro.energy$metro, sts.metro.energy$energy)
library(ggplot2)
ggplot(sts.metro.energy, aes(x = metro, y = energy)) +
  geom_point()+
  geom_smooth()
##   2. Print and interpret the model `summary'
model1 = lm(energy~metro, data=sts.metro.energy)
summary(model1)

##   3. `plot' the model to look for deviations from modeling assumptions
hist(residuals(model1))
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) 
plot(model1, which = c(1, 2))
##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
sts.metro.energy.gr <- subset(states.data, select = c("metro", "energy", "green"))
sts.metro.energy.gr <- na.omit(sts.metro.energy.gr)
summary(sts.metro.energy.gr)
cor(sts.metro.energy.gr[,c(1:3)])

library(ggplot2)
ggplot(sts.metro.energy.gr, aes(x = metro, y = energy, size= green)) +
  geom_point()+
  geom_smooth()
##   2. Print and interpret the model `summary'
model2 = lm(energy~metro+green, data=sts.metro.energy.gr)
summary(model2)
hist(residuals(model2))
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) 
plot(model2, which = c(1, 2))
# model 2 is fitted better, has better R-squared and better significance level








##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
sts.metro.energy.gr <- subset(states.data, select = c("metro", "energy", "green", "income", "+ region"))
sts.metro.energy.gr <- na.omit(sts.metro.energy.gr)
summary(sts.metro.energy.gr)
cor(sts.metro.energy.gr[,c(1:3)])
model2 = lm(energy ~ metro * green, data=sts.metro.energy.gr)
summary(model2)
coef(summary(model2))
##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
sts.metro.energy.gr <- subset(states.data, select = c("metro", "energy", "green", "income", "region"))
sts.metro.energy.gr <- na.omit(sts.metro.energy.gr)
model2 = lm(energy ~ metro * green + region, data=sts.metro.energy.gr)
summary(model2)
anova(model2)
# There is a significant difference between regions!
