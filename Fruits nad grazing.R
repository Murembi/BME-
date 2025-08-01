fold <-'C:/Users/murem/OneDrive/Desktop/BME/Datasets'
setwd(fold)
getwd()
regrowth <- read.table('ipomopsis.txt', header=TRUE)
head(regrowth)
is.numeric(regrowth$Root)
is.numeric(regrowth$Fruit)
is.numeric(regrowth$Grazing)
is.factor(regrowth$Grazing)
regrowth$Grazing <- as.factor(regrowth$Grazing)
is.factor(regrowth$Grazing)
summary(regrowth)
boxplot(regrowth)
boxplot(Fruit~ Grazing, data=regrowth)
model1 <- lm(Fruit~ Grazing, data=regrowth)
anova(model1)
#based on the plot, grazed has a high seed production whereas the ungrazed has low fruit production
summary(model1)
model2 <- lm(Fruit ~ Grazing + Root, data= regrowth)
anova(model2)
crPlots(model2)
