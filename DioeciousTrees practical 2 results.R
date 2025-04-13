fold <- 'C:/Users/murem/OneDrive/Desktop/BME/Datasets/Datasets'
setwd(fold)
getwd()
# give your file a name, it can be any name
#Read the table and check if the data provided is categorical or numerical
Trees <- read.table('DioeciousTrees.txt', header=TRUE)
is.numeric(Trees$FLOWERS)
is.factor(Trees$FLOWERS)
is.numeric(Trees$DBH)
is.factor(Trees$DBH)
is.numeric(Trees$SEX)
is.factor(Trees$SEX)
#x=DBH= predictor, Y=flowers= response
lm(Trees$DBH ~ Trees$FLOWERS)
Regr<- lm(DBH ~ FLOWERS, data= Trees)
#Summary() for the regression analysis
summary(Regr)
#the mathematical equations m= 0.1560, c= 131.7790, the straight line eqn is y=0.1560x +131.7790
plot(Trees$DBH, Trees$FLOWERS)
plot(DBH~FLOWERS, data=Trees)
#Answer the question if it's appropriate for fit the best fit line
abline(Regr)
#finish up 