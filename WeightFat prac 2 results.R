fold<- 'C:/Users/murem/OneDrive/Desktop/BME/Datasets/Datasets'
setwd(fold)
getwd()
#import the data set into R and change the name of the file into Fats----
#header=TRUE specifies that the first row of the
Fats <- read.table('WeightFat.txt', header=TRUE)
# check if the data is numeric or categorical and based on the data weight and fat are continous meaning are numeric and not factors
is.numeric(Fats$WEIGHT)
is.factor(Fats$WEIGHT)
is.numeric(Fats$FAT)
is.factor(Fats$FAT)
# x = predictor= fat, Y= response= weight
Regr <- lm(FAT ~ WEIGHT, data= Fats)
summary(Regr)
#the summary ()fucntions is used to obtaine results of the regression analysis----
#The equation is y= 5.757 + 0.323x
plot(FAT~WEIGHT,data=Fats)
#Plot the best firt line
abline(Regr)
abline(a= 26.88558, b= 0.02069, col='blue')

#6  yes, the line is significantly different from zero
#7 the slope of zero would mean that we accept the null hypothesis and there's no relation between the weight and fats
#8
#9. R= 0.006081, which is explains the strength between the variables under invrstigation.
