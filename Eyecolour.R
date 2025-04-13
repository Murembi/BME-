fold <- "C:/Users/murem/OneDrive/Desktop/BME/Datasets"
setwd(fold)
getwd()
Eyecolour <- read.table ("EyeColour.txt", header=T)
summary(Eyecolour)
is.factor(Eyecolour$Gender)
Eyecolour$Gender <- as.factor(Eyecolour$Gender)
is.factor(Eyecolour$EyeColour)
Eyecolour$EyeColour <- as.factor(Eyecolour$EyeColour)
summary(Eyecolour)
table(Eyecolour)#convert the data into a contigency table
test.result <- chisq.test(table(Eyecolour))
test.result$expected
test.result$residuals
mosaicplot(table(Eyecolour))
#Alternative 
chisq.test(Eyecolour$Gender, Eyecolour$EyeColour)
