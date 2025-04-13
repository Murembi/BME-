fold <- "C:/Users/murem/OneDrive/Desktop/BME/Datasets"
setwd(fold)
getwd()
Eyecolour <- read.table("EyeColour.txt", header=T)
head(Eyecolour)
summary(Eyecolour)
is.factor(Eyecolour$Gender)
Eyecolour$Gender <- as.factor(Eyecolour$Gender)
is.factor(Eyecolour$EyeColour)
Eyecolour$EyeColour <- as.factor(Eyecolour$EyeColour)
summary(Eyecolour)
head(Eyecolour)
table(Eyecolour)
test.result <- chisq.test(table(Eyecolour))
test.result
chisq.test(Eyecolour$Gender, Eyecolour$EyeColour)
test.result$expected
test.result$residuals 
mosaicplot(table(Eyecolour))
