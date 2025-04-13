fold <- "C:/Users/murem/OneDrive/Desktop/BME/Datasets"
setwd(fold)
getwd()
dispersal <- read.table("Dispersal.txt", header=T)
head(dispersal)
is.factor(dispersal$STRATEGY)
dispersal$STRATEGY <- as.factor(dispersal$STRATEGY)
is.factor(dispersal$PLSIZE)
dispersal$PLSIZE <- as.factor(dispersal$PLSIZE)
is.numeric(dispersal$NOSP)
contable <- xtabs(NOSP ~ STRATEGY + PLSIZE, data=dispersal)
contable
str(contable)
(test.result <- chisq.test(contable))
test.result$residuals
mosaicplot(contable)
mosaicplot(contable, shade=T, main= "disperesal strategy vs propagule size")
