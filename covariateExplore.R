group = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
          rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))

trust$V2[trust$V2 == 11] = NA
proDev$V1[proDev$V1 == 11] = NA
proDev$V1[proDev$V1 == 8] = NA

covData = data.frame(trust$V1, trust$V2, gov$V1, proDev$V1, group)
View(covData)

boxplot(covData$trust.V1 ~ covData$group)
boxplot(covData$trust.V2 ~ covData$group)
plot(covData$trust.V1, covData$trust.V2)
var(covData$trust.V1, covData$trust.V2, na.rm = T)

var(scale(covData), na.rm = T)

boxplot(covData$proDev.V1 ~ covData$group)
boxplot(covData$gov.V1 ~ covData$group)

#missingG = c(rep(1, 9), rep(2, 2), rep(3, 4), rep(4, 13), rep(5, 6),
#                rep(6, 8), rep(7, 7), rep(8, 10), rep(9, 6), rep(10, 4))

missingG = rep(0, 194)
missingG[which(is.na(trust$V2))] = 1

cor(covData, missingG)
boxplot(missingG ~ covData$group)

c(9/19, 2/13, 4/24, 13/21, 6/16, 8/21, 7/25, 10/20, 6/20, 4/15)
boxplot(covData$trust.V1 ~ missingG)




