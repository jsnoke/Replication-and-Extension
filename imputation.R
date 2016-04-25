library(statnet)

wave.1.network <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/wave 1 network.dat", 
                             quote="\"", comment.char="")

wave.2.network <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/wave 2 network.dat", 
                             quote="\"", comment.char="")

trust <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/Trust.dat", 
                    quote="\"", comment.char="")

proDev <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/Prodev-proenv1-7.dat", 
                     quote="\"", comment.char="")

gov <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/gov-nongov.dat", 
                  quote="\"", comment.char="")


#####
## mple imputation
#####

### predict original data - not what we want
yass = glm(testDF1$tie ~ ., data = testDF1, family = binomial)
summary(yass)
tieProbsOrig = predict(yass, type = "response", newdata = testDF1)

newTieProbsOrig = vector("list", 10)
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]^2
    
    newTieProbsOrig[[a]] = matrix(tieProbsOrig[first:second], 
                              nrow = groupings[a], ncol = groupings[a])
    
    first = first + groupings[a]^2
}

## prediction
evalSeq = seq(0, 1, 0.05)
predictionOut = data.frame(matrix(NA, nrow = length(evalSeq), ncol = 4))
colnames(predictionOut) = c("0-0", "0-1", "1-0", "1-1")
for(b in 1:length(evalSeq)){
    predicted.Wave.1 = ergmNet
    diag(predicted.Wave.1) = 0
    first = 1
    second = 0
    tempOut = rep(0, 4)
    for(a in 1:10){
        second = second + groupings[a]
        
        tempMat = predicted.Wave.1[first:second, first:second]
        tempVec = rep(0, ncol(tempMat)^2)
        
        tempVec[which(newTieProbsOrig[[a]] > evalSeq[b])] = 1
        tempVec[which(newTieProbsOrig[[a]] <= evalSeq[b])] = 0
        
        predicted.Wave.1[first:second, first:second] = tempVec
        
        tempOut = tempOut + c(sum(predicted.Wave.1[first:second,first:second] == 0 & 
                                      ergmNet[first:second,first:second] == 0),
                              sum(predicted.Wave.1[first:second,first:second] == 1 & 
                                      ergmNet[first:second,first:second] == 0),
                              sum(predicted.Wave.1[first:second,first:second] == 0 & 
                                      ergmNet[first:second,first:second] == 1),
                              sum(predicted.Wave.1[first:second,first:second] == 1 & 
                                      ergmNet[first:second,first:second] == 1))
        
        first = first + groupings[a]
    }
    predictionOut[b, ] = tempOut
}

## correct classification
corClass = (predictionOut[, 1] + predictionOut[, 4]) / 3894

## recall - sensitivity
recall = (predictionOut[, 4]) / (predictionOut[, 3] + predictionOut[, 4])

## specificity
specif = (predictionOut[, 1]) / (predictionOut[, 1] + predictionOut[, 2])

plot(1-specif, recall, type = "b")
abline(0, 1)
points(1-specif, corClass)


## predict new data
yass2 = glm(tie ~ in.star + out.star + twopath + triple + reciprocity + 
                in.gov + 
                out.gov + 
                in.prodev +
                out.prodev + 
                in.trust + 
                out.trust + 
                match.gov + 
                diff.prodev + 
                diff.trust + 
                testDF1$tie, data = testDF2, family = binomial)
summary(yass2)
tieProbsWave2 = predict(yass2, type = "response", newdata = testDF2)


newTieProbsWave2 = vector("list", 10)
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]^2
    
    newTieProbsWave2[[a]] = matrix(tieProbsWave2[first:second], 
                              nrow = groupings[a], ncol = groupings[a])
    
    first = first + groupings[a]^2
}


## prediction
evalSeq = seq(0, 1, 0.05)
predictionOut = data.frame(matrix(NA, nrow = length(evalSeq), ncol = 4))
colnames(predictionOut) = c("0-0", "0-1", "1-0", "1-1")
for(b in 1:length(evalSeq)){
    predicted.Wave.2 = ergmNet2
    diag(predicted.Wave.2) = 0
    first = 1
    second = 0
    tempOut = rep(0, 4)
    for(a in 1:10){
        second = second + groupings[a]
        
        tempMat = predicted.Wave.2[first:second, first:second]
        tempVec = rep(NA, ncol(tempMat)^2)
        
        tempVec[which(newTieProbsWave2[[a]] > evalSeq[b])] = 1
        tempVec[which(newTieProbsWave2[[a]] <= evalSeq[b])] = 0
        
        predicted.Wave.2[first:second, first:second] = tempVec
        diag(predicted.Wave.2[first:second, first:second]) = 0
        
        tempOut = tempOut + c(sum(predicted.Wave.2[first:second,first:second] == 0 & 
                                      ergmNet2[first:second,first:second] == 0, na.rm = T),
                              sum(predicted.Wave.2[first:second,first:second] == 1 & 
                                      ergmNet2[first:second,first:second] == 0, na.rm = T),
                              sum(predicted.Wave.2[first:second,first:second] == 0 & 
                                      ergmNet2[first:second,first:second] == 1, na.rm = T),
                              sum(predicted.Wave.2[first:second,first:second] == 1 & 
                                      ergmNet2[first:second,first:second] == 1, na.rm = T))
        
        first = first + groupings[a]
    }
    predictionOut[b, ] = tempOut
}

## correct classification
corClass = (predictionOut[, 1] + predictionOut[, 4]) / 2421

## recall - sensitivity
recall = (predictionOut[, 4]) / (predictionOut[, 3] + predictionOut[, 4])

## specificity
specif = (predictionOut[, 1]) / (predictionOut[, 1] + predictionOut[, 2])

plot(1-specif, recall, type = "b")
abline(0, 1)
points(1-specif, corClass)


#####
## imputed network
#####

tieProbsWavePred = predict(yass2, type = "response", 
                           newdata = data.frame(cbind(predDF, testDF1$tie)))

newTieProbsWavePred = vector("list", 10)
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]^2
    
    newTieProbsWavePred[[a]] = matrix(tieProbsWavePred[first:second], 
                                   nrow = groupings[a], ncol = groupings[a])
    
    first = first + groupings[a]^2
}

## prediction
imputed.Wave.2 = ergmNet2
diag(imputed.Wave.2) = 0
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]
    
    tempMat = imputed.Wave.2[first:second, first:second]
    tempVec = rep(NA, length(tempMat[is.na(tempMat)]))
    
    tempVec[which(newTieProbsWavePred[[a]][which(is.na(tempMat))] > 0.3)] = 1
    tempVec[which(newTieProbsWavePred[[a]][which(is.na(tempMat))] <= 0.3)] = 0
    
    tempMat[is.na(tempMat)] = tempVec
    diag(tempMat) = 0
    
    imputed.Wave.2[first:second, first:second] = tempMat

    first = first + groupings[a]
}

imputedNetwork = network(imputed.Wave.2, directed = T)
imputedNetwork

dyad.census(imputedNetwork)
summary(degree(imputedNetwork, cmode = "indegree"))
summary(degree(fooNet, cmode = "indegree"))








