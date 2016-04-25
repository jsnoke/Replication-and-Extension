#####
## CART
#####

#####
## setup dataframes
#####
origNet1 = wave.1.network
origNet1[origNet1 == 10] = 0

origNet2 = wave.2.network
origNet2[origNet2 == 9] = NA
origNet2[origNet2 == 10] = 0

predNet = wave.2.network
predNet[predNet == 9] = 0
predNet[predNet == 10] = 0

sepNets1 = vector("list", 10)
sepNets2 = vector("list", 10)
predNets = vector("list", 10)
mpleOuts1 = vector("list", 10)
mpleOuts2 = vector("list", 10)
predMPLE = vector("list", 10)
groupings = c(19, 13, 24, 21, 16, 21, 25, 20, 20, 15)
first = 1
second = 0
waveDF1 = NULL
waveDF2 = NULL
predDF = NULL

for(a in 1:10){
    second = second + groupings[a]
    
    sepNets1[[a]] = network(origNet1[first:second, first:second], directed = T)
    sepNets1[[a]] %v% "trust" = covDF$trust1[first:second]
    sepNets1[[a]] %v% "prodev" = covDF$prodev[first:second]
    sepNets1[[a]] %v% "gov" = covDF$gov[first:second]
    
    mpleOuts1[[a]] = ergmMPLE(sepNets1[[a]] ~ istar(2) + ostar(2) + twopath + ttriple + mutual + 
                                  nodeifactor("gov") + nodeofactor("gov") + nodeicov("prodev") + 
                                  nodeocov("prodev") + nodeicov("trust") + nodeocov("trust") + 
                                  nodematch("gov") + absdiff("prodev") + absdiff("trust"), 
                              output = "array")
    
    waveDF1 = rbind(waveDF1, cbind(c(mpleOuts1[[a]]$response), 
                                   matrix(c(mpleOuts1[[a]]$predictor[, , 1:14]), ncol = 14)))
    
    sepNets2[[a]] = network(origNet2[first:second, first:second], directed = T)
    sepNets2[[a]] %v% "trust" = covDF$trust2[first:second]
    sepNets2[[a]] %v% "prodev" = covDF$prodev[first:second]
    sepNets2[[a]] %v% "gov" = covDF$gov[first:second]
    
    mpleOuts2[[a]] = ergmMPLE(sepNets2[[a]] ~ istar(2) + ostar(2) + twopath + ttriple + mutual + 
                                  nodeifactor("gov") + nodeofactor("gov") + nodeicov("prodev") + 
                                  nodeocov("prodev") + nodeicov("trust") + nodeocov("trust") + 
                                  nodematch("gov") + absdiff("prodev") + absdiff("trust"), 
                              output = "array")
    
    waveDF2 = rbind(waveDF2, cbind(c(mpleOuts2[[a]]$response), 
                                   matrix(c(mpleOuts2[[a]]$predictor[, , 1:14]), ncol = 14)))
    
    predNets[[a]] = network(predNet[first:second, first:second], directed = T)
    predNets[[a]] %v% "trust" = covDF$trust2[first:second]
    predNets[[a]] %v% "prodev" = covDF$prodev[first:second]
    predNets[[a]] %v% "gov" = covDF$gov[first:second]
    
    predMPLE[[a]] = ergmMPLE(predNets[[a]] ~ istar(2) + ostar(2) + twopath + ttriple + mutual + 
                                 nodeifactor("gov") + nodeofactor("gov") + nodeicov("prodev") + 
                                 nodeocov("prodev") + nodeicov("trust") + nodeocov("trust") + 
                                 nodematch("gov") + absdiff("prodev") + absdiff("trust"), 
                             output = "array")
    
    predDF = rbind(predDF, cbind(c(predMPLE[[a]]$response), 
                                 matrix(c(predMPLE[[a]]$predictor[, , 1:14]), ncol = 14)))
    
    first = first + groupings[a]
}


colnames(waveDF1) = colnames(waveDF2) = colnames(predDF) = c("tie", "in-star", "out-star", 
                                                             "twopath", 
                                                             "triple","reciprocity", "in-gov", 
                                                             "out-gov", "in-prodev","out-prodev", 
                                                             "in-trust", "out-trust", "match-gov",
                                                             "diff-prodev", "diff-trust")
waveDF1 = data.frame(waveDF1)
waveDF2 = data.frame(waveDF2)

#####
## estimate and predict
#####
s = sample(nrow(predDF), replace = TRUE)
estRpart = rpart(tie ~ in.star + out.star + twopath + triple + reciprocity + 
                      in.gov + 
                      out.gov + 
                      in.prodev +
                      out.prodev + 
                      in.trust + 
                      out.trust + 
                      match.gov + 
                      diff.prodev + 
                      diff.trust + 
                      waveDF1$tie, data = waveDF2, method = "class", cp = 1e-04, minbucket = 5)
nodes = predict(object = estRpart, 
                newdata = data.frame(cbind(rep(1, nrow(predDF)), predDF[,-1], waveDF1$tie)))
#library(gdata)
new = apply(nodes, MARGIN=1, FUN=function(s) resample(colnames(nodes),size=1,
                                                      prob=s))
summary(new)
#new = factor(new,levels=levels(y))  

newDat = vector("list", 10)
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]^2
    
    newDat[[a]] = new[first:second]
    
    first = first + groupings[a]^2
}


imputed.Wave.2 = origNet2
diag(imputed.Wave.2) = 0
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]
    
    tempMat = imputed.Wave.2[first:second, first:second]
    tempVec = rep(NA, length(tempMat[is.na(tempMat)]))
    
    tempVec[which(is.na(tempMat))] = newDat[[a]][which(is.na(tempMat))]
    
    tempMat[is.na(tempMat)] = tempVec[!is.na(tempVec)]
    diag(tempMat) = 0
    
    imputed.Wave.2[first:second, first:second] = tempMat
    
    first = first + groupings[a]
}

imputedNetwork = network(imputed.Wave.2, directed = T)
imputedNetwork

dyad.census(imputedNetwork)
summary(degree(imputedNetwork, cmode = "outdegree"))





