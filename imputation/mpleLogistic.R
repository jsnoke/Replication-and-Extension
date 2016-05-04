#####
## MI proper - logistic regression
#####

proDev$V1[proDev$V1 == 8] = NA
proDev$V1[proDev$V1 == 11] = NA
trust$V2[trust$V2 == 11] = NA

logMIWave2 = vector("list", 10)
logMICov = vector("list", 10)

set.seed(1959)

for(c in 1:10){

#####
## covariate multiple impute - normal regression
#####
covDF = cbind(trust, proDev, gov)
colnames(covDF) = c("trust1", "trust2", "prodev", "gov")

## trust wave 2
x = as.matrix(cbind(rep(1, length(covDF$trust1[!is.na(covDF$trust2), drop = F])), 
                    covDF$trust1[!is.na(covDF$trust2), drop = F]))
y = as.matrix(covDF$trust2[!is.na(covDF$trust2), drop = F])
xp = as.matrix(cbind(rep(1, length(covDF$trust1[is.na(covDF$trust2), drop = F])), 
                     covDF$trust1[is.na(covDF$trust2), drop = F]))
xtx = t(x) %*% x
pen = 0.00001 * diag(xtx)
if (length(pen)==1) pen = matrix(pen)
v = solve(xtx + diag(pen))
coef = t(y) %*% x %*% v
residuals   = y - x %*% t(coef)
sigma.star  = sqrt(sum((residuals)^2)/rchisq(1, length(y) - ncol(x)))
beta.star   = coef + t((chol((v + t(v))/2) %*% rnorm(ncol(x))) * sigma.star)
parm = list(coef, beta.star, sigma.star)
names(parm) = c("coef","beta","sigma") 
res = xp %*% t(parm$beta) + rnorm(nrow(xp)) * parm$sigma
res = round(res, 1)
res[res > 10] = 10
res[res < 0] = 0
covDF$trust2[is.na(covDF$trust2)] = res

## prodev
x = as.matrix(cbind(rep(1, nrow(covDF[!is.na(covDF$prodev), 1:2, drop = F])), 
                    covDF[!is.na(covDF$prodev), 1:2, drop = F]))
y = as.matrix(covDF$prodev[!is.na(covDF$prodev), drop = F])
xp = as.matrix(cbind(rep(1, nrow(covDF[is.na(covDF$prodev), 1:2, drop = F])),
                     covDF[is.na(covDF$prodev), 1:2]))
xtx = t(x) %*% x
pen = 0.00001 * diag(xtx)
if (length(pen)==1) pen = matrix(pen)
v = solve(xtx + diag(pen))
coef = t(y) %*% x %*% v
residuals   = y - x %*% t(coef)
sigma.star  = sqrt(sum((residuals)^2)/rchisq(1, length(y) - ncol(x)))
beta.star   = coef + t((chol((v + t(v))/2)) %*% rnorm(ncol(x)) * sigma.star)
parm = list(coef, beta.star, sigma.star)
names(parm) = c("coef","beta","sigma") 
res = xp %*% t(parm$beta) + rnorm(nrow(xp)) * parm$sigma
res = round(res, 1)
res[res > 7] = 7
res[res < 1] = 1
covDF$prodev[is.na(covDF$prodev)] = res


summary(covDF)

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
### estimate and predict
#####

newTieModel = glm(tie ~ in.star + out.star + twopath + triple + reciprocity + 
                #in.gov + 
                #out.gov + 
                #in.prodev +
                #out.prodev + 
                #in.trust + 
                #out.trust + 
                #match.gov + 
                #diff.prodev + 
                #diff.trust + 
                waveDF1$tie, data = waveDF2, family = binomial)


fit.sum = summary(newTieModel)
beta = coef(newTieModel)
rv   = t(chol(fit.sum$cov.unscaled))

betaNew = beta + rv %*% rnorm(ncol(rv))

#p = 1 / (1 + exp(-(cbind(rep(1, nrow(predDF)), predDF[ , -1], waveDF1$tie) %*% betaNew)))  
p = 1 / (1 + exp(-(cbind(rep(1, nrow(predDF)), predDF[ , c(2:6)], waveDF1$tie) %*% betaNew)))
newDraw = (runif(nrow(p))<= p)
newDraw = as.numeric(newDraw) 
summary(as.factor(newDraw))
newWaveDat = vector("list", 10)
first = 1
second = 0
for(a in 1:10){
    second = second + groupings[a]^2
    
    newWaveDat[[a]] = newDraw[first:second]
    
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
    
    tempVec[which(is.na(tempMat))] = newWaveDat[[a]][which(is.na(tempMat))]
    
    tempMat[is.na(tempMat)] = tempVec[!is.na(tempVec)]
    diag(tempMat) = 0
    
    imputed.Wave.2[first:second, first:second] = tempMat
    
    first = first + groupings[a]
}

#####
## store
#####
logMIWave2[[c]] = imputed.Wave.2 
logMICov[[c]] = covDF

}

save(logMIWave2, file = "imputation/logMIWaveShort.RData")
save(logMICov, file = "imputation/logMICov.RData")

summary(logMICov[[2]])
imputedNetwork = network(logMIWave2[[2]], directed = T)
imputedNetwork
plot(imputedNetwork)

dyad.census(imputedNetwork)
summary(degree(imputedNetwork, cmode = "outdegree"))
summary(degree(fooNet, cmode = "indegree"))

