library(RSiena)

#####
## SAO - ignore
#####

##example...
#myalgorithm <- sienaAlgorithmCreate(nsub=2, n3=100)
# nsub=2 and n3=100 is used here for having a brief computation, not for practice.
#mynet1 <- sienaDependent(array(c(tmp3, tmp4), dim=c(32, 32, 2)))
#mydata <- sienaDataCreate(mynet1)
#myeff <- getEffects(mydata)
#ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)


#mynet <- sienaDependent(array(c(s501, s502, s503), dim=c(50, 50, 3)))
#mybeh <- sienaDependent(s50a, type='behavior')
#mydata <- sienaDataCreate(mynet, mybeh)


#####
## baseline modal imputation
#####
## test
wave.2.network[wave.2.network == 9] = NA
trust[trust$V2 == 11, 2] = NA
proDev[proDev == 11] = NA
proDev[proDev == 8] = NA

testNet = sienaDependent(array(c(as.matrix(wave.1.network), as.matrix(wave.2.network)), 
                               dim=c(194, 194, 2)))
trustAtt =  sienaDependent(as.matrix(trust), type = "behavior")
#trustCo = coCovar(as.matrix(trust)[, 1])
prodevAtt = coCovar(as.matrix(proDev)[, 1])
govAtt = coCovar(as.matrix(gov)[, 1])
#trustCo = coCovar(as.matrix(trust)[, 2])

fullNet = sienaDataCreate(testNet, trustAtt, prodevAtt, govAtt)
myEff = getEffects(fullNet)
myEff = includeEffects(myEff, transTrip, inPop)
myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "prodevAtt")
myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "govAtt")
myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "trustAtt")
myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "govAtt")
myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "prodevAtt")
#myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "trustCo")
myEff = includeEffects(myEff, name = "trustAtt", avSim, interaction1 = "testNet")

myEff
#which(myEff$include == TRUE)
#myEff$include[311] = FALSE
#myEff

set.seed(86139)
## simulations
testAlgorithm = sienaAlgorithmCreate( projname = 'fullSAO')
ans = siena07(testAlgorithm, data = fullNet, effects = myEff)
ans

save(ans, file = "ergms/modalSAO.RData")


#####
## logistic multiple imputation
#####
## test
#wave.2.network[wave.2.network == 9] = NA
#trust[trust$V2 == 11, 2] = NA
#proDev[proDev == 11] = NA
#proDev[proDev == 8] = NA

load("imputation/logMIWaveShort.RData")
load("imputation/logMICov.RData")
logMIAns = vector("list", 10)
set.seed(86139)

for(a in 1:10){
    wave.2.network = logMIWave2[[a]]
    wave.2.network[wave.1.network == 10] = 10
    
    testNet = sienaDependent(array(c(as.matrix(wave.1.network), as.matrix(wave.2.network)), 
                                   dim=c(194, 194, 2)))
    trustAtt =  sienaDependent(as.matrix(logMICov[[a]][ , 1:2]), type = "behavior")
    #trustCo = coCovar(as.matrix(trust)[, 1])
    prodevAtt = coCovar(logMICov[[a]][ , 3])
    govAtt = coCovar(logMICov[[a]][ , 4])
    #trustCo = coCovar(as.matrix(trust)[, 2])
    
    fullNet = sienaDataCreate(testNet, trustAtt, prodevAtt, govAtt)
    myEff = getEffects(fullNet)
    myEff = includeEffects(myEff, transTrip, inPop)
    myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "prodevAtt")
    myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "govAtt")
    myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "trustAtt")
    myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "govAtt")
    myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "prodevAtt")
    #myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "trustCo")
    myEff = includeEffects(myEff, name = "trustAtt", avSim, interaction1 = "testNet")
    
    myEff
    #which(myEff$include == TRUE)
    #myEff$include[311] = FALSE
    #myEff
    
    ## simulations
    testAlgorithm = sienaAlgorithmCreate( projname = 'fullSAO')
    logMIAns[[a]] = siena07(testAlgorithm, data = fullNet, effects = myEff)

}

save(logMIAns, file = "ergms/logMISAO.RData")
logMIAns


#####
## cart multiple imputation
#####
## test
#wave.2.network[wave.2.network == 9] = NA
#trust[trust$V2 == 11, 2] = NA
#proDev[proDev == 11] = NA
#proDev[proDev == 8] = NA

load("imputation/cartMIWaveShort.RData")
load("imputation/cartMICov.RData")
cartMIAns = vector("list", 10)
set.seed(86139)

for(a in 1:10){
    wave.2.network = cartMIWave2[[a]]
    wave.2.network[wave.1.network == 10] = 10
    
    testNet = sienaDependent(array(c(as.matrix(wave.1.network), as.matrix(wave.2.network)), 
                                   dim=c(194, 194, 2)))
    trustAtt =  sienaDependent(as.matrix(cartMICov[[a]][ , 1:2]), type = "behavior")
    #trustCo = coCovar(as.matrix(trust)[, 1])
    prodevAtt = coCovar(cartMICov[[a]][ , 3])
    govAtt = coCovar(cartMICov[[a]][ , 4])
    #trustCo = coCovar(as.matrix(trust)[, 2])
    
    fullNet = sienaDataCreate(testNet, trustAtt, prodevAtt, govAtt)
    myEff = getEffects(fullNet)
    myEff = includeEffects(myEff, transTrip, inPop)
    myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "prodevAtt")
    myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "govAtt")
    myEff = includeEffects(myEff, egoX, altX, simX, interaction1 = "trustAtt")
    myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "govAtt")
    myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "prodevAtt")
    #myEff = includeEffects(myEff, name = "trustAtt", effFrom, interaction1 = "trustCo")
    myEff = includeEffects(myEff, name = "trustAtt", avSim, interaction1 = "testNet")
    
    myEff
    #which(myEff$include == TRUE)
    #myEff$include[311] = FALSE
    #myEff
    
    ## simulations
    testAlgorithm = sienaAlgorithmCreate( projname = 'fullSAO')
    cartMIAns[[a]] = siena07(testAlgorithm, data = fullNet, effects = myEff)
    
}

save(cartMIAns, file = "ergms/cartMISAO.RData")
cartMIAns
