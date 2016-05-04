
#####
## setup
#####

ergmNet2 = wave.2.network
ergmNet2[ergmNet2 == 9] = NA
ergmNet2[ergmNet2 == 10] = 0
diag(ergmNet2) = 0
#ergmNet2[is.na(ergmNet2)] = ergmNet[is.na(ergmNet2)]

wave2Network = network(ergmNet2, directed = T)

load("imputation/carryForward.RData")
load("imputation/modalCov.RData")
wave2Network = carryForwardNetwork

## run imputation using covariates code first
wave2Network %v% "trust" = modalDF$trust2
wave2Network %v% "prodev" = modalDF$prodev
wave2Network %v% "gov" = modalDF$gov
wave2Network %v% "group" = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                       rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
wave2Network
dyad.census(wave2Network)
triad.census(wave2Network)

#####
## base model
#####
baseERGM = ergm(wave2Network ~ edges, constraints = ~ blockdiag("group"), 
                estimate = "MLE")

mcmc.diagnostics(baseERGM) ## nothing
summary(baseERGM)

par(mfrow = c(2, 2))
plot(gof(baseERGM))


#####
## structural terms model - good
#####
load("imputation/cartMIWaveShort.RData")
load("imputation/cartMICov.RData")
structuralERGM = vector("list", 10)
for(a in 1:10){
    wave2Network = network(cartMIWave2[[a]], directed = T)
    
    ## run imputation using covariates code first
    wave2Network %v% "trust" = cartMICov[[a]]$trust2
    wave2Network %v% "prodev" = cartMICov[[a]]$prodev
    wave2Network %v% "gov" = cartMICov[[a]]$gov
    wave2Network %v% "group" = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                                 rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
    
    structuralERGM[[a]] = ergm(wave2Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + mutual, 
                               constraints = ~ blockdiag("group") + bd(maxout = 3),
                               estimate = "MLE")
}

save(structuralERGM, file = "ergms/cartStrucERGM.RData")

mcmc.diagnostics(structuralERGM[[1]]) ## nothing
summary(structuralERGM)

par(mfrow = c(2, 2))
plot(gof(structuralERGM[[1]]))


#####
## government and structural model - okay, not great
#####
govStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + mutual + 
                          nodeifactor("gov") + nodeofactor("gov") + nodematch("gov"), 
                      constraints = ~ blockdiag("group") + bd(maxout = 3), 
                      estimate = "MLE")

summary(govStructuralERGM)
mcmc.diagnostics(govStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(govStructuralERGM))


#####
## pro-development and structural model
#####
prodevStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + 
                                mutual + 
                             nodeifactor("prodev") + nodeofactor("prodev") + absdiff("prodev"), 
                         constraints = ~ blockdiag("group") + bd(maxout = 3), 
                         estimate = "MLE") ## will not even mix

summary(prodevStructuralERGM)
mcmc.diagnostics(prodevStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(prodevStructuralERGM))


#####
## trust and structural model
#####
trustStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + 
                                mutual + 
                                nodeifactor("trust") + nodeofactor("trust") + absdiff("trust"), 
                            constraints = ~ blockdiag("group") + bd(maxout = 3), 
                            estimate = "MLE") ## will not even mix

summary(trustStructuralERGM)
mcmc.diagnostics(trustStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(trustStructuralERGM))


#####
## all attributes and structural model
#####
load("imputation/logMIWaveShort.RData")
load("imputation/logMICov.RData")
allStructuralERGM = vector("list", 10)
for(a in 1:10){
    wave2Network = network(logMIWave2[[a]], directed = T)
    
    ## run imputation using covariates code first
    wave2Network %v% "trust" = logMICov[[a]]$trust2
    wave2Network %v% "prodev" = logMICov[[a]]$prodev
    wave2Network %v% "gov" = logMICov[[a]]$gov
    wave2Network %v% "group" = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                                 rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
    
    allStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + 
                                 twopath + ttriple + mutual + 
                                 nodeifactor("gov") + nodeofactor("gov") + nodematch("gov") +
                                 nodeicov("prodev") + nodeocov("prodev") + absdiff("prodev") +
                                 nodeicov("trust") + nodeocov("trust") + absdiff("trust"), 
                             constraints = ~ blockdiag("group") + bd(maxout = 3), 
                             estimate = "MLE") 
}

save(allStructuralERGM, file = "ergms/baseAllERGMs.RData")


summary(allStructuralERGM)
mcmc.diagnostics(allStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(allStructuralERGM))


#####
## alter and structural model - decent
#####
alterStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + 
                             twopath + ttriple + mutual + 
                             nodeifactor("gov") +
                             nodeicov("prodev") +
                             nodeicov("trust"), 
                         constraints = ~ blockdiag("group") + bd(maxout = 3), 
                         estimate = "MLE") 

summary(alterStructuralERGM)
mcmc.diagnostics(alterStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(alterStructuralERGM))


#####
## ego and structural model - decent
#####
egoStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + 
                               twopath + ttriple + mutual + 
                               nodeofactor("gov") +
                               nodeocov("prodev") +
                               nodeocov("trust"), 
                           constraints = ~ blockdiag("group") + bd(maxout = 3), 
                           estimate = "MLE") 

summary(egoStructuralERGM)
mcmc.diagnostics(egoStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(egoStructuralERGM))


#####
## similarity and structural model - decent
#####
similarityStructuralERGM = ergm(wave2Network ~ edges + istar(2) + ostar(2) + 
                             twopath + ttriple + mutual + 
                             nodematch("gov") +
                             absdiff("prodev") +
                             absdiff("trust"), 
                         constraints = ~ blockdiag("group") + bd(maxout = 3), 
                         estimate = "MLE") 

summary(similarityStructuralERGM)
mcmc.diagnostics(similarityStructuralERGM) ## nothing

par(mfrow = c(2, 2))
plot(gof(similarityStructuralERGM))


