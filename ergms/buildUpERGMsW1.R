library(statnet)
wave.1.network <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/wave 1 network.dat", 
                             quote="\"", comment.char="")

#####
## setup
#####

ergmNet = wave.1.network
ergmNet[ergmNet == 10] = 0

wave1Network = network(ergmNet, directed = T)

load("../imputation/modalCov.RData")

wave1Network %v% "trust" = modalDF$trust1
wave1Network %v% "prodev" = modalDF$prodev
wave1Network %v% "gov" = modalDF$gov
wave1Network %v% "group" = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                       rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))


#####
## base model
#####
baseERGM = ergm(wave1Network ~ edges, constraints = ~ blockdiag("group") + bd(maxout = 3), 
                estimate = "MLE")

mcmc.diagnostics(baseERGM) ## nothing
summary(baseERGM)

par(mfrow = c(2, 2))
plot(gof(baseERGM))


#####
## structural terms model - good
#####
structuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + mutual, 
                      constraints = ~ blockdiag("group") + bd(maxout = 3),
                      estimate = "MLE")

mcmc.diagnostics(structuralERGM) ## nothing
summary(structuralERGM)

par(mfrow = c(2, 2))
plot(gof(structuralERGM))


#####
## government and structural model - okay, not great
#####
govStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + mutual + 
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
prodevStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + 
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
trustStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + twopath + ttriple + 
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
allStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + 
                             twopath + ttriple + mutual + 
                               nodeifactor("gov") + nodeofactor("gov") + nodematch("gov") +
                               nodeicov("prodev") + nodeocov("prodev") + absdiff("prodev") +
                               nodeicov("trust") + nodeocov("trust") + absdiff("trust"), 
                           constraints = ~ blockdiag("group") + bd(maxout = 3), 
                           estimate = "MLE") 

summary(allStructuralERGM)
mcmc.diagnostics(allStructuralERGM) ## nothing

par(mfrow = c(3, 4))
plot(gof(allStructuralERGM))


#####
## alter and structural model - decent
#####
alterStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + 
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
egoStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + 
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
similarityStructuralERGM = ergm(wave1Network ~ edges + istar(2) + ostar(2) + 
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


#####
## make table
#####
library(xtable)

wave1Table = xtable(summary(baseERGM)$coefs[,1:2])
wave1Table
wave1Table = xtable(summary(structuralERGM)$coefs[,1:2])
wave1Table
wave1Table = xtable(summary(alterStructuralERGM)$coefs[,1:2])
wave1Table
wave1Table = xtable(summary(allStructuralERGM)$coefs[,1:2])
wave1Table




