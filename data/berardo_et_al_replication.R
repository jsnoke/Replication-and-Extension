#############################
#############################
#
# Replication of network statistics in Berardo et. al 2010
# Wave 1 good
# Wave 2, some discrepancies
#
#############################
#############################

### load in data
wave.1.network = read.table("wave 1 network.dat", quote="\"", comment.char="")

wave.2.network = read.table("wave 2 network.dat", quote="\"", comment.char="")

trust = read.table("Trust.dat", quote="\"", comment.char="")

proDev = read.table("Prodev-proenv1-7.dat", quote="\"", comment.char="")

gov = read.table("gov-nongov.dat", quote="\"", comment.char="")

## network library
library(statnet)

###
## network data
###

### wave 1
wave.1.network[wave.1.network == 10] = 0 ## change structural zeros to 0 for network object
wave1Net = network(wave.1.network, directed = T)
wave1Net ## correct number of nodes, ties

dyad.census(wave1Net) ## correct
triad.census(wave1Net) ## correct

### wave 2
wave.2.network[wave.2.network == 10] = 0 ## change structural zeros to 0 for network object

which((diag(as.matrix(wave.2.network)) == 9)) ## 74 nodes missing (should be 69)
which(diag(as.matrix(wave.2.network) == 1)) ## 6 self-loops (should be 0)

missingInd = which((diag(as.matrix(wave.2.network)) == 9)) ## missing nodes to remove from adjacency matrix
wave.2.network = wave.2.network[-missingInd, ] ## remove rows
wave.2.network = wave.2.network[, -missingInd] ## remove cols

wave2Net = network(wave.2.network, directed = T, loops = T)
wave2Net ## nodes 120, edges 202 (should be 125 and 235)

###
## covariate data
###

### trust
mean(trust$V1) ## correct
sd(trust$V1) ## correct
mean(trust$V2[trust$V2 != 11]) ## correct
sd(trust$V2[trust$V2 != 11]) ## correct
which(trust$V2 == 11) ## 69 reported missing -- correct!

## nodes reported differently, 
## 7 nodes missing only in network wave 2 and 2 nodes only missing in trust
c(setdiff(which(diag(as.matrix(wave.2.network)) == 9), which(trust$V2 == 11)), 
  setdiff(which(trust$V2 == 11), which(diag(as.matrix(wave.2.network)) == 9)))


## pro development
mean(proDev$V1[proDev$V1 != 11]) ## correct
sd(proDev$V1[proDev$V1 != 11])
which(proDev$V1 == 8) ## one is coded 8 (max should be 7 and missing is 11)

## government actor
mean(gov$V1) ## correct
sd(gov$V1) ## correct




