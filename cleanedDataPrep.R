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

detach("package:statnet", unload = T)
library(igraph)

#####
## wave 1
#####
wave.1.network[wave.1.network == 10] = 0

wave1Net = graph_from_adjacency_matrix(as.matrix(wave.1.network), mode = "directed")    
V(wave1Net)$group = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                      rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
V(wave1Net)$trust = trust$V1
V(wave1Net)$prodev = proDev
V(wave1Net)$gov = gov

## summary stats
summary(wave1Net)
mean(degree(wave1Net, mode = "in"))
summary(as.factor(degree(wave1Net, mode = "out")))

dC = dyad_census(wave1Net)
dC
tC = triad_census(wave1Net)
names(tC) = c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C", "201", 
              "120D", "120U", "120C", "210", "300")
tC
trantrip = c("030T", "120U", "120D") ## 54 + 3 + 15
out2 = c("021D"*1, "030T"*1, "120U"*2, "120D"*1, "111U"*1, "120C"*1, "201"*1, "210"*2) ## 53 + 54 + 6 + 15 + 37 + 6 + 16 + 18
in2 = c("021U"*1, "030T"*1, "120U"*1, "120D"*2, "111D"*1, "120C"*1, "201"*1, "210"*2) ## 286 + 54 + 3 + 30 + 120 + 6 + 16 + 18

#####
## wave 2
#####
wave.2.network = wave.2.network[-(which(trust$V2 == 11)), ]
wave.2.network = wave.2.network[, -(which(trust$V2 == 11))]

removeID = 1
for(i in 2:125){
    if(sum(wave.2.network[i, ] < 9) == 0){
        removeID = c(removeID, i)
    }
}

wave.2.network = wave.2.network[-(removeID[-1]), ]
wave.2.network = wave.2.network[, -(removeID[-1])]
wave.2.network[wave.2.network == 10] = 0
diag(wave.2.network) = 0

wave2Net = graph_from_adjacency_matrix(as.matrix(wave.2.network), mode = "directed")
V(wave2Net)$group = c(rep(1, 9), rep(2, 10), rep(3, 19), rep(4, 8), rep(5, 8),
                      rep(6, 12), rep(7, 17), rep(8, 10), rep(9, 14), rep(10, 11))
#V(wave2Net)$trust = trust$V2
#V(wave2Net)$prodev = proDev
#V(wave2Net)$gov = gov

## summary stats
for(i in 1:118){
    if(wave.2.network[i,i] == 1){
        print(i)
    }
}
summary(wave2Net)
mean(degree(wave2Net, mode = "in"))

dC = dyad_census(wave2Net)
dC
tC = triad_census(wave2Net)
names(tC) = c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C", "201", 
              "120D", "120U", "120C", "210", "300")
tC
trantrip = c("030T", "120U", "120D") ## 27 + 5 + 19
out2 = c("021D"*1, "030T"*1, "120U"*2, "120D"*1, "111U"*1, "120C"*1, "201"*1, "210"*2) ## 24 + 27 + 10 + 19 + 7 + 6 + 7 + 16
in2 = c("021U"*1, "030T"*1, "120U"*1, "120D"*2, "111D"*1, "120C"*1, "201"*1, "210"*2) ## 191 + 54 + 5 + 38 + 81 + 6 + 7 + 16

#####
## non-network summary stats
#####
summary(trust$V1)
sd(trust$V1)
summary(trust$V2[trust$V2 != 11])
sd(trust$V2[trust$V2 != 11], na.rm = T)
summary(proDev[proDev != 11]) ## how is there one that is 8?
sd(proDev[proDev != 11 & proDev != 8]) ## this gives right sd
summary(gov$V1)
sd(gov$V1)

compT = trust[trust$V2 != 11, ]
roundT = round(compT$V2 - compT$V1) ## I think what they did

summary(roundT == 0)
summary(roundT > 0) ## one off from paper
summary(roundT < 0) ## ditto
sum(roundT[roundT > 0]) ## also slightly off...
sum(roundT[roundT < 0]) 

#####
## change in links
#####

zeroZero = 0
zeroOne = 0
oneZero = 0
oneOne = 0
toMiss = 0
for(a in 1:194){
    for(b in 1:194){
        if(wave.1.network[a, b] == 10 & wave.2.network[a, b] == 10){
            zeroZero = zeroZero + 1
        } else if(wave.1.network[a, b] == 0 & wave.2.network[a, b] == 0){
            zeroZero = zeroZero + 1
        } else if(wave.2.network[a, b] == 9){
            toMiss = toMiss + 1
        } else if(wave.1.network[a, b] == 0 & wave.2.network[a, b] == 1){
            zeroOne = zeroOne + 1
        } else if(wave.1.network[a, b] == 1 & wave.2.network[a, b] == 0){
            oneZero = oneZero + 1
        } else if(wave.1.network[a, b] == 1 & wave.2.network[a, b] == 1){
            oneOne = oneOne + 1
        }
    }
}
zeroZero
zeroOne
oneZero
oneOne
toMiss

#####
## Network statistics
#####

reciprocity(wave1Net)
transitivity(wave1Net)

mean(betweenness(wave1Net)) / max(betweenness(wave1Net))
mean(evcent(wave1Net)$vector)

par(mfrow = c(1,2))
hist(betweenness(wave1Net) / max(betweenness(wave1Net)), main = "Betweenness", 
     xlab = "Normalized Centrality")
hist(betweenness(wave2Net) / max(betweenness(wave2Net)), main = "Betweenness",
     xlab = "Normalized Centrality")


hist(evcent(wave1Net)$vector, main = "Eigenvector", xlab = "Eigenvector Centrality")
hist(evcent(wave2Net)$vector, main = "Eigenvector", xlab = "Eigenvector Centrality")









