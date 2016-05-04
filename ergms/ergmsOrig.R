#library(igraph)
library(statnet)
#detach("package:igraph", unload = T)

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
## ERGM wave 1
#####

ergmNet = wave.1.network
ergmNet[ergmNet == 10] = 0
fooNet = network(ergmNet, directed = T)
plot(fooNet, displayisolates = F, 
     vertex.col = get.vertex.attribute(fooNet, "group"),
     vertex.cex = get.vertex.attribute(fooNet, "trust")/6)

fooNet %v% "trust" = trust$V1
proDev$V1[proDev$V1 == 11] = 5.5
proDev$V1[proDev$V1 == 8] = 5.5
fooNet %v% "prodev" = proDev$V1
fooNet %v% "gov" = gov$V1
fooNet %v% "group" = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                       rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))

## using edges slightly better and less degenerate
test = ergm(fooNet ~ edges + istar(2) + ostar(2) + twopath + ttriple + mutual + 
                nodeifactor("gov") + nodeofactor("gov") + nodeicov("prodev") + nodeocov("prodev") +
                nodeicov("trust") + nodeocov("trust") + nodematch("gov") + absdiff("prodev") + 
                absdiff("trust"), 
            constraints = ~ blockdiag("group"), estimate = "MPLE")
summary(test)
mcmc.diagnostics(test)
plot(gof(test))
plot(simulate(test))
testSimulation = simulate(test, nsim = 2)
summary(testSimulation)

test = ergm(fooNet ~ istar(2) + ostar(2) + mutual + nodeicov("trust") + nodeocov("trust"),
            constraints = ~ blockdiag("group"))
summary(test)
mcmc.diagnostics(test)
plot(simulate(test))

par(mfrow = c(2,2))
plot(gof(test))

#####
## ERGM wave2
#####

ergmNet2 = wave.2.network
ergmNet2[ergmNet2 == 9] = NA
ergmNet2[ergmNet2 == 10] = 0
ergmNet2[is.na(ergmNet2)] = ergmNet[is.na(ergmNet2)]
fooNet2 = network(ergmNet2, directed = T)
#summary(fooNet2)
#fooNet2 %v% "group" = c(rep(1, 10), rep(2, 11), rep(3, 20), rep(4, 8), rep(5, 10),
#                        rep(6, 13), rep(7, 18), rep(8, 10), rep(9, 14), rep(10, 11))
#fooNet2 %v% "group" = c(rep(1, 9), rep(2, 10), rep(3, 19), rep(4, 8), rep(5, 8),
#                        rep(6, 12), rep(7, 17), rep(8, 10), rep(9, 14), rep(10, 11))
fooNet2 %v% "group" = c(rep(11, 19), rep(12, 13), rep(13, 24), rep(14, 21), rep(15, 16),
                       rep(16, 21), rep(17, 25), rep(18, 20), rep(19, 20), rep(20, 15))                        
#fooNet2 %v% "trust" = trust$V2[as.numeric(row.names(wave.2.network))]
trust$V2[is.na(trust$V2)] = 6.04
fooNet2 %v% "trust" = trust$V2
plot(fooNet2, displayisolates = F, 
     vertex.col = get.vertex.attribute(fooNet2, "group"),
     vertex.cex = get.vertex.attribute(fooNet2, "trust")/6)
fooNet2 %v% "gov" = gov$V1
fooNet2 %v% "prodev" = proDev$V1
set.edge.value(fooNet2, "wave1", ergmNet)

test2 = ergm(fooNet2 ~ istar(2) + ostar(2) + twopath + ttriple + mutual + 
                nodeifactor("gov") + nodeofactor("gov") + nodeicov("prodev") + nodeocov("prodev") +
                nodeicov("trust") + nodeocov("trust") + nodematch("gov") + absdiff("prodev") + 
                absdiff("trust"), 
            constraints = ~ blockdiag("group") + bd(maxout = 3), estimate = "MLE")
summary(test2)



