wave.1.network <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/data/wave 1 network.dat", 
                             quote="\"", comment.char="")

wave.2.network <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/data/wave 2 network.dat", 
                             quote="\"", comment.char="")

trust <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/data/Trust.dat", 
                    quote="\"", comment.char="")

proDev <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/data/Prodev-proenv1-7.dat", 
                    quote="\"", comment.char="")

gov <- read.table("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/data/data/gov-nongov.dat", 
                    quote="\"", comment.char="")

library(igraph)

wave.1.network[wave.1.network == 10] = 0
wave1Net = graph_from_adjacency_matrix(as.matrix(wave.1.network), mode = "directed")
V(wave1Net)$group = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                   rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
V(wave1Net)$trust = trust$V1
V(wave1Net)$prodev = proDev
V(wave1Net)$gov = gov

foo = layout.auto(wave1Net)
plot(wave1Net, layout = foo, vertex.label.cex = 0.5, vertex.size = V(wave1Net)$trust, 
     edge.arrow.size = 0.03, vertex.color = V(wave1Net)$group)

triad_census(wave1Net)


