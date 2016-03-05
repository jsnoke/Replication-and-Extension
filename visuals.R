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
##### igraph
#####
detach("package:sna", unload = T)
library(igraph)

## wave 1
wave.1.network[wave.1.network == 10] = 0
#wave.1.nets = vector("list", 10)
#wave1Nets = vector("list", 10)
#b = c(19, 13, 24, 21, 16, 21, 25, 20, 20, 15)
#b1 = 1
#b2 = b[1]
#for(a in 1:10){
    wave.1.nets[[a]] = wave.1.network[b1:b2, b1:b2]

    wave1Nets[[a]] = graph_from_adjacency_matrix(as.matrix(wave.1.nets[[a]]), mode = "directed") 
    
    V(wave1Nets[[a]])$group = rep(a, b[a])
    V(wave1Nets[[a]])$trust = trust$V1[b1:b2]
    V(wave1Nets[[a]])$prodev = proDev[b1:b2, ]
    V(wave1Nets[[a]])$gov = gov[b1:b2, ]
    
    b1 = b1 + b[a]
    b2 = b2 + b[a + 1]
}

#layouts = lapply(wave1Nets, layout_with_kk)
#lay = merge_coords(wave1Nets, layouts)
#g = disjoint_union(wave1Nets)
#plot(g, lay, vertex.label.cex = 0.5, vertex.size = 10, 
#     edge.arrow.size = 0.03)

wave1Net = graph_from_adjacency_matrix(as.matrix(wave.1.network), mode = "directed")    
V(wave1Net)$group = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                   rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
V(wave1Net)$trust = trust$V1
V(wave1Net)$prodev = proDev
V(wave1Net)$gov = gov

foo = layout_nicely(wave1Net)
plot(wave1Net, layout = foo, vertex.label.cex = 0.5, vertex.size = V(wave1Net)$trust, 
     edge.arrow.size = 0.03, vertex.color = V(wave1Net)$group)

dC = dyad_census(wave1Net)
tC = triad_census(wave1Net)
c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C", "201", "120D", "120U", 
  "120C", "210", "300")

mean(degree(wave1Net, mode = "in"))

## wave 2
wave.2.network[wave.2.network == 10] = 0
wave.2.network[wave.2.network == 9] = 0
which(is.na(wave.2.network[,1]))

test = na.omit(wave.2.network)

wave2Net = graph_from_adjacency_matrix(na.omit(as.matrix(wave.2.network)), mode = "directed")
V(wave2Net)$group = c(rep(1, 19), rep(2, 13), rep(3, 24), rep(4, 21), rep(5, 16),
                      rep(6, 21), rep(7, 25), rep(8, 20), rep(9, 20), rep(10, 15))
V(wave2Net)$trust = trust$V2
V(wave2Net)$prodev = proDev
V(wave2Net)$gov = gov

foo = layout_nicely(wave2Net)
plot(wave2Net, layout = foo, vertex.label.cex = 0.5, vertex.size = V(wave1Net)$trust, 
     edge.arrow.size = 0.03, vertex.color = V(wave1Net)$group)

#####
## isolates...
#####
foo[5, ] = colMeans(foo[1:19,, drop = F])
foo[8, ] = colMeans(foo[1:19,, drop = F])

foo[33, ] = colMeans(foo[33:56,, drop = F])
foo[34, ] = colMeans(foo[33:56,, drop = F])
foo[36, ] = colMeans(foo[33:56,, drop = F])
foo[51, ] = colMeans(foo[33:56,, drop = F])
foo[52, ] = colMeans(foo[33:56,, drop = F])

foo[57, ] = colMeans(foo[57:77,, drop = F])
foo[66, ] = colMeans(foo[57:77,, drop = F])
foo[67, ] = colMeans(foo[57:77,, drop = F])

foo[79, ] = colMeans(foo[78:93,, drop = F])
foo[80, ] = colMeans(foo[78:93,, drop = F])
foo[87, ] = colMeans(foo[78:93,, drop = F])

foo[98, ] = colMeans(foo[94:114,, drop = F])
foo[108, ] = colMeans(foo[94:114,, drop = F])
foo[109, ] = colMeans(foo[94:114,, drop = F])

foo[120, ] = colMeans(foo[115:139,, drop = F])
foo[121, ] = colMeans(foo[115:139,, drop = F])
foo[126, ] = colMeans(foo[115:139,, drop = F])

foo[143, ] = colMeans(foo[140:159,, drop = F])
foo[145, ] = colMeans(foo[140:159,, drop = F])
foo[147, ] = colMeans(foo[140:159,, drop = F])
foo[150, ] = colMeans(foo[140:159,, drop = F])
foo[152, ] = colMeans(foo[140:159,, drop = F])
foo[155, ] = colMeans(foo[140:159,, drop = F])
foo[156, ] = colMeans(foo[140:159,, drop = F])

foo[162, ] = colMeans(foo[160:179,, drop = F])
foo[163, ] = colMeans(foo[160:179,, drop = F])
foo[166, ] = colMeans(foo[160:179,, drop = F])

foo[185, ] = colMeans(foo[180:194,, drop = F])
foo[187, ] = colMeans(foo[180:194,, drop = F])

#####
##### sna
#####
detach("package:igraph", unload = T)
library(sna)

wave.1.network[wave.1.network == 10] = 0
tC = triad.census(wave.1.network)
dC = dyad.census(wave.1.network)
pC = kpath.census(wave.1.network, maxlen = 2)

mean(degree(wave.1.network))


