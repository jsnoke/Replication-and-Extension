library(igraph)
setwd("~/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension")

##
ca.AstroPh = read.delim("data/ca-AstroPh.txt", header=FALSE, comment.char="#")


##
wiki.Vote = read.delim("data/wiki-Vote.txt", header=FALSE, comment.char="#")
wikiNetwork = graph_from_data_frame(wiki.Vote, directed = T)

wikiLayout = layout_with_graphopt(wikiNetwork)

plot(wikiNetwork, layout = wikiLayout, edge.arrow.size = 0.2, rescale = F,
     xlim = range(wikiLayout[, 1]), ylim = range(wikiLayout[, 2]))
