library(igraph)
setwd("C:/Users/jvs140/Desktop")

##
ca.AstroPh = read.delim("ca-AstroPh.txt", header=FALSE, comment.char="#")
ca.AstroPhNetwork = graph_from_data_frame(ca.AstroPh, directed = F)

ca.AstroPhLayout = layout_with_drl(ca.AstroPhNetwork)
save(ca.AstroPhLayout, file = "ca.AstroPhLayout.RData")

#load("ca.AstroPhLayout.RData")
pdf("caAstroPhPlot.pdf", height = 8, width = 8)
plot(ca.AstroPhNetwork, layout = ca.AstroPhLayout, vertex.label = NA, vertex.size = 1)
dev.off()

rm(ca.AstroPh)
rm(ca.AstroPhNetwork)
rm(ca.AstroPhLayout)

##
ca.CondMat = read.delim("ca-CondMat.txt", header=FALSE, comment.char="#")
ca.CondMatNetwork = graph_from_data_frame(ca.CondMat, directed = F)

ca.CondMatLayout = layout_with_drl(ca.CondMatNetwork)
save(ca.CondMatLayout, file = "ca.CondMatLayout.RData")

#load("ca.CondMatLayout.RData")
pdf("caCondMatPlot.pdf", height = 8, width = 8)
plot(ca.CondMatNetwork, layout = ca.CondMatLayout, vertex.label = NA, vertex.size = 1)
dev.off()

rm(ca.CondMat)
rm(ca.CondMatNetwork)
rm(ca.CondMatLayout)

##
ca.HepPh = read.delim("ca-HepPh.txt", header=FALSE, comment.char="#")
ca.HepPhNetwork = graph_from_data_frame(ca.HepPh, directed = F)

ca.HepPhLayout = layout_with_drl(ca.HepPhNetwork)
save(ca.HepPhLayout, file = "ca.HepPhLayout.RData")

#load("ca.HepPhLayout.RData")
pdf("caHepPhPlot.pdf", height = 8, width = 8)
plot(ca.HepPhNetwork, layout = ca.HepPhLayout, vertex.label = NA, vertex.size = 1)
dev.off()

rm(ca.HepPh)
rm(ca.HepPhNetwork)
rm(ca.HepPhLayout)

##
cit.HepTh = read.delim("cit-HepTh.txt", header=FALSE, comment.char="#")
cit.HepThNetwork = graph_from_data_frame(cit.HepTh, directed = T)

cit.HepThLayout = layout_with_drl(cit.HepThNetwork)
save(cit.HepThLayout, file = "cit.HepThLayout.RData")

#load("cit.HepThLayout.RData")
pdf("citHepThPlot.pdf", height = 8, width = 8)
plot(cit.HepThNetwork, layout = cit.HepThLayout, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0.3)
dev.off()

rm(cit.HepTh)
rm(cit.HepThNetwork)
rm(cit.HepThLayout)

##
cit.HepPh = read.delim("cit-HepPh.txt", header=FALSE, comment.char="#")
cit.HepPhNetwork = graph_from_data_frame(cit.HepPh, directed = T)

cit.HepPhLayout = layout_with_drl(cit.HepPhNetwork)
save(cit.HepPhLayout, file = "cit.HepPhLayout.RData")

#load("cit.HepPhLayout.RData")
pdf("citHepPhPlot.pdf", height = 8, width = 8)
plot(cit.HepPhNetwork, layout = cit.HepPhLayout, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0.3)
dev.off()

rm(cit.HepPh)
rm(cit.HepPhNetwork)
rm(cit.HepPhLayout)

##
enron = read.delim("email-Enron.txt", header=FALSE, comment.char="#")
enronNet = graph_from_data_frame(enron, directed = F)

enronLayout = layout_with_drl(enronNet)
save(enronLayout, file = "enronLayout.RData")

#load("enronLayout.RData")
pdf("enronPlot.pdf", height = 8, width = 8)
plot(enronNet, layout = enronLayout, vertex.label = NA, vertex.size = 1)
dev.off()

rm(enron)
rm(enronNet)
rm(enronLayout)


##
oregon.1 = read.delim("oregon1_010331.txt", header=FALSE, comment.char="#")
oregonNet = graph_from_data_frame(oregon.1, directed = F)

oregonLayout = layout_with_drl(oregonNet)
save(oregonLayout, file = "oregonLayout.RData")

#load("oregonLayout.RData")
pdf("oregon1Plot.pdf", height = 8, width = 8)
plot(oregonNet, layout = oregonLayout, vertex.label = NA, vertex.size = 1)
dev.off()

rm(oregon.1)
rm(oregonNet)
rm(oregonLayout)

##
oregon.2 = read.delim("oregon1_010526.txt", header=FALSE, comment.char="#")
oregonNet2 = graph_from_data_frame(oregon.2, directed = F)

oregonLayout2 = layout_with_drl(oregonNet2)
save(oregonLayout2, file = "oregonLayout2.RData")

#load("oregonLayout2.RData")
pdf("oregon2Plot.pdf", height = 8, width = 8)
plot(oregonNet2, layout = oregonLayout2, vertex.label = NA, vertex.size = 1)
dev.off()

rm(oregon.2)
rm(oregonNet2)
rm(oregonLayout2)

##
gnutella.1 = read.delim("p2p-Gnutella25.txt", header=FALSE, comment.char="#")
gnutellaNet = graph_from_data_frame(gnutella.1, directed = T)

gnutellaLayout = layout_with_drl(gnutellaNet)
save(gnutellaLayout, file = "gnutellaLayout.RData")

#load("gnutellaLayout.RData")
pdf("gnutella1Plot.pdf", height = 8, width = 8)
plot(gnutellaNet, layout = gnutellaLayout, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0.3)
dev.off()

rm(gnutella.1)
rm(gnutellaNet)
rm(gnutellaLayout)

##
gnutella.2 = read.delim("p2p-Gnutella30.txt", header=FALSE, comment.char="#")
gnutellaNet2 = graph_from_data_frame(gnutella.2, directed = T)

gnutellaLayout2 = layout_with_drl(gnutellaNet2)
save(gnutellaLayout2, file = "gnutellaLayout2.RData")

#load("gnutellaLayout2.RData")
pdf("gnutella2Plot.pdf", height = 8, width = 8)
plot(gnutellaNet2, layout = gnutellaLayout2, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0.3)
dev.off()

rm(gnutella.2)
rm(gnutellaNet2)
rm(gnutellaLayout2)

##
socEpin = read.delim("soc-Epinions1.txt", header=FALSE, comment.char="#")
socEpinNet = graph_from_data_frame(socEpin, directed = T)

socEpinLayout = layout_with_drl(socEpinNet)
save(socEpinLayout, file = "socEpinLayout.RData")

#load(socEpinLayout.RData")
pdf("socEpinPlot.pdf", height = 8, width = 8)
plot(socEpinNet, layout = socEpinLayout, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0.3)
dev.off()

rm(socEpin)
rm(socEpinNet)
rm(socEpinLayout)


##
wiki.Vote = read.delim("wiki-Vote.txt", header=FALSE, comment.char="#")
wikiNetwork = graph_from_data_frame(wiki.Vote, directed = T)

wikiLayout = layout_with_drl(wikiNetwork)
save(wikiLayout, file = "wikiLayout.RData")

#load("wikiLayout.RData")
pdf("wikiPlot.pdf", height = 8, width = 8)
plot(wikiNetwork, layout = wikiLayout, vertex.label = NA, vertex.size = 1, edge.arrow.size = 0.3)
dev.off()

rm(wiki.Vote)
rm(wikiNetwork)
rm(wikiLayout)

















