#####
## carry forward imputation
#####
origNet1 = wave.1.network
origNet1[origNet1 == 10] = 0

origNet2 = wave.2.network
origNet2[origNet2 == 9] = NA
origNet2[origNet2 == 10] = 0

carry.forward.impute = origNet2

for(a in 1:nrow(origNet1)){
    carry.forward.impute[is.na(carry.forward.impute[ , a]), a] = 
        origNet1[is.na(carry.forward.impute[ , a]), a]
}

carryForwardNetwork = network(carry.forward.impute, directed = T)
carryForwardNetwork

save(carryForwardNetwork, file = "imputation/carryForward.RData")

dyad.census(carryForwardNetwork)
summary(degree(carryForwardNetwork, cmode = "indegree"))
summary(degree(carryForwardNetwork, cmode = "outdegree"))