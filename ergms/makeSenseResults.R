setwd("/Users/jvsnoke/Documents/Classes/Spring2016/PLSC597E/Replication-and-Extension/ergms")

load("modalSAO.RData")
load("cartMISAO.RData")
load("logMISAO.RData")

meanLog = sdLog = meanCart = sdCart = matrix(NA, nrow = 20, ncol = 10)

for(a in 1:10){
	meanLog[, a] = logMIAns[[a]]$theta
	sdLog[, a] = sqrt(diag(logMIAns[[a]]$covtheta))
	
	meanCart[, a] = cartMIAns[[a]]$theta
	sdCart[, a] = sqrt(diag(cartMIAns[[a]]$covtheta))
}

fooLog = xtable(cbind(rowMeans(meanLog),rowMeans(sdLog),rowMeans(meanCart),rowMeans(sdCart)))
fooLog



load("logStrucERGM.RData")
logERGM = structuralERGM
load("cartStrucERGM.RData")
cartERGM = structuralERGM
load("baseStrucERGM.RData")

meanLog = sdLog = meanCart = sdCart = matrix(NA, nrow = 6, ncol = 10)

for(a in 1:10){
	meanLog[, a] = summary(logERGM[[a]])$coefs[,1]
	sdLog[, a] = summary(logERGM[[a]])$coefs[,2]
	
	meanCart[, a] = summary(cartERGM[[a]])$coefs[,1]
	sdCart[, a] = summary(cartERGM[[a]])$coefs[,2]
}

fooLog = xtable(cbind(summary(structuralERGM)$coefs[,1:2],rowMeans(meanLog),rowMeans(sdLog),rowMeans(meanCart),rowMeans(sdCart)))
fooLog




load("logAllERGMs.RData")
logERGM = allStructuralERGM
load("cartAllERGMs.RData")
cartERGM = allStructuralERGM
load("baseAllERGMs.RData")

meanLog = sdLog = meanCart = sdCart = matrix(NA, nrow = 15, ncol = 10)

for(a in 1:10){
	meanLog[, a] = summary(logERGM[[a]])$coefs[,1]
	sdLog[, a] = summary(logERGM[[a]])$coefs[,2]
	
	meanCart[, a] = summary(cartERGM[[a]])$coefs[,1]
	sdCart[, a] = summary(cartERGM[[a]])$coefs[,2]
}

fooLog = xtable(cbind(summary(allStructuralERGM)$coefs[,1:2],rowMeans(meanLog),rowMeans(sdLog),rowMeans(meanCart),rowMeans(sdCart)))
fooLog









