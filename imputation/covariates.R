#####
## covariate multiple impute - normal regression
#####
proDev$V1[proDev$V1 == 8] = NA
proDev$V1[proDev$V1 == 11] = NA

trust$V2[trust$V2 == 11] = NA

covDF = cbind(trust, proDev, gov)
colnames(covDF) = c("trust1", "trust2", "prodev", "gov")

## trust wave 2
x = as.matrix(cbind(rep(1, length(covDF$trust1[!is.na(covDF$trust2), drop = F])), 
                 covDF$trust1[!is.na(covDF$trust2), drop = F]))
y = as.matrix(covDF$trust2[!is.na(covDF$trust2), drop = F])
xp = as.matrix(cbind(rep(1, length(covDF$trust1[is.na(covDF$trust2), drop = F])), 
                     covDF$trust1[is.na(covDF$trust2), drop = F]))
xtx = t(x) %*% x
pen = 0.00001 * diag(xtx)
if (length(pen)==1) pen = matrix(pen)
v = solve(xtx + diag(pen))
coef = t(y) %*% x %*% v
residuals   = y - x %*% t(coef)
sigma.star  = sqrt(sum((residuals)^2)/rchisq(1, length(y) - ncol(x)))
beta.star   = coef + t((chol((v + t(v))/2) %*% rnorm(ncol(x))) * sigma.star)
parm = list(coef, beta.star, sigma.star)
names(parm) = c("coef","beta","sigma") 
res = xp %*% t(parm$beta) + rnorm(nrow(xp)) * parm$sigma
res = round(res, 1)
res[res > 10] = 10
res[res < 0] = 0
covDF$trust2[is.na(covDF$trust2)] = res

## prodev
x = as.matrix(cbind(rep(1, nrow(covDF[!is.na(covDF$prodev), 1:2, drop = F])), 
                    covDF[!is.na(covDF$prodev), 1:2, drop = F]))
y = as.matrix(covDF$prodev[!is.na(covDF$prodev), drop = F])
xp = as.matrix(cbind(rep(1, nrow(covDF[is.na(covDF$prodev), 1:2, drop = F])),
                     covDF[is.na(covDF$prodev), 1:2]))
xtx = t(x) %*% x
pen = 0.00001 * diag(xtx)
if (length(pen)==1) pen = matrix(pen)
v = solve(xtx + diag(pen))
coef = t(y) %*% x %*% v
residuals   = y - x %*% t(coef)
sigma.star  = sqrt(sum((residuals)^2)/rchisq(1, length(y) - ncol(x)))
beta.star   = coef + t((chol((v + t(v))/2)) %*% rnorm(ncol(x)) * sigma.star)
parm = list(coef, beta.star, sigma.star)
names(parm) = c("coef","beta","sigma") 
res = xp %*% t(parm$beta) + rnorm(nrow(xp)) * parm$sigma
res = round(res, 1)
res[res > 7] = 7
res[res < 1] = 1
covDF$prodev[is.na(covDF$prodev)] = res


summary(covDF)

#####
## covariate multiple impute - CART
#####
proDev$V1[proDev$V1 == 8] = NA
proDev$V1[proDev$V1 == 11] = NA

trust$V2[trust$V2 == 11] = NA

covDF = cbind(trust, proDev, gov)
colnames(covDF) = c("trust1", "trust2", "prodev", "gov")

## trust wave 2
x = as.matrix(cbind(rep(1, length(covDF$trust1[!is.na(covDF$trust2), drop = F])), 
                    covDF$trust1[!is.na(covDF$trust2), drop = F]))
y = as.matrix(covDF$trust2[!is.na(covDF$trust2), drop = F])
xp = as.matrix(cbind(rep(1, length(covDF$trust1[is.na(covDF$trust2), drop = F])), 
                     covDF$trust1[is.na(covDF$trust2), drop = F]))

s = sample(length(y), replace=TRUE)
fit = rpart(V3 ~ ., data = as.data.frame(cbind(x, y)), method = "anova",
             minbucket = 5, cp = 1e-04)
leafnr = floor(as.numeric(row.names(fit$frame[fit$where,])))
fit$frame$yval = as.numeric(row.names(fit$frame))
nodes = predict(object = fit, newdata = as.data.frame(xp))
uniquenodes <- unique(nodes)
new  <- vector("numeric",nrow(xp))
for(j in uniquenodes){
    donors = y[leafnr==j] # values of y in a leaf
    new[nodes==j] = resample(donors,size=sum(nodes==j),replace=T)
}
new[new > 10] = 10
new[new < 0] = 0
covDF$trust2[is.na(covDF$trust2)] = new

## prodev
x = as.matrix(cbind(rep(1, nrow(covDF[!is.na(covDF$prodev), 1:2, drop = F])), 
                    covDF[!is.na(covDF$prodev), 1:2, drop = F]))
colnames(x) = c("intercept", "trust1", "trust2")
y = as.matrix(covDF$prodev[!is.na(covDF$prodev), drop = F])
xp = as.matrix(cbind(rep(1, nrow(covDF[is.na(covDF$prodev), 1:2, drop = F])),
                     covDF[is.na(covDF$prodev), 1:2]))
colnames(xp) = c("intercept", "trust1", "trust2")
s = sample(length(y), replace=TRUE)
fit = rpart(V4 ~ ., data = as.data.frame(cbind(x, y)), method = "anova",
            minbucket = 5, cp = 1e-04)
leafnr = floor(as.numeric(row.names(fit$frame[fit$where,])))
fit$frame$yval = as.numeric(row.names(fit$frame))
nodes = predict(object = fit, newdata = as.data.frame(xp))
uniquenodes = unique(nodes)
new = vector("numeric",nrow(xp))
for(j in uniquenodes){
    donors = y[leafnr==j] # values of y in a leaf
    new[nodes==j] = resample(donors,size=sum(nodes==j),replace=T)
}
new[new > 7] = 7
new[new < 1] = 1
covDF$prodev[is.na(covDF$prodev)] = new


summary(covDF)

#####
## covariate modal impute
#####
proDev$V1[proDev$V1 == 8] = NA
proDev$V1[proDev$V1 == 11] = NA

trust$V2[trust$V2 == 11] = NA

modalDF = cbind(trust, proDev, gov)
colnames(modalDF) = c("trust1", "trust2", "prodev", "gov")

mean(proDev$V1, na.rm = T)
modalDF$prodev[is.na(modalDF$prodev)] = mean(proDev$V1, na.rm = T)

mean(trust$V2, na.rm = T)
modalDF$trust2[is.na(modalDF$trust2)] = mean(trust$V2, na.rm = T)

summary(modalDF)

save(modalDF, file = "imputation/modalCov.RData")





