## libraries  
library(ISLR)
library(Matrix)
library(gamlr)
library(MASS)
library(class)
library(glmnet)
 

## Regularization example
 
SC <- read.csv("semiconductor.csv")
# names(SC)


full <- glm(FAIL~., data=SC, family=binomial)
1 - full$deviance/full$null.deviance


pvals <- summary(full)$coef[-1,4] #-1 to drop the intercept
pdf('3_code_figure1.pdf')
hist(pvals, xlab="p-value", main="", col="lightblue")# figure 3.1
dev.off()


signif <- which(pvals <= quantile(pvals,25/200))
cutvar <- c("FAIL", names(signif))
cut    <- glm(FAIL ~ ., data=SC[cutvar], family="binomial")
1-cut$deviance/cut$null.deviance # new in-sample R2


# page 73
# setup the experiment
n <- nrow(SC) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
foldid[1:20]



# page 72
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)){ y <- as.numeric(y)>1 }
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev  <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}


# create an empty dataframe of results
OOS    <- data.frame(full=rep(NA,K), cut=rep(NA,K)) 
?rep
for(k in 1:K){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  rcut  <- glm(FAIL~., data=SC[,cutvar], subset=train, family=binomial)
  
  ## get predictions: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  predcut  <- predict(rcut, newdata=SC[-train,], type="response")
  
  ## calculate and log R2
  OOS$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  OOS$cut[k] <- R2(y=SC$FAIL[-train], pred=predcut, family="binomial")
  
}
colMeans(OOS)



full=rep(NA,K)
cut=rep(NA,K)
for(k in 1:K){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions
  rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
  rcut  <- glm(FAIL~., data=SC[,cutvar], subset=train, family=binomial)
  
  ## get predictions: type=response so we have probabilities
  predfull <- predict(rfull, newdata=SC[-train,], type="response")
  predcut  <- predict(rcut, newdata=SC[-train,], type="response")
  
  full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
  cut[k] <- R2(y=SC$FAIL[-train], pred=predcut, family="binomial")
  
  
}

c(mean(full),mean(cut))



## Skip starts
web <- read.csv("browser-domains.csv")
sitenames <- scan("browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
web$id <- factor(web$id, levels=1:length(unique(web$id)))
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]
xweb <- sparseMatrix(i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,dims=c(nlevels(web$id),nlevels(web$site)),dimnames=list(id=levels(web$id), site=levels(web$site)))
yspend <- as.matrix(read.csv("browser-totalspend.csv", row.names=1))  
## Skip ends

head(xweb[1, xweb[1,]!=0])
cbind(yspend,xweb)[1,1:10]

#figure 3.6
spender <- gamlr(xweb, log(yspend), verb=FALSE)
pdf('3_code_figure2.pdf')
plot(spender) ## path plot
dev.off()

# figure 3.8
cv.spender <- cv.gamlr(xweb, log(yspend))
pdf('3_code_figure3.pdf')
plot(cv.spender) ## path plot
dev.off()
betamin <- coef(cv.spender, select="min") ## min cv selection
betamin[c("google.com","microsoft.com","jcpenney.com"),]
# Type betamin when you replicate this code to see how many variables are dropped. 
cv.spender$gamlr$lambda[cv.spender$seg.min] # Selected lambda
