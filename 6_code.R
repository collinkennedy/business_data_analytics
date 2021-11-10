library(ISLR)
library(Matrix)
library(gamlr)
library(MASS)
library(class)
library(glmnet)
library(tree)
library(randomForest)
library(ranger)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(NLP) 
 
## Data prep starts 
CAhousing <- read.csv("CAhousing.csv")
CAhousing$AveBedrms <- CAhousing$totalBedrooms/CAhousing$households
CAhousing$AveRooms <- CAhousing$totalRooms/CAhousing$households
CAhousing$AveOccupancy <- CAhousing$population/CAhousing$households
logMedVal <- log(CAhousing$medianHouseValue)
CAhousing <- CAhousing[,-c(4,5,9)] # lose medval and the room totals
CAhousing$logMedVal <- logMedVal # attach to the DF
## Data prep ends 

head(CAhousing)

XXca <- model.matrix(logMedVal~.*longitude*latitude, data=data.frame(scale(CAhousing)))[,-1]


#### Trees
catree <- tree(logMedVal ~ ., data=CAhousing) 
cvca <- cv.tree(catree)
cvca$dev

pdf("6_code_figure1.pdf")
plot(catree)
text(catree)
dev.off()


## Random Forest 
carf <- ranger(logMedVal ~ ., data=CAhousing,write.forest=TRUE, num.tree=200, min.node.size=25, importance="impurity")
sort(carf$variable.importance, decreasing=TRUE)


## LASSO
calasso <- cv.gamlr(x=XXca, y=logMedVal)
round(coef(calasso),2)


## Out-of-sample comparison 

MSE <- list(LASSO=NULL, CART=NULL, RF=NULL)
for(i in 1:10){
  train <- sample(1:nrow(CAhousing), 5000)
  
  lin       <- cv.gamlr(x=XXca[train,], y=logMedVal[train])
  yhat.lin  <- drop(predict(lin, XXca[-train,], select="min"))
  MSE$LASSO <- c( MSE$LASSO, var(logMedVal[-train] - yhat.lin))
  
  rt       <- tree(logMedVal ~ ., data=CAhousing[train,])
  yhat.rt  <- predict(rt, newdata=CAhousing[-train,])
  MSE$CART <- c( MSE$CART, var(logMedVal[-train] - yhat.rt))
  
  rf      <- ranger(logMedVal ~ ., data=CAhousing[train,],num.tree=200, min.node.size=25, write.forest=TRUE)
  yhat.rf <- predict(rf, data=CAhousing[-train,])$predictions
  MSE$RF  <- c( MSE$RF, var(logMedVal[-train] - yhat.rf) )
  
} 
pdf("6_code_figure2.pdf")
boxplot(log(as.data.frame(MSE)), xlab="model", ylab="log(MSE)")
dev.off()





### Below is for Figure 9.12
# 
# yhatlasso <- predict(calasso, XXca, select="min")
# yhattree  <- predict(catree, CAhousing)
# yhatrf    <- predict(carf, CAhousing)$predictions
# 
# rl <- drop(logMedVal - yhatlasso)
# rt <- logMedVal - yhattree
# rr <- logMedVal - yhatrf
# 
# pdf("6_code_figure0.pdf", width=8, height=4)
# par(mfrow=c(1,3), mai=c(.1,.1,.1,.1), omi=c(0,0,0,0))
# map('state', 'california') 
# points(CAhousing[,1:2], col=c("black","gray70")[1 + (rl>0)], cex=abs(rl))
# mtext("lasso", line=1)
# map('state', 'california') 
# points(CAhousing[,1:2], col=c("black","gray70")[1 + (rt>0)], cex=abs(rt))
# mtext("tree", line=1)
# map('state', 'california') 
# points(CAhousing[,1:2], col=c("black","gray70")[1 + (rr>0)], cex=abs(rr))
# mtext("forest", line=1)
# legend("topright", title="residuals", bty="n", pch=1, pt.cex=c(2,1,1,2), col=c("gray70","gray70","black","black"), legend=c(2,1, -1,-2))
# dev.off()
