library(ISLR)
library(Matrix)
library(gamlr)
library(MASS)
library(class)
library(glmnet)

browser = read.csv("web-browsers.csv")
head(browser)

pdf('1_code_figure1.pdf')
hist(log(browser$spend), freq=FALSE, xaxt="n",main="", xlab="total online spend", col=8, border="grey90")
lgrid = c(1,10,100,1000,10000,100000)
axis(1, at=log(lgrid), labels=sprintf("%.0e",lgrid))
dev.off()


c(mean(browser$spend),sd(browser$spend),sqrt(var(browser$spend)/dim(browser)[1]))


mean(browser$spend)+c(-1,1)*1.96*sqrt(var(browser$spend)/dim(browser)[1])

mub <- c()
for (b in 1:500){
  samp_b = sample.int(dim(browser)[1], replace=TRUE)
  xbar_b = mean(browser$spend[samp_b])
  mub    = c(mub,xbar_b)
}
sd(mub)




rbind(
  sample.int(5,replace=TRUE),
  sample.int(5,replace=TRUE),
  sample.int(5,replace=TRUE)
)



browser$spend[c(4,4,3,5,2)]
mean(browser$spend[c(4,4,3,5,2)])



test_result = t.test(browser$spend,mu=0,alternative="greater")
test_result$statistic
c(test_result$p.value,1-pnorm(test_result$statistic))

