library(glmnet)
library(gbm)

n=10000

#generate train data

set.seed(13)
X1 = rnorm(n,0,1)
X2 = rnorm(n,2,1)
X3 = rnorm(n,1,1)
eta = 2 + 2*X1 -X2
pi = 1/(1+exp(-eta))
y = rbinom(n,1,pi)
fit = glm(y~X1 + X2 + X3, family=binomial)

pfit = glmnet(cbind(X1,X2,X3), y, 
              family="binomial",
              lambda.min.ratio=0, alpha=1)

#generate test data (unneeded really bc n large)

set.seed(14)
X1 = rnorm(n,0,1)
X2 = rnorm(n,2,1)
X3 = rnorm(n,1,1)
eta = 2 + 2*X1 -X2
pi = 1/(1+exp(-eta))
y = rbinom(n,1,pi)

#get hats

testpred = predict(fit, 
                   as.data.frame(cbind(X1, X2, X3)),  
                   type="response")

hats = predict(pfit, cbind(X1, X2, X3), 
               type="response")

# plots
plot.new()
par(mfrow=c(2,1))
calibrate.plot(y, hats[,2], replace=T, shade.col=NA)
plot(performance(prediction(hats[,2], y), 
                 "tpr", "fpr"), add=F)
text(0.5, 0.5, paste("AUC:", 
round(performance(prediction(hats[,2], y), 
      measure='auc')@y.values[[1]],2)))

