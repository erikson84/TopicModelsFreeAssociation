# Script to run cross-validation tests based on
# model parameters to predict speaker and session number
# from the topic probabilities for each document (theta_d)
# using Random Forest models.

library(randomForest)
dim(MFinal$theta)
set.seed(666)
folds <- c(sample(rep(1:10, 317)), sample(1:10, 2, replace = FALSE))
out <- array(NA, c(10, 3))

dadosInter <- cbind(data.frame(inter=corpusFinal$meta$speaker), data.frame(MFinal$theta))
dadosSession <- cbind(data.frame(session=corpusFinal$meta$session), data.frame(MFinal$theta))

for (i in 1:10){
  mTemp <- randomForest(inter~., data=dadosInter, subset=folds!=i)
  pred <- predict(mTemp, newdata=dadosInter[folds==i,])
  tPred <- table(dadosInter$inter[folds==i], pred)
  out[i, 1] <- 1 - (sum(diag(tPred))/sum(tPred))
  
  mTemp2 <- randomForest(session~., data=dadosSession, subset=folds!=i)
  pred <- predict(mTemp2, newdata=dadosSession[folds==i,])
  mse <- mean((dadosSession$session[folds==i] - round(pred))^2)
  mae <- median(abs(dadosSession$session[folds==i] - round(pred)))
  out[i, 2] <- mse
  out[i, 3] <- mae
}