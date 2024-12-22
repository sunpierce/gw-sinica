library(dplyr)
library(ggplot2)
library(MASS) # LDA, QDA
library(class) # KNN
library(caret)
library(smotefamily)
library(SPlit)
library(vioplot)
library(knitr)
library(kableExtra)
library(randomForest)
library(gbm)
library(GpGp)
library(fields)
library(tree)
dat = read.csv("/Users/sunpierce/Desktop/Academia Sinica/Research/frequency/Frequency.csv")

#### Preprocessing ####
# Create a binary variable indicating non-zero probability and declare nominal categorical column as factor
dat <- dat %>%
  mutate(prob_nonzero = ifelse(probability > 0, 1, 0))
dat$prob_nonzero = as.factor(dat$prob_nonzero)

# Highly imbalanced
freq_table = table(dat$prob_nonzero)
prop_table = prop.table(freq_table)
barplot(prop_table, ylim = c(0,1), main = "Distribution of prob_nonzero", xlab = "prob_nonzero",
        ylab = "proportion")

#### Figure 2. ####
set.seed(5)
N = 100 # number of replication
FNR1 = ERR1 = PRO1 = matrix(0, N, 30)
FNR2 = ERR2 = PRO2 = matrix(0, N, 30)
FNR3 = ERR3 = PRO3 = matrix(0, N, 30)
FNR4 = ERR4 = PRO4 = matrix(0, N, 30)
for (i in 1:N) {
  # SPlit the data
  # train: 672, test: 210, validation: 168
  first.idx = SPlit(dat, splitRatio = 0.2)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  # Naive method - Always predict prob_nonzero to 1 (worst)
  pred.naive = rep(1, nrow(test))
  Naive_Err = mean(pred.naive != test$prob_nonzero)
  Naive_FNR = 0
  conf_matrix = table(pred.naive, test$prob_nonzero)
  Naive_PRO = conf_matrix["1","1"] / conf_matrix["1","0"]
  # Logistic regresssion (1)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.logit = glm(prob_nonzero~center+neighbor, data = train, family = binomial)
    prob.logit = predict(fit.logit, validation, type = "response")
    pred.logit = rep(0, length(prob.logit))
    pred.logit[prob.logit > threshold[j]] = 1
    conf_matrix = table(pred.logit, validation$prob_nonzero)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR1[i,j] = false_negatives / sum(validation$prob_nonzero == 1)
    # overall error rate
    ERR1[i,j] = mean(pred.logit != validation$prob_nonzero)
    PRO1[i,j] = conf_matrix["1","1"] / conf_matrix["1","0"]
  }
  # LDA (2)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.lda = lda(prob_nonzero~center+neighbor, data = train)
    out.lda = predict(fit.lda, validation)
    prob.lda = out.lda$posterior[,"1"]
    pred.lda = rep(0, length(prob.lda))
    pred.lda[prob.lda > threshold[j]] = 1
    conf_matrix = table(pred.lda, validation$prob_nonzero)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR2[i,j] = false_negatives / sum(validation$prob_nonzero == 1)
    # overall error rate
    ERR2[i,j] = mean(pred.lda != validation$prob_nonzero)
    PRO2[i,j] = conf_matrix["1","1"] / conf_matrix["1","0"]
  }
  # QDA (3)
  threshold = seq(0, 0.5, length.out = 30)
  for (j in 1:30) {
    fit.qda = qda(prob_nonzero~center+neighbor, data = train)
    out.qda = predict(fit.qda, validation)
    prob.qda = out.qda$posterior[,"1"]
    pred.qda = rep(0, length(prob.qda))
    pred.qda[prob.qda > threshold[j]] = 1
    conf_matrix = table(pred.qda, validation$prob_nonzero)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR3[i,j] = false_negatives / sum(validation$prob_nonzero == 1)
    # overall error rate
    ERR3[i,j] = mean(pred.qda != validation$prob_nonzero)
    PRO3[i,j] = conf_matrix["1","1"] / conf_matrix["1","0"]
  }
  # KNN (4)
  K = 30
  train.X = train[,c("center", "neighbor")]
  train.Y = train$prob_nonzero
  validation.X = validation[,c("center", "neighbor")]
  validation.Y = validation$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  for (j in 1:K) {
    pred.knn = knn(train.X, validation.X, train.Y, k = j)
    conf_matrix = table(pred.knn, validation.Y)
    # False negative rate
    false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
      conf_matrix["0","1"]
    } else {
      0  # If the indices are missing, default to 0
    }
    FNR4[i,j] = false_negatives / sum(validation.Y == 1)
    # overall error rate
    ERR4[i,j] = mean(pred.knn != validation.Y)
    PRO4[i,j] = conf_matrix["1","1"] / conf_matrix["1","0"]
  }
}
FNR_1 = colMeans(FNR1); ERR_1 = colMeans(ERR1); PRO_1 = colMeans(PRO1)
FNR_2 = colMeans(FNR2); ERR_2 = colMeans(ERR2); PRO_2 = colMeans(PRO2)
FNR_3 = colMeans(FNR3); ERR_3 = colMeans(ERR3); PRO_3 = colMeans(PRO3)
FNR_4 = colMeans(FNR4); ERR_4 = colMeans(ERR4); PRO_4 = colMeans(PRO4)
par(mfrow = c(1,4), mar=c(2.7,2.6,2.6,0.5), mgp=c(1.6,.6,0), cex = 1.5)
ts.plot(cbind(FNR_1, ERR_1, PRO_1), xlab = "threshold", col = 1:3, lwd = 2, main = "Logistic regression", ylim = c(0,1))
# legend("topright", legend = c("False negative rate", "Error rate", "Proportion of nonzero"), col = 1:3, lty = 1, lwd = 2, bty = "n", cex = 0.8)
abline(h = Naive_Err, col = 2, lty = 2, lwd = 2)
abline(h = Naive_FNR, col = 1, lty = 2, lwd = 2)
abline(h = Naive_PRO, col = 3, lty = 2, lwd = 2)
abline(v = 25, col = 4, lty = 3, lwd = 3)
ts.plot(cbind(FNR_2, ERR_2, PRO_2), xlab = "threshold", col = 1:3, lwd = 2, main = "LDA", ylim = c(0,1))
# legend("topright", legend = c("False negative rate", "Error rate", "Proportion of nonzero"), col = 1:3, lty = 1, lwd = 2, bty = "n", cex = 0.8)
abline(h = Naive_Err, col = 2, lty = 2, lwd = 2)
abline(h = Naive_FNR, col = 1, lty = 2, lwd = 2)
abline(h = Naive_PRO, col = 3, lty = 2, lwd = 2)
abline(v = 25, col = 4, lty = 3, lwd = 3)
ts.plot(cbind(FNR_3, ERR_3, PRO_3), xlab = "threshold", col = 1:3, lwd = 2, main = "QDA", ylim = c(0,1))
# legend("topright", legend = c("False negative rate", "Error rate", "Proportion of nonzero"), col = 1:3, lty = 1, lwd = 2, bty = "n", cex = 0.8)
abline(h = Naive_Err, col = 2, lty = 2, lwd = 2)
abline(h = Naive_FNR, col = 1, lty = 2, lwd = 2)
abline(h = Naive_PRO, col = 3, lty = 2, lwd = 2)
abline(v = 26, col = 4, lty = 3, lwd = 3)
ts.plot(cbind(FNR_4, ERR_4, PRO_4), xlab = "number of neighbor", col = 1:3, lwd = 2, main = "KNN", ylim = c(0,1))
# legend("topright", legend = c("False negative rate", "Error rate", "Proportion of nonzero"), col = 1:3, lty = 1, lwd = 2, bty = "n", cex = 0.8)
abline(h = Naive_Err, col = 2, lty = 2, lwd = 2)
abline(h = Naive_FNR, col = 1, lty = 2, lwd = 2)
abline(h = Naive_PRO, col = 3, lty = 2, lwd = 2)
abline(v = 11, col = 4, lty = 3, lwd = 3)
#### End of Figure 2. ####

#### Figure 3. ####
set.seed(5)
N = 200
threshold = seq(0, 0.5, length.out = 30)
FNR = ERR = PRO = matrix(0, N, 5) # 5 indicates number of candidate model
for (i in 1:N) {
  # SPlit the data
  # train: 672, test: 210, validation: 168
  first.idx = SPlit(dat, splitRatio = 0.2)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  # Naive method - Always predict prob_nonzero to 1 (worst)
  pred.naive = rep(1, nrow(test))
  Naive_Err = mean(pred.naive != test$prob_nonzero)
  Naive_FNR = 0
  conf_matrix = table(pred.naive, test$prob_nonzero)
  Naive_PRO = conf_matrix["1","1"] / conf_matrix["1","0"]
  # Logistic regression
  train_valid = rbind(train, validation)
  fit.logit = glm(prob_nonzero~center+neighbor, data = train_valid, family = binomial)
  prob.logit = predict(fit.logit, test, type = "response")
  pred.logit = rep(0, length(prob.logit))
  pred.logit[prob.logit > threshold[25]] = 1
  conf_matrix = table(pred.logit, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[i,1] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[i,1] = mean(pred.logit != test$prob_nonzero)
  PRO[i,1] = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
  # LDA
  fit.lda = lda(prob_nonzero~center+neighbor, data = train_valid)
  out.lda = predict(fit.lda, test)
  prob.lda = out.lda$posterior[,"1"]
  pred.lda = rep(0, length(prob.lda))
  pred.lda[prob.lda > threshold[25]] = 1
  conf_matrix = table(pred.lda, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[i,2] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[i,2] = mean(pred.lda != test$prob_nonzero)
  PRO[i,2] = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
  # QDA
  fit.qda = qda(prob_nonzero~center+neighbor, data = train_valid)
  out.qda = predict(fit.qda, test)
  prob.qda = out.qda$posterior[,"1"]
  pred.qda = rep(0, length(prob.qda))
  pred.qda[prob.qda > threshold[26]] = 1
  conf_matrix = table(pred.qda, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[i,3] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[i,3] = mean(pred.qda != test$prob_nonzero)
  PRO[i,3] = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
  # KNN
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  conf_matrix = table(pred.knn, test.Y)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[i,4] = false_negatives / sum(test.Y == 1)
  # overall error rate
  ERR[i,4] = mean(pred.knn != test.Y)
  PRO[i,4] = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
  # Random forest
  fit.rf = randomForest(prob_nonzero~., data = train_valid)
  pred.rf = predict(fit.rf, test)
  conf_matrix = table(pred.rf, test$prob_nonzero)
  # False negative rate
  false_negatives = if ("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix)) {
    conf_matrix["0","1"]
  } else {
    0  # If the indices are missing, default to 0
  }
  FNR[i,5] = false_negatives / sum(test$prob_nonzero == 1)
  # overall error rate
  ERR[i,5] = mean(pred.rf != test$prob_nonzero)
  PRO[i,5] = conf_matrix["1","1"] / (conf_matrix["1","0"] + conf_matrix["1","1"])
}
par(mfrow = c(1,3), mar=c(2.7,2.6,2.6,0.5), mgp=c(1.6,.6,0), cex = 1)
vioplot(FNR, main = "False negative rate", col = "azure3", names = c("Logistic", "LDA", "QDA", "KNN", "RF"))
abline(h = Naive_FNR, col = "blue", lty = 3, lwd = 3)
vioplot(ERR, main = "Error rate", col = "azure3", names = c("Logistic", "LDA", "QDA", "KNN", "RF"), ylim = c(0,0.9))
abline(h = Naive_Err, col = "blue", lty = 3, lwd = 3)
vioplot(PRO, main = "Proportion of nonzero probability", col = "azure3", names = c("Logistic", "LDA", "QDA", "KNN", "RF"))
abline(h = Naive_PRO, col = "blue", lty = 3, lwd = 3)
#### End of Figure 3. ####

#### Second Layer ####
# Hyperparameter tuning for GB
set.seed(5)
N = 200
ntree = seq(100, 1000, by = 100)
interact = seq(1, 5, by = 1)
MSE = matrix(0, N, 50)
id = 1
for (i in 1:N) {
  first.idx = SPlit(dat, splitRatio = 0.4)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # Mean baseline model
  train_valid = rbind(train, validation)
  # RMSE[i,1] = sqrt(mean((mean(train_valid$probability)-test$probability)^2))
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  train_valid = rbind(train, validation)
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  idx = as.logical(as.integer(as.character(pred.knn)))
  dat.baseline = test
  dat.first = test[!idx,] # no adjustment can be made in the second layer
  dat.first$predict = rep(0, nrow(dat.first))
  # second part
  dat.second = test[idx,]
  # table(dat.second$prob_nonzero)
  # Second layer model
  # SPlit for train, test
  train.idx = SPlit(dat.second, splitRatio = 0.6)
  train = dat.second[-train.idx,]
  test = dat.second[train.idx,]
  for (j in ntree) {
    for (k in interact) {
      fit.gbm = gbm(probability~center+neighbor, data = train, distribution = "gaussian", 
                    n.trees = j, interaction.depth = k)
      pred.gbm = predict(fit.gbm, newdata = test, n.trees = j)
      pred.gbm = pmin(pmax(pred.gbm, 0), 1)  # Clip to [0, 1]
      MSE[i,id] = mean((pred.gbm - test$probability)^2)
      if (id + 1 == 51) {
        id = 1
      } else {
        id = id + 1
      }
    }
  }
}
mean_MSE = colMeans(MSE)
ts.plot(mean_MSE)
which.min(mean_MSE)
# Hyperparameter tuning for randomForest
set.seed(5)
N = 200
ntree = seq(1000, 5000, by = 1000)
nodesize = seq(1, 10, by = 1)
MSE = matrix(0, N, 50)
id = 1
for (i in 1:N) {
  first.idx = SPlit(dat, splitRatio = 0.4)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # Mean baseline model
  train_valid = rbind(train, validation)
  # RMSE[i,1] = sqrt(mean((mean(train_valid$probability)-test$probability)^2))
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  train_valid = rbind(train, validation)
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  idx = as.logical(as.integer(as.character(pred.knn)))
  dat.baseline = test
  dat.first = test[!idx,] # no adjustment can be made in the second layer
  dat.first$predict = rep(0, nrow(dat.first))
  # second part
  dat.second = test[idx,]
  # table(dat.second$prob_nonzero)
  # Second layer model
  # SPlit for train, test
  train.idx = SPlit(dat.second, splitRatio = 0.6)
  train = dat.second[-train.idx,]
  test = dat.second[train.idx,]
  for (j in ntree) {
    for (k in nodesize) {
      fit.rf = randomForest(probability~neighbor+center, data = train, ntree = j, nodesize = k)
      pred.rf = predict(fit.rf, test)
      pred.rf = pmin(pmax(pred.rf, 0), 1)  # Clip to [0, 1]
      MSE[i,id] = mean((pred.rf - test$probability)^2)
      if (id + 1 == 51) {
        id = 1
      } else {
        id = id + 1
      }
    }
  }
}
mean_MSE_rf = colMeans(MSE)
ts.plot(mean_MSE_rf)
which.min(mean_MSE_rf)

# Modeling
set.seed(5)
N = 300
RMSE = matrix(0, N, 6) # 6 indicates the number of candidate models
ACCU = matrix(0, N, 6) # accuracy
ERROR = matrix(0, N, 4) # false negative rate exclude baseline
for (i in 1:N) {
  first.idx = SPlit(dat, splitRatio = 0.4)
  first = dat[-first.idx,]
  test = dat[first.idx,]
  valid.idx = SPlit(first, splitRatio = 0.2)
  train = first[-valid.idx,]
  validation = first[valid.idx,]
  # over-sampling using SMOTE on training data only
  genData = SMOTE(train[,c("center", "neighbor")], train[,c("prob_nonzero")])
  train.balanced = genData$data
  train.balanced$prob_nonzero = as.factor(train.balanced$class)
  train = train.balanced[,c("center", "neighbor", "prob_nonzero")]
  validation = validation[,c("center", "neighbor", "prob_nonzero")]
  train_valid = rbind(train, validation)
  train_valid.X = train_valid[,c("center", "neighbor")]
  train_valid.Y = train_valid$prob_nonzero
  test.X = test[,c("center", "neighbor")]
  test.Y = test$prob_nonzero
  pred.knn = knn(train_valid.X, test.X, train_valid.Y, k = 11)
  idx = as.logical(as.integer(as.character(pred.knn)))
  dat.baseline = test
  dat.first = test[!idx,] # no adjustment can be made in the second layer
  dat.first$predict = rep(0, nrow(dat.first))
  # second layer
  dat.second = test[idx,]
  # table(dat.second$prob_nonzero)
  # Second layer model
  # SPlit for train, test
  train.idx = SPlit(dat.second, splitRatio = 0.6)
  train = dat.second[-train.idx,]
  test = dat.second[train.idx,]
  train.idx = SPlit(dat.baseline, splitRatio = 0.6)
  train.baseline = dat.baseline[-train.idx,]
  test.baseline = dat.baseline[train.idx,]
  ## Baseline-1
  RMSE[i,1] = sqrt(mean((mean(dat[-first.idx,"probability"])-dat[first.idx,"probability"])^2))
  ACCU[i,1] = 0 # since the predicted value are all nonzero!!!
  ## Baseline-2
  RMSE[i,2] = sqrt((sum((mean(train.baseline$probability)-test.baseline$probability)^2) + sum((dat.first$probability - dat.first$predict)^2))/(nrow(test.baseline)+nrow(dat.first)))
  same_zero_count = 0
  test_zero_count = sum(test.baseline$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,2] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Gaussian process
  fit.gp = fit_model(train$probability, train[,c("center", "neighbor")], covfun_name="matern15_isotropic", silent = T)
  pred.gp = predictions(fit.gp, locs_pred = test[,c("center", "neighbor")], X_pred = rep(1,nrow(test)))
  pred.gp = pmin(pmax(pred.gp, 0), 1)  # Clip to [0, 1] for those less than 0 set it to 0
  RMSE[i,3] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.gp)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.gp == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,3] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.gp == 0)
  test_zero_count = sum(pred.gp == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,1] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Random forest
  fit.rf = randomForest(probability~neighbor+center, data = train, ntree = 2000, nodesize = 2)
  pred.rf = predict(fit.rf, test)
  pred.rf = pmin(pmax(pred.rf, 0), 1)
  RMSE[i,4] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.rf)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.rf == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,4] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.rf == 0)
  test_zero_count = sum(pred.rf == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,2] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Classification tree
  fit.tree = tree(probability~neighbor+center, data = train)
  # tree.cv = cv.tree(fit.tree)
  # optimal = tree.cv$size[which.min(tree.cv$dev)]
  # fit.pruned = prune.tree(fit.tree, best = optimal)
  # pred.tree = predict(fit.pruned, newdata = test)
  pred.tree = predict(fit.tree, newdata = test)
  pred.tree = pmin(pmax(pred.tree, 0), 1)  # Clip to [0, 1]
  RMSE[i,5] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.tree)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.tree == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,5] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.tree == 0)
  test_zero_count = sum(pred.tree == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,3] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  ## Gradient boosting
  fit.gbm = gbm(probability~center+neighbor, data = train, distribution = "gaussian", 
                n.trees = 100, interaction.depth = 4)
  pred.gbm = predict(fit.gbm, newdata = test, n.trees = 100)
  pred.gbm = pmin(pmax(pred.gbm, 0), 1)  # Clip to [0, 1]
  RMSE[i,6] = sqrt((sum((dat.first$probability - dat.first$predict)^2) + sum((test$probability - pred.gbm)^2)) / (nrow(dat.first) + nrow(test)))
  same_zero_count = sum(test$probability == 0 & pred.gbm == 0)
  test_zero_count = sum(test$probability == 0)
  same_zero_count_first = sum(dat.first$probability == 0 & dat.first$predict == 0)
  real_zero_count = sum(dat.first$probability == 0)
  ACCU[i,6] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
  same_zero_count = sum(test$probability != 0 & pred.gbm == 0)
  test_zero_count = sum(pred.gbm == 0)
  same_zero_count_first = sum(dat.first$probability != 0)
  real_zero_count = length(dat.first$predict)
  ERROR[i,4] = (same_zero_count + same_zero_count_first) / (test_zero_count + real_zero_count)
}
par(mfrow = c(1,1), cex = 1.4, mar = c(2.7,2.6,2.6,0.5), mgp = c(1.6,.6,0))
vioplot(RMSE, main = "RMSE (test)", col = "azure3", names = c("Baseline model-1", "Baseline model-2", "Gaussian Process", "Random Forest", "Regression Tree", "Gradient Boosting"))
colMeans(RMSE)
par(mfrow = c(1,1), cex = 1.4, mar = c(2.7,2.6,2.6,0.5), mgp = c(1.6,.6,0))
vioplot(ACCU, main = "Accuracy (test)", col = "azure3", names = c("Baseline model-1", "Baseline model-2", "Gaussian Process", "Random Forest", "Regression Tree", "Gradient Boosting"))
colMeans(ACCU)
par(mfrow = c(1,1), cex = 1.4, mar = c(2.7,2.6,2.6,0.5), mgp = c(1.6,.6,0))
vioplot(ERROR, main = "Error rate (test)", col = "azure3", names = c("Gaussian Process", "Random Forest", "Regression Tree", "Gradient Boosting"))
colMeans(ERROR)