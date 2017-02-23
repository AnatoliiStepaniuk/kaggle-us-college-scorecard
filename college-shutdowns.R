library(caret)
library(e1071)
library(MASS)
library(flux)
library(leaps)
library(Boruta)
library(tree)
library(randomForest)

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

compare_roc_curves <- function(frame.train, frame.test, title){
  set.seed(1)
  frame.train.factor <- frame.train
  frame.train.factor$NON_OPERATING <- as.factor(frame.train$NON_OPERATING)

  # Naive Bayes
  nb.model <- suppressWarnings(train(NON_OPERATING~., data=frame.train.factor, 'nb', trControl=trainControl(method='cv', number=10)))
  nb.pred <- suppressWarnings(predict(nb.model$finalModel, newdata=frame.test))
  roc.nb <- simple_roc(frame.test$NON_OPERATING, unname(nb.pred$posterior[,2]))
  auc.nb <- auc(x = roc.nb$FPR, y = roc.nb$TPR)

  # LDA
  lda.fit <- suppressWarnings(lda(NON_OPERATING ~ ., data=frame.train))
  lda.prob <- suppressWarnings(predict(lda.fit, newdata=frame.test, type = "response"))
  roc.lda <- simple_roc(frame.test$NON_OPERATING, unname(lda.prob$posterior[,2]))
  auc.lda <- auc(x = roc.lda$FPR, y = roc.lda$TPR)
  
  # GLM
  glm.fit <- suppressWarnings(glm(NON_OPERATING ~ ., data = frame.train, family = "binomial"))
  glm.prob <- suppressWarnings(predict(glm.fit, newdata=frame.test, type = "response"))
  roc.glm <- simple_roc(frame.test$NON_OPERATING, glm.prob)
  auc.glm <- auc(x = roc.glm$FPR, y = roc.glm$TPR)
  # It's interesting, that glm probabilities are quite low (<0.2), although ROC curve is not that bad 
  
  # Decision Tree
  tree.fit <- tree(NON_OPERATING ~ ., data = frame.train.factor)
  tree.pred <- predict(tree.fit, newdata=frame.test, type = "vector")
  roc.tree <- simple_roc(frame.test$NON_OPERATING, unname(tree.pred[,2]))
  auc.tree <- auc(x = roc.tree$FPR, y = roc.tree$TPR)

  # Bagging
  bag.fit <- randomForest(NON_OPERATING ~ ., data=frame.train.factor, importance=TRUE)
  bag.pred <- predict(bag.fit, newdata=frame.test, type = "prob")
  roc.bag <- simple_roc(frame.test$NON_OPERATING, unname(bag.pred[,2]))
  auc.bag <- auc(x = roc.bag$FPR, y = roc.bag$TPR)
  lines(x = roc.bag$FPR, y = roc.bag$TPR, col="black", type = "l", lwd=2)
  
  # Plotting
  par(mfrow=c(1,1))
  plot(x = roc.nb$FPR, y = roc.nb$TPR, main = title, col="green", xlab = "FPR", ylab = "TPR", type = "l", lwd=2)
  lines(x = roc.lda$FPR, y = roc.lda$TPR, col="red", type = "l", lwd=2)
  lines(x = roc.glm$FPR, y = roc.glm$TPR, col="blue", type = "l", lwd=2)
  lines(x = roc.tree$FPR, y = roc.tree$TPR, col="orange", type = "l", lwd=2)
  lines(x = roc.bag$FPR, y = roc.bag$TPR, col="black", type = "l", lwd=2)
  
  legend.nb <- paste("NB. AUC=", round(auc.nb, digits = 3), sep="")
  legend.lda <- paste("LDA. AUC=", round(auc.lda, digits = 3), sep="")
  legend.glm <- paste("GLM. AUC=", round(auc.glm, digits = 3), sep="")
  legend.tree <- paste("Tree. AUC=", round(auc.tree, digits = 3), sep="")
  legend.bag <- paste("Bag. AUC=", round(auc.bag, digits = 3), sep="")
  legend("bottomright",legend = c(legend.nb, legend.lda, legend.glm, legend.tree, legend.bag), col=c("green", "red", "blue", "orange", "black"),lty = 1,lwd = 2,cex = 0.6, x.intersp=1)
  text(0.8, 0.5, paste(colnames(frame.train), collapse="\n"))
  return(c(auc.nb, auc.lda, auc.glm, auc.tree))
}

merge_2_columns <- function(frame, first_col, second_col, result_col){
  frame[is.na(frame[,first_col]),first_col] = 0
  frame[is.na(frame[,second_col]),second_col] = 0
  frame[,result_col] <- frame[,first_col] + frame[,second_col]
  frame[frame[,result_col]==0,result_col] = NA
  frame[,first_col] <- NULL
  frame[,second_col] <- NULL
  return(frame)
}

merge_columns <- function(frame, columns, result_col){
  intermed_col = "INTERMED_COLUMN"
  intermed_col0 = paste(intermed_col, 0, sep = "")
  frame[, intermed_col0] = rep(NA, nrow(frame))
  for (column in columns){
    index <- which(columns == column)
    frame <- merge_2_columns(frame, column, paste(intermed_col, index-1, sep = ""), paste(intermed_col, index, sep = ""))
  }
  colnames(frame)[colnames(frame) == paste(intermed_col,length(columns),sep="")] <- result_col
  return(frame)
}

get_balanced_train_test_frames <- function(frame, train_balance_coef, train_test_coef){
  frame <- frame[sample(1:nrow(frame)),]
  test.ind <- 1:floor(nrow(frame)/(1+train_test_coef))
  train.ind <- setdiff(1:nrow(frame), test.ind)
  frame.test <- frame[test.ind,]
  frame.train <- frame[train.ind,]
  
  nonops.train <- (1:nrow(frame.train))[frame.train$NON_OPERATING == 1]
  ops.train <- (1:nrow(frame.train))[!frame.train$NON_OPERATING == 1]
  stopifnot(train_balance_coef*length(nonops.train) < length(ops.train))

  ops.train <- ops.train[1:(train_balance_coef*length(nonops.train))]
  frame.train <- frame.train[c(nonops.train, ops.train),]
  return(list(frame.train, frame.test))
}

plot_subset_sizes <- function(reg.summary){
  par(mfrow=c(2,2))
  par(mar=c(4,4,4,2))
  plot(reg.summary$rss, xlab ="Number of Variables", ylab ="RSS", type ="l")
  x.min.rss <- which.min(reg.summary$rss)
  points(x.min.rss, reg.summary$rss[x.min.rss], col="red", cex=2, pch=20)
  
  plot(reg.summary$adjr2, xlab ="Number of Variables", ylab ="Adjusted RSq", type ="l")
  x.max.adjr2 <- which.max(reg.summary$adjr2)
  points(x.max.adjr2, reg.summary$adjr2[x.max.adjr2], col="red", cex=2, pch=20)
  
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
  x.min.cp <- which.min(reg.summary$cp)
  points(x.min.cp, reg.summary$cp[x.min.cp], col="red", cex=2, pch=20)
  
  plot(reg.summary$bic, xlab ="Number of Variables", ylab ="BIC", type="l")
  x.min.bic <- which.min(reg.summary$bic)
  points (x.min.bic, reg.summary$bic[x.min.bic], col ="red",cex =2, pch =20)
  mtext("Choosing subset size:", side = 3, cex=1.3, line = -1.5, outer = TRUE)
  mtext(paste(colnames(frame.2013), collapse=" | "), side = 3, cex=.7, line = -3, outer = TRUE)
}


file.2013 <- "./data/MERGED2013_PP.csv"
file.2012 <- "./data/MERGED2012_PP.csv"
raw.2013 <- read.csv(file.2013, stringsAsFactors = FALSE)
#raw.2012 <- read.csv(file.2012, stringsAsFactors = FALSE)

features.2013 <- c("PAR_ED_PCT_HS", "TUITIONFEE_PROG", "TUITIONFEE_OUT", "COSTT4_A", "COSTT4_P", "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER", "NUM4_PUB", "NUM4_PRIV", "NUM4_PROG", "NUM4_OTHER", "TUITFTE", "INEXPFTE", "RET_PTL4", "RET_PT4", "RET_FTL4", "RET_FT4", "C150_4", "C150_L4", "C150_4_POOLED", "C150_L4_POOLED", "D150_4", "D150_L4", "D150_4_POOLED", "D150_L4_POOLED", "C150_4_AIAN", "C150_4_NHPI", "C150_4_2MOR", "C150_4_NRA", "C150_4_API", "C150_L4_NHPI", "C150_L4_2MOR", "C150_L4_NRA", "C150_4_POOLED_SUPP")
frame.2013 <- raw.2013[,features.2013]
frame.2013$NON_OPERATING <- ifelse(raw.2013[,"CURROPER"]==0, 1, 0)
frame.2013[,features.2013] <- suppressWarnings(lapply(frame.2013[,features.2013], as.numeric))

frame.2013 <- merge_columns(frame.2013, c("COSTT4_A", "COSTT4_P"), "COSTT4")
frame.2013 <- merge_columns(frame.2013, c("TUITIONFEE_PROG", "TUITIONFEE_OUT"), "TUITIONFEE")
frame.2013 <- merge_columns(frame.2013, c( "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER"), "NPT4")
frame.2013 <- merge_columns(frame.2013, c( "NUM4_PUB", "NUM4_PRIV", "NUM4_PROG", "NUM4_OTHER"), "NUM4")
frame.2013$REVENUE_EXPENDITURE <- frame.2013$TUITFTE - frame.2013$INEXPFTE
frame.2013$TUITFTE <- NULL
frame.2013$INEXPFTE <- NULL
frame.2013 <- merge_columns(frame.2013, c("RET_PTL4", "RET_PT4", "RET_FTL4", "RET_FT4"), "RET")
frame.2013 <- merge_columns(frame.2013, c("C150_4", "C150_L4", "C150_4_POOLED", "C150_L4_POOLED", "C150_4_AIAN", "C150_4_NHPI", "C150_4_2MOR", "C150_4_NRA", "C150_4_API", "C150_L4_NHPI", "C150_L4_2MOR", "C150_L4_NRA", "C150_4_POOLED_SUPP"), "C150")
frame.2013 <- merge_columns(frame.2013, c("D150_4", "D150_L4", "D150_4_POOLED", "D150_L4_POOLED"), "D150")
frame.2013 <- frame.2013[complete.cases(frame.2013),]

# Feature selection:
# 1. Boruta analysis (employs random forest approach)
boruta <- Boruta(NON_OPERATING ~ ., data = frame.2013, doTrace = 2)
print(boruta$finalDecision)

# 2. Choosing optimal subset size:
regfit.fwd <- regsubsets(NON_OPERATING ~ ., frame.2013, method="forward", nvmax=10)
reg.summary.fwd <- summary(regfit.fwd)
plot_subset_sizes(reg.summary.fwd)
cor(frame.2013[,colnames(frame.2013)!="NON_OPERATING"])

# 3. Rank features importance:
frame.2013.factor <- frame.2013
frame.2013.factor$NON_OPERATING <- as.factor(frame.2013$NON_OPERATING)
model <- suppressWarnings(train(NON_OPERATING~., data=frame.2013.factor, 'nb', trControl=trainControl(method='cv', number=10)))
importance <- varImp(model, scale=FALSE)
print(importance)

# 4. Removing correlated/not important features:
frame.2013$TUITIONFEE<-NULL
frame.2013$COSTT4<-NULL
frame.2013$NUM4<-NULL

train_test_frames.2013 <- get_balanced_train_test_frames(frame.2013, 2, 3)
frame.2013.train <- train_test_frames.2013[[1]]
frame.2013.test <- train_test_frames.2013[[2]]

# The best one at this moment:
compare_roc_curves(frame.2013.train, frame.2013.test, paste("Balance coef: ", 2, " Train size: ", nrow(frame.2013.train), " #Predictors: ", length(colnames(frame.2013.train))-1))



