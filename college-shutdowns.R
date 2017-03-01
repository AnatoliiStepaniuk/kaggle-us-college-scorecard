library(caret)
library(e1071)
library(MASS)
library(flux)
library(leaps)
library(Boruta)
library(tree)
library(randomForest)

# Constants
FPR_FROM = 0.0 
FPR_TO = 0.05
BUCKETS_NUM = 1000
AUC_DIGITS = 4
FOLDS_NUM = 10

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  scores <- scores[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels = labels, scores = scores )
}

plot_graphs <- function(roc.nb, roc.lda, roc.glm, roc.tree, roc.rf, title, colnames, xlim, ylim, pauc, legend.pos){
  plot(x = roc.nb$FPR, y = roc.nb$TPR, main = title, col="green", xlab = "FPR", ylab = "TPR", xlim = xlim, ylim = ylim, type = "l", lwd=2)
  lines(x = roc.lda$FPR, y = roc.lda$TPR, col="red", type = "l", lwd=2)
  lines(x = roc.glm$FPR, y = roc.glm$TPR, col="blue", type = "l", lwd=2)
  lines(x = roc.tree$FPR, y = roc.tree$TPR, col="orange", type = "l", lwd=2)
  lines(x = roc.rf$FPR, y = roc.rf$TPR, col="black", type = "l", lwd=2)
  
  text(0.8, 0.5, paste(colnames[colnames != "NON_OPERATING"], collapse="\n"))
  lines(x=c(FPR_FROM, FPR_FROM), y=c(0,0.6), type = "l", lty="dashed", col="red")
  lines(x=c(FPR_TO, FPR_TO), y=c(0,0.6), type = "l", lty="dashed", col="red")

    if(pauc){
    legend.nb <- paste("NB. AUC=", round(roc.nb$pauc[1], digits = AUC_DIGITS), sep="")
    legend.lda <- paste("LDA. AUC=", round(roc.lda$pauc[1], digits = AUC_DIGITS), sep="")
    legend.glm <- paste("GLM. AUC=", round(roc.glm$pauc[1], digits = AUC_DIGITS), sep="")
    legend.tree <- paste("Tree. AUC=", round(roc.tree$pauc[1], digits = AUC_DIGITS), sep="")
    legend.rf <- paste("RF. AUC=", round(roc.rf$pauc[1], digits = AUC_DIGITS), sep="")
  }
  else{
    legend.nb <- paste("NB. AUC=", round(roc.nb$auc[1], digits = AUC_DIGITS), sep="")
    legend.lda <- paste("LDA. AUC=", round(roc.lda$auc[1], digits = AUC_DIGITS), sep="")
    legend.glm <- paste("GLM. AUC=", round(roc.glm$auc[1], digits = AUC_DIGITS), sep="")
    legend.tree <- paste("Tree. AUC=", round(roc.tree$auc[1], digits = AUC_DIGITS), sep="")
    legend.rf <- paste("RF. AUC=", round(roc.rf$auc[1], digits = AUC_DIGITS), sep="")
  }
  legend(legend.pos,legend = c(legend.nb, legend.lda, legend.glm, legend.tree, legend.rf), col=c("green", "red", "blue", "orange", "black"),lty = 1,lwd = 2,cex = 0.6, x.intersp=1)
}

compare_roc_curves_cv <- function(frame.folds, title){
  set.seed(1)

  # Naive Bayes
  roc.nb <- list()
  roc.nb.buckets <- list()
  for(i in 1:length(frame.folds)){
    nb.model <- suppressWarnings(train(factor(NON_OPERATING) ~ ., data=frame.folds[[i]]$train, 'nb', trControl=trainControl(method='cv', number=10)))
    nb.pred <- suppressWarnings(predict(nb.model$finalModel, newdata=frame.folds[[i]]$test))
    roc.nb[[i]] <- simple_roc(frame.folds[[i]]$test$NON_OPERATING, unname(nb.pred$posterior[,2]))
    roc.nb.buckets[[i]] <- divide_roc_data_buckets(roc.nb[[i]], BUCKETS_NUM)
  }
  roc.nb <- get_roc_mean(roc.nb.buckets)
  roc.nb$pauc <- partial_auc(roc.nb, FPR_FROM, FPR_TO) 

  # LDA
  roc.lda <- list()
  roc.lda.buckets <- list()
  for(i in 1:length(frame.folds)){
    lda.fit <- suppressWarnings(lda(NON_OPERATING ~ ., data=frame.folds[[i]]$train))
    lda.prob <- suppressWarnings(predict(lda.fit, newdata=frame.folds[[i]]$test))
    roc.lda[[i]] <- simple_roc(frame.folds[[i]]$test$NON_OPERATING, unname(lda.prob$posterior[,2]))
    roc.lda.buckets[[i]] <- divide_roc_data_buckets(roc.lda[[i]], BUCKETS_NUM)
  }
  roc.lda <- get_roc_mean(roc.lda.buckets)
  roc.lda$pauc <- partial_auc(roc.lda, FPR_FROM, FPR_TO) 
  
  # GLM
  roc.glm <- list()
  roc.glm.buckets <- list()
  for(i in 1:length(frame.folds)){
    glm.fit <- suppressWarnings(glm(NON_OPERATING ~ ., data = frame.folds[[i]]$train, family = "binomial"))
    glm.prob <- suppressWarnings(predict(glm.fit, newdata=frame.folds[[i]]$test, type = "response"))
    roc.glm[[i]] <- simple_roc(frame.folds[[i]]$test$NON_OPERATING, glm.prob)
    roc.glm.buckets[[i]] <- divide_roc_data_buckets(roc.glm[[i]], BUCKETS_NUM)
  }
  roc.glm <- get_roc_mean(roc.glm.buckets)
  roc.glm$pauc <- partial_auc(roc.glm, FPR_FROM, FPR_TO) 
  
  # Decision Tree
  roc.tree <- list()
  roc.tree.buckets <- list()
  for(i in 1:length(frame.folds)){
    tree.fit <- tree(factor(NON_OPERATING) ~ ., data = frame.folds[[i]]$train)
    tree.pred <- predict(tree.fit, newdata=frame.folds[[i]]$test, type = "vector")
    roc.tree[[i]] <- simple_roc(frame.folds[[i]]$test$NON_OPERATING, unname(tree.pred[,2]))
    roc.tree.buckets[[i]] <- divide_roc_data_buckets(roc.tree[[i]], BUCKETS_NUM)
  }
  roc.tree <- get_roc_mean(roc.tree.buckets)
  roc.tree$pauc <- partial_auc(roc.tree, FPR_FROM, FPR_TO) 
   
  # Random Forest 
  roc.rf <- list()
  roc.rf.buckets <- list()
  for(i in 1:length(frame.folds)){
    rf.fit <- randomForest(factor(NON_OPERATING) ~ ., data=frame.folds[[i]]$train, importance=TRUE)
    rf.pred <- predict(rf.fit, newdata=frame.folds[[i]]$test, type = "prob")
    roc.rf[[i]] <- simple_roc(frame.folds[[i]]$test$NON_OPERATING, unname(rf.pred[,2]))
    roc.rf.buckets[[i]] <- divide_roc_data_buckets(roc.rf[[i]], BUCKETS_NUM)
  }
  roc.rf <- get_roc_mean(roc.rf.buckets)
  roc.rf$pauc <- partial_auc(roc.rf, FPR_FROM, FPR_TO) 
  
  # Plotting
  par(mfrow=c(1,2))
  plot_graphs(roc.nb, roc.lda, roc.glm, roc.tree, roc.rf, title, colnames(frame.folds[[1]]$test), xlim = c(0,1), ylim = c(0,1), FALSE, "bottomright")
  plot_graphs(roc.nb, roc.lda, roc.glm, roc.tree, roc.rf, title, colnames(frame.folds[[1]]$test), xlim = c(FPR_FROM,1.1*FPR_TO), ylim = c(-0.2,0.7), TRUE, "topright")

  return(c(roc.nb$pauc[1], roc.lda$pauc[1], roc.glm$pauc[1], roc.tree$pauc[1], roc.rf$pauc[1]))
}

get_roc_mean <- function(rocs){
  roc.avg <- data.frame(TPR=rep(NA, nrow(rocs[[1]])), FPR = rocs[[1]]$FPR)
  TPRs <- list()
  for(i in 1:length(rocs)){
    TPRs[[i]] <- rocs[[i]]$TPR
  }
  roc.avg$TPR <- rowMeans(data.frame(TPRs))
  return(roc.avg)
}

partial_auc <- function(roc, fpr.from, fpr.to){
  ind <- roc$FPR >= fpr.from & roc$FPR <= fpr.to
  auc = 0
  for(i in 1:(nrow(roc[ind,])-1)){
    auc <- auc + roc$TPR[i] * (roc$FPR[i+1] - roc$FPR[i])
  }
  return(auc)  
}

compare_roc_curves_imbalanced_cv <- function(frame, folds_num, title){
  set.seed(1)
  folds <- createFolds(1:nrow(frame), k=folds_num, list=TRUE, returnTrain = TRUE)
  
  # Naive Bayes
  roc.nb <- list()
  roc.nb.buckets <- list()
  for(i in 1:folds_num){
    nb.model <- suppressWarnings(train(factor(NON_OPERATING) ~ ., data=frame[folds[[i]],], 'nb', trControl=trainControl(method='cv', number=10)))
    nb.pred <- suppressWarnings(predict(nb.model$finalModel, newdata=frame[-folds[[i]],]))
    roc.nb[[i]] <- simple_roc(frame[-folds[[i]],]$NON_OPERATING, unname(nb.pred$posterior[,2]))
    roc.nb.buckets[[i]] <- divide_roc_data_buckets(roc.nb[[i]], BUCKETS_NUM)
  }
  roc.nb <- get_roc_mean(roc.nb.buckets)
  roc.nb$pauc <- partial_auc(roc.nb, FPR_FROM, FPR_TO) 
  roc.nb$auc <- partial_auc(roc.nb, FPR_FROM, 1) 
  
  # LDA
  roc.lda <- list()
  roc.lda.buckets <- list()
  for(i in 1:folds_num){
    lda.fit <- suppressWarnings(lda(factor(NON_OPERATING) ~ ., data=frame[folds[[i]],]))
    lda.prob <- suppressWarnings(predict(lda.fit, newdata=frame[-folds[[i]],]))
    roc.lda[[i]] <- simple_roc(frame[-folds[[i]],]$NON_OPERATING, unname(lda.prob$posterior[,2]))
    roc.lda.buckets[[i]] <- divide_roc_data_buckets(roc.lda[[i]], BUCKETS_NUM)
  }
  roc.lda <- get_roc_mean(roc.lda.buckets)
  roc.lda$pauc <- partial_auc(roc.lda, FPR_FROM, FPR_TO) 
  roc.lda$auc <- partial_auc(roc.lda, FPR_FROM, 1) 
  
  # GLM
  roc.glm <- list()
  roc.glm.buckets <- list()
  for(i in 1:folds_num){
    glm.fit <- suppressWarnings(glm(factor(NON_OPERATING) ~ ., data = frame[folds[[i]],], family = "binomial"))
    glm.prob <- suppressWarnings(predict(glm.fit, newdata=frame[-folds[[i]],], type = "response"))
    roc.glm[[i]] <- simple_roc(frame[-folds[[i]],]$NON_OPERATING, glm.prob)
    roc.glm.buckets[[i]] <- divide_roc_data_buckets(roc.glm[[i]], BUCKETS_NUM)
  }
  roc.glm <- get_roc_mean(roc.glm.buckets)
  roc.glm$auc <- partial_auc(roc.glm, FPR_FROM, 1) 
  roc.glm$pauc <- partial_auc(roc.glm, FPR_FROM, FPR_TO) 
  
  # Decision Tree
  roc.tree <- list()
  roc.tree.buckets <- list()
  for(i in 1:folds_num){
    tree.fit <- tree(factor(NON_OPERATING) ~ ., data = frame[folds[[i]],])
    tree.pred <- predict(tree.fit, newdata=frame[-folds[[i]],], type = "vector")
    roc.tree[[i]] <- simple_roc(frame[-folds[[i]],]$NON_OPERATING, unname(tree.pred[,2]))
    roc.tree.buckets[[i]] <- divide_roc_data_buckets(roc.tree[[i]], BUCKETS_NUM)
  }
  roc.tree <- get_roc_mean(roc.tree.buckets)
  roc.tree$auc <- partial_auc(roc.tree, FPR_FROM, 1) 
  roc.tree$pauc <- partial_auc(roc.tree, FPR_FROM, FPR_TO) 
  
  # Random Forest 
  roc.rf <- list()
  roc.rf.buckets <- list()
  for(i in 1:folds_num){
    rf.fit <- randomForest(factor(NON_OPERATING) ~ ., data=frame[folds[[i]],], importance=TRUE)
    rf.pred <- predict(rf.fit, newdata=frame[-folds[[i]],], type = "prob")
    roc.rf[[i]] <- simple_roc(frame[-folds[[i]],]$NON_OPERATING, unname(rf.pred[,2]))
    roc.rf.buckets[[i]] <- divide_roc_data_buckets(roc.rf[[i]], BUCKETS_NUM)
  }
  roc.rf <- get_roc_mean(roc.rf.buckets)
  roc.rf$auc <- partial_auc(roc.rf, FPR_FROM, 1) 
  roc.rf$pauc <- partial_auc(roc.rf, FPR_FROM, FPR_TO) 

  # Plotting
  par(mfrow=c(1,2))
  plot_graphs(roc.nb, roc.lda, roc.glm, roc.tree, roc.rf, title, colnames(frame), xlim = c(0,1), ylim = c(0,1), FALSE, "bottomright")
  plot_graphs(roc.nb, roc.lda, roc.glm, roc.tree, roc.rf, title, colnames(frame), xlim = c(FPR_FROM,1.1*FPR_TO), ylim = c(0,0.7), TRUE, "topright")
  
  return(c(roc.nb$pauc[1], roc.lda$pauc[1], roc.glm$pauc[1], roc.tree$pauc[1], roc.rf$pauc[1]))
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
  
  nonops.train.ind <- (1:nrow(frame.train))[frame.train$NON_OPERATING == 1]
  ops.train.ind <- (1:nrow(frame.train))[!frame.train$NON_OPERATING == 1]
  stopifnot(train_balance_coef*length(nonops.train.ind) < length(ops.train.ind))

  ops.train.ind <- ops.train.ind[1:(train_balance_coef*length(nonops.train.ind))]
  frame.train <- frame.train[c(nonops.train.ind, ops.train.ind),]
  return(list(frame.train, frame.test))
}

get_balanced_train_test_frames_cv <- function(frame, folds_num, train_balance_coef){
  frames.out = list()  
  folds.train <- createFolds(1:nrow(frame), k=folds_num, list=TRUE, returnTrain = TRUE)
  folds.test = list()
  for(i in 1:folds_num){
    folds.test[[i]] <- setdiff(1:nrow(frame), folds.train[[i]])
  }

  for(i in 1:folds_num){
    frame.test <- frame[folds.test[[i]],]
    frame.train <- frame[folds.train[[i]],]
    
    nonops.train.ind <- (1:nrow(frame.train))[frame.train$NON_OPERATING == 1]
    ops.train.ind <- (1:nrow(frame.train))[!frame.train$NON_OPERATING == 1]
    stopifnot(train_balance_coef*length(nonops.train.ind) < length(ops.train.ind))
    
    ops.train.ind <- ops.train.ind[1:(train_balance_coef*length(nonops.train.ind))]
    frame.train <- frame.train[c(nonops.train.ind, ops.train.ind),]
  
    frames.out[[i]] <- list(train=frame.train, test=frame.test)
  }

  return(frames.out)
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

draw_feature_selection_plots <- function(frame){
  # 1. Boruta analysis (employs random forest approach)
  boruta <- Boruta(NON_OPERATING ~ ., data = frame, doTrace = 2)
  print(boruta$finalDecision)
  
  # 2. Choosing optimal subset size:
  regfit.fwd <- regsubsets(NON_OPERATING ~ ., frame, method="forward", nvmax=10)
  reg.summary.fwd <- summary(regfit.fwd)
  plot_subset_sizes(reg.summary.fwd)
  print(reg.summary.fwd)
  print(cor(frame))
  
  # 3. Rank features importance:
  frame.factor <- frame
  frame.factor$NON_OPERATING <- as.factor(frame$NON_OPERATING)
  model <- suppressWarnings(train(NON_OPERATING~., data=frame.factor, 'nb', trControl=trainControl(method='cv', number=10)))
  importance <- varImp(model, scale=FALSE)
  print(importance)
}

divide_roc_data_buckets <- function(roc, n_buckets){
  buckets <- seq(from=0, to=1.00-1.00/n_buckets, length.out = n_buckets)
  roc.out <- data.frame(TPR=rep(NA,n_buckets), FPR=buckets)
  
  for(x in buckets){
    indices <- roc$FPR >= x & roc$FPR < x+1.00/n_buckets
    roc.out$TPR[which(buckets == x)] <- mean(roc$TPR[indices])
  }
  roc.out$TPR <- fill_NAs_with_neighbours(roc.out$TPR)
  return(roc.out)
}

fill_NAs_with_neighbours <- function(list){
  for(i in 2:length(list)){
    if(is.na(list[i]))
      list[i] = list[i-1]
  }
  
  for(i in (length(list)-1):1){
    if(is.na(list[i])){
      list[i] = list[i+1]
    }    
  }
  return(list)  
}

get_prob_threshold <- function(roc, fpr_to){
  return(min(roc[roc$FPR<fpr_to,]$scores))
}

choose_RF_threshold <- function(frame, folds_num, fpr_to){
  folds <- createFolds(1:nrow(frame), k=folds_num, list=TRUE, returnTrain = TRUE)
  thresholds <- rep(NA, folds_num)
  for(i in 1:folds_num){
    rf.fit <- randomForest(factor(NON_OPERATING) ~ ., data=frame[folds[[i]],], importance=TRUE)
    rf.pred <- predict(rf.fit, newdata=frame[-folds[[i]],], type = "prob")
    roc.rf <- simple_roc(frame[-folds[[i]],]$NON_OPERATING, unname(rf.pred[,2]))
    thresholds[i] <- get_prob_threshold(roc.rf, fpr_to)
  }
  return(mean(thresholds))
}

estimate_RF_performance <- function(frame, folds_num, fpr_rate){
  folds <- createFolds(1:nrow(frame), k=folds_num, list=TRUE, returnTrain = TRUE)
  thresholds <- rep(NA, folds_num)
  fprs <- list()
  tprs <- list()
  precisions <- list()
  for(i in 1:folds_num){
    rf.threshold <- choose_RF_threshold(frame[folds[[i]],], folds_num, fpr_rate)
    test.model <- randomForest(factor(NON_OPERATING) ~ ., data=frame[folds[[i]],], importance=TRUE)
    test.prob <- unname(predict(test.model, newdata=frame[-folds[[i]],], type = "prob")[,2])
    test.pred <- ifelse(test.prob > rf.threshold, 1, 0)
    test.resp <- frame[-folds[[i]],]$NON_OPERATING
    precisions[[i]] <- sum(test.resp == 1 & test.pred == 1)/sum(test.pred==1)
    tprs[[i]] <- sum(test.resp == 1 & test.pred == 1)/sum(test.resp==1)
    fprs[[i]] <- sum(test.resp == 0 & test.pred == 1)/sum(test.resp==0)
  }
  
  precision <- mean(unlist(precisions))
  tpr <- mean(unlist(tprs))
  fpr <- mean(unlist(fprs))
  return(list(precision=precision, tpr=tpr, fpr=fpr))
}

feature_engineering_and_selection <- function(raw){
  features <- c("PAR_ED_PCT_PS", "TUITIONFEE_PROG", "TUITIONFEE_OUT", "COSTT4_A", "COSTT4_P", "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER", "NUM4_PUB", "NUM4_PRIV", "NUM4_PROG", "NUM4_OTHER", "TUITFTE", "INEXPFTE", "RET_PTL4", "RET_PT4", "RET_FTL4", "RET_FT4", "C150_4", "C150_L4", "C150_4_POOLED", "C150_L4_POOLED", "D150_4", "D150_L4", "D150_4_POOLED", "D150_L4_POOLED", "C150_4_AIAN", "C150_4_NHPI", "C150_4_2MOR", "C150_4_NRA", "C150_4_API", "C150_L4_NHPI", "C150_L4_2MOR", "C150_L4_NRA", "C150_4_POOLED_SUPP")
  frame <- raw[,features]
  frame$NON_OPERATING <- ifelse(raw.2013[,"CURROPER"]==0, 1, 0)
  frame[,features] <- suppressWarnings(lapply(frame[,features], as.numeric))
  
  frame <- merge_columns(frame, c("COSTT4_A", "COSTT4_P"), "COSTT4")
  frame <- merge_columns(frame, c("TUITIONFEE_PROG", "TUITIONFEE_OUT"), "TUITIONFEE")
  frame <- merge_columns(frame, c( "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER"), "NPT4")
  frame <- merge_columns(frame, c( "NUM4_PUB", "NUM4_PRIV", "NUM4_PROG", "NUM4_OTHER"), "NUM4")
  frame$REVENUE_EXPENDITURE <- frame$TUITFTE - frame$INEXPFTE
  frame$TUITFTE <- NULL
  frame$INEXPFTE <- NULL
  frame <- merge_columns(frame, c("RET_PTL4", "RET_PT4", "RET_FTL4", "RET_FT4"), "RET")
  frame <- merge_columns(frame, c("C150_4", "C150_L4", "C150_4_POOLED", "C150_L4_POOLED", "C150_4_AIAN", "C150_4_NHPI", "C150_4_2MOR", "C150_4_NRA", "C150_4_API", "C150_L4_NHPI", "C150_L4_2MOR", "C150_L4_NRA", "C150_4_POOLED_SUPP"), "C150")
  frame <- merge_columns(frame, c("D150_4", "D150_L4", "D150_4_POOLED", "D150_L4_POOLED"), "D150")
  frame <- frame[complete.cases(frame),]
  
  # Eliminated, since I've focused on lowest 5% AUC instead of global parameters like RSS or R-squared:
  #draw_feature_selection_plots(frame.train)
  
  # Removing features according to lowest 5% AUC measurements:
  frame$COSTT4<-NULL
  frame$RET<-NULL
  
  return(frame)
}

file.2013 <- "./data/MERGED2013_PP.csv"
file.2012 <- "./data/MERGED2012_PP.csv"
raw.2013 <- read.csv(file.2013, stringsAsFactors = FALSE)
#raw.2012 <- read.csv(file.2012, stringsAsFactors = FALSE)

frame.2013 <- feature_engineering_and_selection(raw.2013)

# Estimating feature importance
par(mfrow=c(1,1))
rf.fit <- randomForest(factor(NON_OPERATING) ~ ., data=frame.2013, keep.forest=FALSE, importance=TRUE)
varImpPlot(rf.fit, pt.cex=1.5, main = "Random Forest feature importance")
importance(rf.fit)

# It turned out Random Forest performs better on imbalanced data, so no additional balancing is needed:
#train_test_frames_folds.2013 <- get_balanced_train_test_frames_cv(frame.2013, 10, 25)
#aucs <- compare_roc_curves_cv(train_test_frames_folds.2013, paste("Train 1:25, Folds = 10"))

aucs <- compare_roc_curves_imbalanced_cv(frame.2013, FOLDS_NUM, paste("Inbalanced. CV", " Train size: ", floor((9/10)*nrow(frame.2013)), " #Predictors: ", length(colnames(frame.2013))-1))

# Estimating final performance 
performance <- estimate_RF_performance(frame.2013, FOLDS_NUM, FPR_TO)
performance$precision
performance$tpr
performance$fpr
