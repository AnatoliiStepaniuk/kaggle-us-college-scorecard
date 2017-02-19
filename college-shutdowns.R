library(caret)
library(e1071)
library(MASS)
library(flux)
set.seed(1)

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

compare_roc_curves <- function(frame, features, title){
  data <- frame[, c("NON_OPERATING", features)]

  # Naive Bayes
  X <- data[,features]
  Y <- as.factor(data$NON_OPERATING)
  model <- suppressWarnings(train(X, Y, 'nb', trControl=trainControl(method='cv', number=10)))
  nb.pred <- suppressWarnings(predict(model$finalModel, X))
  roc.nb <- simple_roc(frame[,"NON_OPERATING"], unname(nb.pred$posterior[,2]))
  auc.nb <- auc(x = roc.nb$FPR, y = roc.nb$TPR)
  
  # LDA
  lda.fit.cv <- suppressWarnings(lda(NON_OPERATING ~ ., data=data, CV=TRUE))
  roc.lda <- simple_roc(data$NON_OPERATING, unname(lda.fit.cv$posterior[,2]))
  auc.lda <- auc(x = roc.lda$FPR, y = roc.lda$TPR)
  
  # GLM
  glm.fit <- suppressWarnings(glm(NON_OPERATING ~ ., data = data, family = "binomial"))
  glm.prob <- predict(glm.fit, data, type = "response")
  roc.glm <- simple_roc(data$NON_OPERATING, glm.prob)
  auc.glm <- auc(x = roc.glm$FPR, y = roc.glm$TPR)
  # It's interesting, that glm probabilities are quite low (<0.2), although ROC curve is not that bad 
  
  
  # Plotting
  plot(x = roc.nb$FPR, y = roc.nb$TPR, main = title, col="green", xlab = "FPR", ylab = "TPR", type = "l", lwd=2)
  lines(x = roc.lda$FPR, y = roc.lda$TPR, col="red", type = "l", lwd=2)
  lines(x = roc.glm$FPR, y = roc.glm$TPR, col="blue", type = "l", lwd=2)
  
  legend.nb <- paste("Naive Bayes. AUC=", round(auc.nb, digits = 3), sep="")
  legend.lda <- paste("LDA. AUC=", round(auc.lda, digits = 3), sep="")
  legend.glm <- paste("GLM. AUC=", round(auc.glm, digits = 3), sep="")
  legend("bottomright",legend = c(legend.nb, legend.lda, legend.glm), col=c("green", "red", "blue"),lty = 1,lwd = 2,cex = 0.6, x.intersp=1)
  
  return(c(auc.nb, auc.lda, auc.glm))  
}

path_to_file_2013 <- "./data/MERGED2013_PP.csv"
raw_data_2013 <- read.csv(path_to_file_2013, stringsAsFactors = FALSE)

# Feature set #1 (Program percentages)
fs1.2013 <- c("PCIP01", "PCIP03", "PCIP04", "PCIP05", "PCIP09", "PCIP10", "PCIP11", "PCIP12", "PCIP13", "PCIP14", "PCIP15", "PCIP16", "PCIP19", "PCIP22", "PCIP23", "PCIP24", "PCIP25", "PCIP26", "PCIP27", "PCIP29", "PCIP30", "PCIP31", "PCIP38", "PCIP39", "PCIP40", "PCIP41", "PCIP42", "PCIP43", "PCIP44", "PCIP45", "PCIP46", "PCIP47", "PCIP48", "PCIP49", "PCIP50", "PCIP51", "PCIP52", "PCIP54")
frame1.2013 <- raw_data_2013[,c("ï.¿UNITID", "INSTNM", fs1.2013)]
colnames(frame1.2013)[1] <- "ID"
frame1.2013$NON_OPERATING <- ifelse(raw_data_2013[,"CURROPER"]==0, 1, 0)
frame1.2013[,fs1.2013] <- suppressWarnings(lapply(frame1.2013[,fs1.2013], as.numeric))
frame1.2013 <- frame1.2013[complete.cases(frame1.2013),]

# Feature set #2 (EXPLORE THIS - what feature have the biggest impact?)
fs2.2013 <- c("DISTANCEONLY", "TUITIONFEE_IN", "TUITIONFEE_OUT", "TUITFTE", "INEXPFTE", "AVGFACSAL", "PFTFAC", "PAR_ED_PCT_MS", "PAR_ED_PCT_HS", "PAR_ED_PCT_PS")
frame2.2013 <- raw_data_2013[,c("ï.¿UNITID", "INSTNM", fs2.2013)]
colnames(frame2.2013)[1] <- "ID"
frame2.2013$NON_OPERATING <- ifelse(raw_data_2013[,"CURROPER"]==0, 1, 0)
frame2.2013[,fs2.2013] <- suppressWarnings(lapply(frame2.2013[,fs2.2013], as.numeric))
frame2.2013 <- frame2.2013[complete.cases(frame2.2013),]

# Feature set #3 (Admission/enrollment)
#features.2013 <- c("ADM_RATE", "ADM_RATE_ALL", "UGDS")
# Feature set #4 (Cost)
#features.2013 <- c("TUITIONFEE_PROG", "TUITIONFEE_OUT", "TUITIONFEE_IN", "COSTT4_A", "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER", "NPT41_PUB", "NPT42_PUB", "NPT43_PUB", "NPT44_PUB", "NPT45_PUB", "NPT41_PRIV", "NPT42_PRIV", "NPT43_PRIV", "NPT44_PRIV", "NPT45_PRIV", "NPT41_PROG", "NPT42_PROG", "NPT43_PROG", "NPT44_PROG", "NPT45_PROG", "NPT41_OTHER", "NPT42_OTHER", "NPT43_OTHER", "NPT44_OTHER", "NPT45_OTHER", "NPT4_048_PUB", "NPT4_048_PRIV", "NPT4_048_PROG", "NPT4_048_OTHER", "NPT4_3075_PUB", "NPT4_3075_PRIV", "NPT4_75UP_PUB", "NPT4_75UP_PRIV", "NPT4_3075_PROG", "NPT4_3075_OTHER", "NPT4_75UP_PROG", "NPT4_75UP_OTHER", "NUM4_PUB", "NUM4_PRIV", "NUM4_PROG", "NUM4_OTHER", "NUM41_PUB", "NUM42_PUB", "NUM43_PUB", "NUM44_PUB", "NUM45_PUB", "NUM41_PRIV", "NUM42_PRIV", "NUM43_PRIV", "NUM44_PRIV", "NUM45_PRIV", "NUM41_PROG", "NUM42_PROG", "NUM43_PROG", "NUM44_PROG", "NUM45_PROG", "NUM41_OTHER", "NUM42_OTHER", "NUM43_OTHER", "NUM44_OTHER", "NUM45_OTHER")
# Feature set #5 (School)
#features.2013 <- c("TUITFTE", "INEXPFTE", "AVGFACSAL", "PFTFAC")
# Feature set #5 (Completion/Retention)
#features.2013 <- c("RET_PTL4", "RET_PT4", "RET_FTL4", "RET_FT4", "C150_4", "C150_L4", "C150_4_POOLED", "C150_L4_POOLED", "D150_4", "D150_L4", "D150_4_POOLED", "D150_L4_POOLED", "C150_4_AIAN", "C150_4_NHPI", "C150_4_2MOR", "C150_4_NRA", "C150_4_API", "C150_L4_NHPI", "C150_L4_2MOR", "C150_L4_NRA", "C150_L4_POOLED_SUP", "C150_4_POOLED_SUPP", "C200_L4_POOLED_SUP", "C200_4_POOLED_SUPP")

compare_roc_curves(frame1.2013, fs1.2013, "FS#1 (Program percentages)")
compare_roc_curves(frame2.2013, fs2.2013, "FS#2 (NEEDS ANALYSIS)")