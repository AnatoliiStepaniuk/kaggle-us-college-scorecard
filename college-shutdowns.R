library("caret")
library("e1071")
library("MASS")
library("boot")
set.seed(1)

getGlmError <- function(train, test){
  glm.fit <- glm(NON_OPERATING ~ ., data = train, family = "binomial")
  glm.probs <- predict(glm.fit, test)
  glm.pred <- rep(times = nrow(test), 0)
  glm.pred[glm.probs > 0.5] <- 1
  glm.error <- mean(glm.pred != test$NON_OPERATING)
  return(c(glm.error,glm.probs))
}

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

path_to_file_2013 <- "./data/MERGED2013_PP.csv"
#raw_data_2013 <- read.csv(path_to_file_2013, stringsAsFactors = FALSE)

#features.2013 <- c("PCIP01", "PCIP03", "PCIP04", "PCIP05", "PCIP09", "PCIP10", "PCIP11", "PCIP12", "PCIP13", "PCIP14", "PCIP15", "PCIP16", "PCIP19", "PCIP22", "PCIP23", "PCIP24", "PCIP25", "PCIP26", "PCIP27", "PCIP29", "PCIP30", "PCIP31", "PCIP38", "PCIP39", "PCIP40", "PCIP41", "PCIP42", "PCIP43", "PCIP44", "PCIP45", "PCIP46", "PCIP47", "PCIP48", "PCIP49", "PCIP50", "PCIP51", "PCIP52", "PCIP54")
features.2013 <- c("DISTANCEONLY", "TUITIONFEE_IN", "TUITIONFEE_OUT", "TUITFTE", "INEXPFTE", "AVGFACSAL", "PFTFAC", "PAR_ED_PCT_MS", "PAR_ED_PCT_HS", "PAR_ED_PCT_PS")

frame.2013 <- raw_data_2013[,c("ï.¿UNITID", "INSTNM", features.2013)]
colnames(frame.2013)[1] <- "ID"
frame.2013$NON_OPERATING <- ifelse(raw_data_2013[,"CURROPER"]==0, 1, 0)
frame.2013[,features.2013] <- suppressWarnings(lapply(frame.2013[,features.2013], as.numeric))
frame.2013 <- frame.2013[complete.cases(frame.2013),]
data.2013 <- frame.2013[, c("NON_OPERATING", features.2013)]

# Naive Bayes attempt
X <- data.2013[,features.2013]
Y <- as.factor(data.2013$NON_OPERATING)
model <- suppressWarnings(train(X, Y, 'nb', trControl=trainControl(method='cv', number=10)))
nb.pred <- suppressWarnings(predict(model$finalModel, X))

roc.nb <- simple_roc(frame.2013[,"NON_OPERATING"], unname(nb.pred$posterior[,2]))
plot(x = roc.nb$FPR, y = roc.nb$TPR, main = "Comparing ROC curves", col="green", xlab = "FPR", ylab = "TPR", type = "l", lwd=2)

# LDA
lda.fit.cv <- suppressWarnings(lda(NON_OPERATING ~ ., data=data.2013, CV=TRUE))
roc.lda <- simple_roc(data.2013$NON_OPERATING, unname(lda.fit.cv$posterior[,2]))
lines(x = roc.lda$FPR, y = roc.lda$TPR, col="red", main = "LDA ROC(with CV)", xlab = "FPR", ylab = "TPR", type = "l", lwd=2)

# GLM
glm.fit <- glm(NON_OPERATING ~ ., data = data.2013, family = "binomial")
glm.prob <- predict(glm.fit, data.2013, type = "response")
roc.glm <- simple_roc(data.2013$NON_OPERATING, glm.prob)
lines(x = roc.glm$FPR, y = roc.glm$TPR, col="blue", type = "l", lwd=2)
# It's interesting, that glm probabilities are quite low (<0.2), although ROC curve is not that bad 

legend("bottomright",legend = c("Naive Bayes", "LDA", "GLM"), col=c("green", "red", "blue"),lty = 1,lwd = 2,cex = 0.8)





