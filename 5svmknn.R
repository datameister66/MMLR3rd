library(magrittr)
install.packages("glmnet")
install.packages("caret")
install.packages("classifierplots")
install.packages("InformationValue")
install.packages("Metrics")
install.packages("ROCR")
install.packages("tidyverse")
options(scipen=999)

# santander_prepd <- read.csv("C:/Users/cory/Desktop/data/santander_prepd.csv")

download.file('https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/a5a',
              'chap5')
svmguide1 = read.libsvm('chap5') # see functin below

mydf <- e1071::read.matrix.csr("chap5")
y <- mydf$y
y <- ifelse(y == "+1", 1, 0)
table(y)
x <- as.matrix(mydf$x)

wtf <- svmguide1@x

vsplit <- function(v, n) {
  l = length(v)
  r = l/n
  return(lapply(1:n, function(i) {
    s = max(1, round(r*(i-1))+1)
    e = min(l, round(r*i))
    return(v[s:e])
  }))
}

df <- vsplit(wtf, 5)

n <- length(df[[1]])
df1 <- structure(df, row.names = c(NA, -n), .Names = paste("v", seq_along(df), sep = ""), class = "data.frame")

table(df1$v1)

read.libsvm = function( filename ) {
  content = readLines( filename )
  num_lines = length( content )
  tomakemat = cbind(1:num_lines, -1, substr(content,1,1))
  
  # loop over lines
  makemat = rbind(tomakemat,
                  do.call(rbind, 
                          lapply(1:num_lines, function(i){
                            # split by spaces, remove lines
                            line = as.vector( strsplit( content[i], ' ' )[[1]])
                            cbind(i, t(simplify2array(strsplit(line[-1],
                                                               ':'))))   
                          })))
  class(makemat) = "numeric"
  
  #browser()
  yx = sparseMatrix(i = makemat[,1], 
                    j = makemat[,2]+2, 
                    x = makemat[,3])
  return( yx )
}


y_factor <- as.factor(y)
table(y_factor)
# x <- df1[, -1]
x <- data.frame(x) # do some preprocess

xNZV <- caret::nearZeroVar(x, saveMetrics = TRUE)
table(xNZV$nzv)
table(xNZV$zeroVar)

# eliminate the NZV, explore the number of unique

set.seed(654321)

ctrl <- caret::rfeControl(functions = caret::lrFuncs,
                          method = "cv",
                          
                          verbose = TRUE)

subsets <- c(2:3)

lmProfile <- caret::rfe(x, y_factor,
                        sizes = subsets,
                        rfeControl = ctrl,
                        method = "svmRadial")

lmProfile

lmProfile$optVariables

x_selected <- as.matrix(x[, c("X69", "X30", "X74", "X76", "X13")])
svm1 <- kernlab::ksvm(x_selected, y_factor, kernel = "rbfdot", prob.model = TRUE) # scale?
svm1

#DALEX
ynumeric <- ifelse(y == -1, 0, 1)

pred_svm <- kernlab::predict(svm1, x_selected, type = "probabilities")
pred_svm <- data.frame(pred_svm)

# results_svm <- attr(pred_svm, "probabilities")
# results_svm <- data.frame(results_svm[, 1])
# colnames(results_svm) <- "prob"
classifierplots::density_plot(ynumeric, pred_svm$Class2)
Metrics::auc(ynumeric, pred_svm$Class2)
Metrics::logLoss(ynumeric, pred_svm$Class2)


HR_glm_model <- glm(left~., data = breakDown::HR_data, family = "binomial")
explainer_glm <- DALEX::explain(HR_glm_model, data = HR_data, y = HR_data$left,
                                predict_function = function(m,x) predict.glm(m,x,type = "response"))


# create custom predict function
wtf_pred <- function(model, newdata)  {
  pred_svm <- kernlab::predict(svm1, x_selected, type = "probabilities")
  pred_svm <- data.frame(pred_svm)
  results <- pred_svm[, 2]
  return(results)
}

splaining <-
  DALEX::explain(
    model = svm1,
    data = x_selected, # test
    y = ynumeric,
    predict_function = wtf_pred# test
  )

resids_mp <- DALEX::model_performance(splaining)
resids_mp
plot(mp)
plot(mp, geom = "boxplot")
vi_rf <- DALEX::variable_importance(splaining, type = 'ratio')
vi_rf
plot(vi_rf)

sv_rf <-
  DALEX::single_variable(splaining, variable =  "TwoFactor1", type = "pdp")
plot(sv_rf)
sv_rf <-
  DALEX::single_variable(splaining, variable =  "TwoFactor2", type = "ale")
plot(sv_rf)

mp_rf <- DALEX::model_performance(splaining)
library(ggplot2)
ggplot2::ggplot(mp_rf, aes(observed, diff)) + geom_point() +
  xlab("Observed") + ylab("Predicted - Observed") +
  ggtitle("Diagnostic plot for the random forest model")

which.min(mp_rf$diff)
new_claim <- x_selected[which.min(mp_rf$diff), ]
new_claim <- data.frame(t(new_claim))

new_rf <-
  DALEX::single_prediction(splaining, new_claim) # takes some time
breakDown:::print.broken(new_rf)
plot(new_rf)

x_dalex <- data.frame(x_selected)
bd <-
  DALEX::prediction_breakdown(splaining, x_dalex[2, ])
bd

plot(bd)

# Genetic Algo