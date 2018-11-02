# SVM ---------------------------------------------------------------------
colnames(regDF)
prep <- caret::preProcess(regDF[, -25])
x <- predict(prep, regDF[, -25]) 
x <- data.frame(x)
subsets <- c(4:8)

set.seed(10)

ctrl <- caret::rfeControl(functions = caret::lrFuncs,
                          method = "cv",
                          repeats = 3,
                          verbose = TRUE)

y <- regDF$Class
x <- x[, -11]

lmProfile <- caret::rfe(x, y,
                        sizes = subsets,
                        rfeControl = ctrl,
                        method = "svmRadial")

lmProfile

lmProfile$optVariables

x_selected <- as.matrix(x[, c("TwoFactor2", "TwoFactor1", "Linear2", "V41", "Linear3")])
svm1 <- kernlab::ksvm(x_selected, y, kernel = "rbfdot", prob.model = TRUE) 
svm1

#DALEX
ynumeric <- ifelse(y == 'Class1', 0, 1)

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

