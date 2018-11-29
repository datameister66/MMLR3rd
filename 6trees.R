
library(magrittr)
library(RCurl)
options(scipen=999)


# RPART -------------------------------------------------------------------
sim_df <- read.csv("C:/Users/ps324/OneDrive - Cummins/documents/sim_df.csv", stringsAsFactors=FALSE)

table(sim_df$y)
sim_df$y <- as.factor(sim_df$y)
tree_fit <- rpart::rpart(y ~ ., data = sim_df)
tree_fit$cptable
cp <- min(tree_fit$cptable[, 3]) 
cp
nsplit <- tree_fit$cptable[tree_fit$cptable[, 3] == cp, ]
nsplit[[2]]

plot(partykit::as.party(tree_fit))

rparty.test <- predict(tree_fit, newdata = sim_df)
head(rparty.test)
rparty.test <- rparty.test[, 2]

classifierplots::density_plot(sim_df$y, rparty.test)

Metrics::auc(sim_df$y, rparty.test) #.811
ynum <- as.numeric(ifelse(sim_df$y == "1", 1, 0))
MLmetrics::LogLoss(rparty.test, ynum) #.25

InformationValue::optimalCutoff(ynum, rparty.test, optimiseFor = "Both")

rattle::fancyRpartPlot(tree_fit)
rpart.plot::rpart.plot(tree_fit, type = 3, extra = 2,
                       branch = .3, under = TRUE) # extra 2 or 3, 6, 
rules <- rpart.plot::rpart.rules(tree_fit)

rp <- predict(tree_fit, sim_df, type = "class")
table(sim_df$y, rp)
# santander ---------------------------------------------------------------



santander_prepd <- read.csv("C:/Users/ps324/OneDrive - Cummins/documents/santander_prepd.csv", stringsAsFactors=FALSE)

table(santander_prepd$y)

# set same seed as before

set.seed(1066)
index <- caret::createDataPartition(santander_prepd$y, p = 0.7, list = F)
train <- santander_prepd[index, ]
# train <- train[, -2] # drop age
test <- santander_prepd[-index, ]

x <- as.matrix(train[, -143])
y <- as.factor(train$y)

set.seed(1966)
forest_fit <- randomForest::randomForest(x = x, y = y,
                                         ntree = 200,
                                         sampsize = c(3600, 1200))
# plot(forest_fit)

which.min(forest_fit$err.rate[, 1]) #166
forest_fit$err.rate[166]
randomForest::varImpPlot(forest_fit)
ff <- data.frame(unlist(forest_fit$importance))
ff$var <- row.names(ff)

summary(ff)

# ff

my_forest_vars <- dplyr::filter(ff, MeanDecreaseGini > 3.63)
my_forest_vars <- my_forest_vars$var

x_reduced <- x[, my_forest_vars]
dim(x_reduced)

set.seed(567)
forest_fit2 <- randomForest::randomForest(x = x_reduced, y = y,
                                         ntree = 170, #100
                                         sampsize = c(3600, 1200)) #3600,1200
plot(forest_fit2)
which.min(forest_fit2$err.rate[, 1]) #119
randomForest::varImpPlot(forest_fit2)
rf_pred <- predict(forest_fit)

rf_prob <- predict(forest_fit, type = "prob")

y_prob <- rf_prob[, 2]

classifierplots::density_plot(y, y_prob)

Metrics::auc(y, y_prob) #.811
ynum <- as.numeric(ifelse(y == "1", 1, 0))
MLmetrics::LogLoss(y_prob, ynum) #.25

InformationValue::optimalCutoff(ynum, y_prob, optimiseFor = "Both") #0.18

testRF <- predict(forest_fit, type = "prob", newdata = test)
testRF <- testRF[, 2]

classifierplots::density_plot(test$y, testRF)
ytest <- as.numeric(ifelse(test$y == "1", 1, 0))
Metrics::auc(ytest, testRF) #83
MLmetrics::LogLoss(testRF, ytest) #0.25

tabb <- InformationValue::confusionMatrix(ytest, testRF, threshold = 0.18)

# DALEX experiment --------------------------------------------------------

splaining <-
  DALEX::explain(model = forest_fit2, data = x_reduced, y = ynum)
mp <- DALEX::model_performance(splaining)
mp
plot(mp)
plot(mp, geom = "boxplot")

vi_rf <- DALEX::variable_importance(splaining, type = 'ratio')
vi_rf
plot(vi_rf)

#sv_rf <-
#  DALEX::single_variable(splaining, variable =  "V2", type = "pdp")
#plot(sv_rf)

sv_rf <-
  DALEX::single_variable(splaining, variable =  "V2", type = "ale")
plot(sv_rf)

mp_rf <- DALEX::model_performance(splaining)

library(ggplot2)
ggplot2::ggplot(mp_rf, aes(observed, diff)) + geom_point() +
  xlab("Observed") + ylab("Predicted - Observed") +
  ggtitle("Diagnostic plot for the random forest model")

which.min(mp_rf$diff)
new_claim <- x_reduced[1, ] #642 or 690 0.0054
new_claim <- data.frame(t(new_claim))

new_rf <-
  DALEX::single_prediction(splaining, new_claim) # takes some time
breakDown:::print.broken(new_rf)
plot(new_rf)


# XGBOOST -----------------------------------------------------------------

grid = expand.grid(
  nrounds = c(75, 100),
  colsample_bytree = 1,
  min_child_weight = 1,
  eta = c(0.01, 0.1, 0.3), #0.3 is default,
  gamma = c(0.5, 0.25),
  subsample = 0.5,
  max_depth = c(2, 3)
)

grid

cntrl = caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "final"
)

set.seed(123)
 train.xgb = caret::train(
  x = x_reduced,
  y = y,
  trControl = cntrl,
  tuneGrid = grid,
  method = "xgbTree",
  metric = "Kappa"
)

 train.xgb
 
 param <- list( objective = "binary:logistic",
                booster = "gbtree",
                eval_metric = "error",
                eta = 0.3,
                max_depth = 3,
                subsample = 0.5,
                colsample_bytree = 1,
                gamma = 0.25
 )
 
 train.mat <- xgboost::xgb.DMatrix(data = x_reduced, label = ynum)
 
 set.seed(1232)
 xgb.fit <- xgboost::xgb.train(params = param, data = train.mat, nrounds =
                          100)
 impMatrix <- xgboost::xgb.importance(feature_names = dimnames(x)[[2]],
                             model = xgb.fit)
 xgboost::xgb.plot.importance(impMatrix, main = "Gain by Feature")

 pred <- predict(xgb.fit, x_reduced) 

classifierplots::density_plot(y, pred)
 
Metrics::auc(y, pred) #.811
ynum <- as.numeric(ifelse(y == "1", 1, 0))
MLmetrics::LogLoss(pred, ynum) #.12
 
InformationValue::optimalCutoff(ynum, pred, optimiseFor = "Both") # 0.035

test_xgb <- as.matrix(test)
test_xgb <- test_xgb[, my_forest_vars]

xgb_test_matrix <- xgboost::xgb.DMatrix(data = test_xgb, label = ytest) 
xgb_pred <- predict(xgb.fit, xgb_test_matrix)

classifierplots::density_plot(ytest, xgb_pred)

Metrics::auc(ytest, xgb_pred) #.811

MLmetrics::LogLoss(xgb_pred, ytest) #.25


# RF feature selection ----------------------------------------------------

set.seed(5150)
rf_fs <- Boruta::Boruta(y ~ ., data = sim_df)

rf_fs$timeTaken #2.84 minutes workstation

table(rf_fs$finalDecision)

fnames <- Boruta::getSelectedAttributes(rf_fs)

fnames


