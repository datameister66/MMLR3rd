santander <- readr::read_csv("C:/Users/ps324/OneDrive - Cummins/documents/Explore/santander.csv")
library(magrittr)
table(santander$TARGET)

dupes <- duplicated(santander)
table(dupes)

# unhappy <- santander %>%
#   dplyr::filter(TARGET == 0) %>%
#   dplyr::sample_n(15040)

# newDF <- santander %>%
#   dplyr::filter(TARGET == 1) %>%
#   dplyr::bind_rows(unhappy)

newDF <- santander
feature_variance <- caret::nearZeroVar(newDF, 
                                       saveMetrics = TRUE)

table(feature_variance$zeroVar)
table(feature_variance$nzv)
##########
newDF <- newDF[, feature_variance$zeroVar == 'FALSE']
readr::write_csv(newDF, "C:/Users/ps324/OneDrive - Cummins/documents/Explore/santander2.csv")
newDF <- readr::read_csv("C:/Users/ps324/OneDrive - Cummins/documents/Explore/santander2.csv")
##########
na_count <-
  sapply(train, function(y)
    sum(length(which(is.na(
      y
    )))))
na_count <- data.frame(na_count)

newDF %>%
#  dplyr::group_by(type) %>%
  sjmisc::descr()  -> descr

readr::write_csv(descr, "santander_descr.csv")

newDF %>% 
  dplyr::summarise_all(dplyr::funs(dplyr::n_distinct(.))) -> unique_count

# deal with missing
newDF$var3_isNA <-
  ifelse(newDF$var3 == -999999, 1, 0)
table(newDF$var3_isNA)
newDF$var3 <- ifelse(newDF$var3 == -999999, 0, newDF$var3)

# treat

# drop id
newDF <- newDF[, -1]
varlist = colnames(newDF[, !colnames(newDF) %in% c("TARGET")])

df_treatment <- vtreat::designTreatmentsZ(
  dframe = newDF,
  varlist = varlist
)

treated <- vtreat::prepare(df_treatment, newDF)

colnames(treated) <-
  sub('_clean', "", colnames(treated))

colnames(treated) <-
  sub('_clean', "", colnames(treated))

colnames(treated) <-
  sub('_isBAD', "_isNA", colnames(treated))

colnames(treated) <-
  sub('_isBAD', "_isNA", colnames(treated))

treated$y <- newDF$TARGET
readr::write_csv(treated, 'santander_treated.csv')
# correlation
linear_combos <- caret::findLinearCombos(x = treated)

linear_combos
linear_remove <- colnames(treated)[linear_combos$remove]

treated <-
  treated[, !(colnames(treated) %in% linear_remove)]

my_data_cor <- cor(treated)
y <- as.data.frame(my_data_cor)
y$var1 <- row.names(y)
myDataFrame_correlation <- tidyr::gather(data = y, key = "var2", value = "correlation", -var1)

# rid ourselves of the diagonal; filter by high/low values as desired
myDataFrame_correlation <- myDataFrame_correlation %>%
  dplyr::filter(var1 != var2)

readr::write_csv(myDataFrame_correlation, "C:/Users/ps324/OneDrive - Cummins/documents/Explore/santanCorr.csv")

# remove > 0.9
high_corr <- caret::findCorrelation(my_data_cor, cutoff = 0.8)

length(high_corr)

santander_DF <- treated[, -high_corr]
readr::write_csv(santander_DF, "santander_prepd.csv")
santander_DF <- readr::read_csv("santander_prepd.csv")
# santander_DF$target <- newDF$TARGET
# information value
options(scipen=10)

set.seed(1966)

santander_DF %>%
  dplyr::sample_frac(.8) -> train

santander_DF %>%
  dplyr::anti_join(train) -> test

table(train$y)
train_zero <- caret::nearZeroVar(train, saveMetrics = TRUE)
table(train_zero$zeroVar)
train <- train[, train_zero$zeroVar == 'FALSE']

table(test$y)

IV <- Information::create_infotables(data=train, 
                        y="y",
                        parallel=FALSE)
knitr::kable(head(IV$Summary, 20)) # var15 suspicious
knitr::kable(IV$Tables$var15)
knitr::kable(IV$Tables$ind_var8_0)
knitr::kable(IV$Tables$var38)
Information::plot_infotables(IV, "var15", show_values=TRUE)
Information::plot_infotables(IV, "ind_var8_0", show_values=TRUE)
Information::plot_infotables(IV, "var38", show_values=TRUE)
Information::plot_infotables(IV, IV$Summary$Variable[1:4], same_scales=TRUE)

features <- IV$Summary$Variable[1:20]

train_reduced <- train[, colnames(train) %in% features]
train_reduced$y <- train$y
cor_trainR <- cor(train_reduced)
corrplot::corrplot.mixed(cor_trainR, tl.pos = "lt", upper = 'ellipse')

full_fit <- glm(y ~ ., family = binomial, data = train_reduced)
summary(full_fit)
exp(coef(full_fit))
car::vif(full_fit)
full_probability <- predict(full_fit, type = 'response')
earth::plotd(full_fit)
full_cutoff <-
  InformationValue::optimalCutoff(
    train_reduced$y,
    full_probability,
    optimiseFor = 'Both',
    returnDiagnostics = TRUE
  )
InformationValue::confusionMatrix(train_reduced$y, full_probability, threshold = 0.07)
Metrics::auc(train_reduced$y, full_probability)
mean(Metrics::ll(train_reduced$y, full_probability))
full_test <- predict(full_fit, test, type = 'response')
InformationValue::confusionMatrix(test$y, full_test, threshold = 0.07)
Metrics::auc(test$y, full_test)
mean(Metrics::ll(test$y, full_test))
# 0.152

glm_control <-
  caret::trainControl(method = "cv",
                      number = 5,
                      returnResamp = "final")


x <- train_reduced[, -15]
y <- as.factor(train_reduced$y)
set.seed(2018)
glm_fit <-
  caret::train(x, y, method =  "glm",    
               trControl = glm_control,
               trace = FALSE)

glm_fit$results
glm_fit$finalModel
glm_test_pred <- predict(glm_fit, test, type = "prob")

cv_cutoff <-
  InformationValue::optimalCutoff(
    test$y,
    glm_test_pred,
    optimiseFor = 'Both',
    returnDiagnostics = TRUE
  )
cv_cutoff #0,07
InformationValue::confusionMatrix(test$y, glm_test_pred$`1`, threshold = 0.07)
Metrics::auc(test$y, glm_test_pred$`1`)
Metrics::logLoss(test$y, glm_test_pred$`1`)

# MARS
colnames(train)
set.seed(1972)
earth_fit <-
  earth::earth(
    x = train[, -142],
    y = train[, 142],
    pmethod = 'cv',
    #'cv'
    nfold = 5,
    # ncross = 1,
    degree = 1,
    minspan = -1,
    nprune = 15,
    glm = list(family = binomial)
  )

summary(earth_fit) 
plot(earth_fit)
# earth::evimp(earth_fit)
plotmo::plotmo(earth_fit, nrug = TRUE, rug.col = "red")

pred <- predict(earth_fit, train, type = 'response')
earth::plotd(earth_fit)
test_pred <- predict(earth_fit, test, type = 'response')

# 0.41
mars_cutoff <-
  InformationValue::optimalCutoff(
    train$y,
    pred,
    optimiseFor = 'Both',
    returnDiagnostics = TRUE
  )
InformationValue::confusionMatrix(train$y, pred, threshold = 0.05)

Metrics::auc(actual = train$y, predicted = pred)
Metrics::auc(test$y, test_pred)
Metrics::logLoss(actual = train$y, predicted = pred)
Metrics::logLoss(test$y, test_pred)

max(pred)
min(pred)

# joint AUC plots


# lift chart

