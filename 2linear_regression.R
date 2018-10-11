
data(anscombe)

attach(anscombe)
anscombe

cor(x1, y1) #correlation of x1 and y1

cor(x2, y1) #correlation of x2 and y2

par(mfrow = c(2,2)) #create a 2x2 grid for plotting

plot(x1, y1, main = "Plot 1")

plot(x2, y2, main = "Plot 2")

plot(x3, y3, main = "Plot 3")

plot(x4, y4, main = "Plot 4")

install.packages("alr3")

library(alr3)

data(snake)

dim(snake)

head(snake)

colnames(snake) <- c("content", "yield")

attach(snake) # attach data with new names

head(snake)


plot(content,
     yield,
     main = "Scatterplot of Snow vs. Yield",
     xlab = "water content of snow",
     ylab = "water yield")

yield_fit <- lm(yield ~ content)

summary(yield_fit)

plot(content, log(yield))

abline(yield_fit, lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(yield_fit)

# multivariate
library(magrittr)
options(scipen = 999)
# install.packages("caret")
# install.packages("extRemes")
# install.packages("ggthemes")
# install.packages("janitor")
# install.packages("mixlm")
# install.packages("readr")
# install.packages("sjmisc")
# install.packages("smwrStats")
# install.packages("tidyverse")
# install.packages("vtreat")

ames <- readr::read_csv("C:/Users/cory/Desktop/data/ames.csv")
ames <- read_csv("C:/Users/ps324/OneDrive - Cummins/documents/Explore/ames.csv")

dim(ames)

dupes <- duplicated(ames)
table(dupes)

ames %>%
  sjmisc::descr() -> ames_descr
#Id
ames <- ames[, -1]
#overall quality and condition
# sales price
# age
ames %>%
  dplyr::mutate(yearsOld = 2011 - YearBuilt) -> ames
# year remod
ames %>%
  dplyr::mutate(yearsRemodel = 2011 - YearRemodAdd) -> ames


# garage year built
ames %>%
  dplyr::mutate(yearsGarage = 2011 - GarageYrBlt) -> ames
# missing
ames$yearsGarage_isNA <- 
  ifelse(is.na(ames$yearsGarage), 1, 0)

ames$yearsGarage[is.na(ames$yearsGarage)] <- 0
# MoSold character
ames$MoSold <- as.character(ames$MoSold)

# remove
ames <- ames[, c(-19, -20, -59)]

ggplot2::ggplot(ames, ggplot2::aes(x = SalePrice)) + 
  ggplot2::geom_histogram() +
  ggthemes::theme_few()

ames %>%
  dplyr::mutate(logSales = log(SalePrice)) -> ames

ggplot2::ggplot(ames, ggplot2::aes(x = logSales)) + 
  ggplot2::geom_histogram() +
  ggthemes::theme_economist_white()

# LotFrontage

# MasVnrArea

ames$LotFrontage_isNA <- 
  ifelse(is.na(ames$LotFrontage), 1, 0)

ames$LotFrontage[is.na(ames$LotFrontage)] <- 0

ames$MasVnrArea_isNA <- 
  ifelse(is.na(ames$MasVnrArea), 1, 0)

ames$MasVnrArea[is.na(ames$MasVnrArea)] <- 0

# low or no variance ------------------------------------------------------

feature_variance <- caret::nearZeroVar(ames, saveMetrics = TRUE)

table(feature_variance$zeroVar)
#Street

# train and test
set.seed(1944)

ames %>%
  dplyr::sample_frac(.8) -> train

ames %>%
  dplyr::anti_join(train) -> test

varlist = colnames(ames[, !colnames(ames) %in% c('SalePrice', 'logSales')])

train_y <- train$SalePrice
train_logy <- train$logSales
test_y <- test$SalePrice
test_logy <- test$logSales

# design treatments
df_treatment <- vtreat::designTreatmentsZ(
  dframe = train,
  varlist = varlist,
  minFraction = 0.10
)

trained <- vtreat::prepare(df_treatment, train)
tested <- vtreat::prepare(df_treatment, test)

dim(trained)
dim(tested)

trained <- 
  trained %>%
  dplyr::select(-dplyr::contains('_catP'))

tested <- 
  tested %>%
  dplyr::select(-dplyr::contains('_catP'))

colnames(trained) <-
  sub('_clean', "", colnames(trained))

colnames(tested) <-
  sub('_clean', "", colnames(tested))

colnames(trained) <-
  sub('_isBAD', "_isNA", colnames(trained))

colnames(tested) <-
  sub('_isBAD', "_isNA", colnames(tested))

# correlation
df_corr <- cor(trained)

high_corr <- caret::findCorrelation(df_corr, cutoff = 0.79)

length(high_corr)

trained <- trained[, -high_corr]

caret::findLinearCombos(trained)

# linear_remove <- colnames(trained[, c(104, 145)])

#trained <-
#  trained[, !(colnames(trained) %in% linear_remove)]


# trained$y <- train_logy

# linear dependency
plm::detect_lin_dep(trained)

# fit stepwise
step_control <-
  caret::trainControl(method = "cv",
                      number = 3,
                      returnResamp = "final")

set.seed(1984)
step_fit <-
  caret::train(trained, train_logy, method =  "leapForward",     #"lmStepAIC", #leapSeq
               tuneGrid = data.frame(nvmax = 15:30), 
               trControl = step_control,
               trace = FALSE)

step_fit$results
step_fit$bestTune
# summary(step_fit$finalModel)
# coef(step_fit$finalModel, 22)
# or
coef(step_fit$finalModel)
broom::tidy(coef(step_fit$finalModel, 20)) -> step_coef

step_coef <- step_coef[-1, ]
lm_formula <- as.formula(paste("y ~ ",paste(step_coef$names, collapse="+"), sep = ""))

trained$y <- train_logy
step_lm <- lm(lm_formula, data = trained)

summary(step_lm)
plot(step_lm) 

train_reduced <- trained[c(-87, -248, -918), ] #345

step_lm2 <- lm(lm_formula, data = train_reduced)
plot(step_lm2)

step_lm2 #0.87, 0.1427, 0.100
step_vif <- broom::tidy(car::vif(step_lm2))

step_pred <- predict(step_lm2, tested)
caret::postResample(pred = step_pred, obs = test_logy)

lm_predExp <- exp(step_pred)
caret::postResample(pred = lm_predExp, obs = test_y)


step_resid <- (test_logy - step_pred)
hist(step_resid)
extRemes::qqnorm(step_resid)

# 
# step_fit <-
#   mixlm::wideForward(y ~ ., data = trained, alpha = 0.01)
# 
# # step_fit
# step_model <- broom::tidy(step_fit$model)
# step_model$p.value <- round(step_model$p.value, 3)
# 
# broom::glance(step_fit$model)
# 
# # broom::augment(step_fit$model)
# 
# # give me colnames for best subset
# 
# step_model %>%
#   dplyr::filter(p.value < 0.05) -> step_selected
# 
# # step_model %>%
# #   dplyr::arrange(p.value) %>%
# #   dplyr::filter(dplyr::row_number() < 22) -> step_selected # includes intercept
# 
# train_filtered <- trained[, colnames(trained) %in% step_selected$term]
# 
# lm_control <-
#      caret::trainControl(method = "cv",
#                          number = 5,
#                          returnResamp = "final")
# 
# set.seed(1984)
# lm_fit <-
#    caret::train(train_filtered, train_logy, method = "lm", trControl = lm_control)
# 
# lm_fit #0.87, 0.1427, 0.100
# 
# lm_pred <- predict(lm_fit, tested)
# caret::postResample(pred = lm_pred, obs = test_logy)
# #0.88, 0.1334, 0.097
# lm_predExp <- exp(lm_pred)
# caret::postResample(pred = lm_predExp, obs = test_y)
# #.8565, 18302
# 
# lm_resid <- (test_logy - lm_pred)
# hist(lm_resid)
# extRemes::qqnorm(lm_resid)

# Earth -------------------------------------------------------------------
colnames(train_reduced)
set.seed(1988)
earth_fit <-
  earth::earth(
    x = train_reduced[, -96],
    y = train_reduced[, 96],
    pmethod = 'cv',
    #'cv'
    nfold = 5,
    # ncross = 1,
    degree = 1,
    minspan = -1,
    nprune = 25
  )

summary(earth_fit) 
plot(earth_fit)
earth::evimp(earth_fit)
plotmo::plotmo(earth_fit, nrug = TRUE, rug.col = "red")

earth_pred <- predict(earth_fit, tested)
earth_residTest <- test_logy - earth_pred
hist(earth_residTest)
extRemes::qqnorm(earth_residTest)
caret::postResample(earth_pred, test_logy) #0.86, 0.10

earth_predExp <- exp(earth_pred)
caret::postResample(earth_predExp, test_y) #0.87, 19109
# 0.885, 17,597

earth_resid <- earth_fit$residuals
hist(earth_resid)
extRemes::qqnorm(earth_resid)

# plot(earth_pred, lm_pred)
# plot(earth_pred, test_logy)
# plot(lm_pred, test_logy)

e_pred <- (earth_pred + step_pred) / 2
e_resid <- test_logy - e_pred
plot(e_pred, test_logy)
caret::postResample(e_pred, test_logy)
# heavy tailed 0.89, .009
e_exp <- exp(e_pred)
caret::postResample(e_exp, test_y) #0.88, #17074

# Duan's smoothing function
expo_earth_resid <- exp(earth_resid)
expo_earth_pred <- exp(earth_pred)
hist(expo_earth_resid)
avg_expo_resid <- mean(expo_earth_resid)

smear_pred <- avg_expo_resid * expo_earth_pred
plot(smear_pred)
plot(smear_pred, expo_earth_pred)

duan_smear <- function(pred, resid){
  expo_resid <- exp(resid)
  expo_pred <- exp(pred)
  avg_expo_resid <- mean(expo_resid)
  smear_predictions <- avg_expo_resid * expo_pred
  return(smear_predictions)
}

pred = e_pred
resid = e_resid
duan_pred <- duan_smear(pred = e_pred, resid = e_resid)

caret::postResample(duan_pred, test_y)

results <- data.frame(duan_pred, test_y)
colnames(results) <- c('predicted', 'actual')

ggplot2::ggplot(results, ggplot2::aes(predicted, actual)) +
            ggplot2::geom_point(size=1) +
            ggplot2::geom_smooth() +
            ggthemes::theme_few()

