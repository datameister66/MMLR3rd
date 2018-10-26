# Regularization ----------------------------------------------------------
library(magrittr)
options(scipen=999)


set.seed(123)
sim <-
  caret::twoClassSim(
    n = 10000,
    intercept = -10,
    linearVars = 3,
    noiseVars = 16
  )
table(sim$Class)

regDF <- sim

regDF$V26 <- rnorm(10000, 10, 1)
regDF$V27 <- rbeta(10000, shape1 = 1, shape2 = 3)
regDF$V28 <- rcauchy(10000, 0, 1)
regDF$V29 <- rexp(10000, rate = 2)
regDF$V30 <- rgamma(10000, 5)
regDF$V31 <- rlogis(10000, location = 1, scale = 1)
regDF$V32 <- rlnorm(10000, meanlog = 5, sdlog = .5)
regDF$V33 <- rnbinom(10000, size = 1, prob = .3)
regDF$V34 <- rpois(10000, lambda = 2)
regDF$V35 <- runif(10000, 0, 100)
regDF$V36 <- rweibull(10000, shape = .5)

mu <- rep(0,4)
Sigma <- matrix(.85, nrow=4, ncol=4) + diag(4)*.2
mvr <- MASS::mvrnorm(10, mu = mu, Sigma = Sigma)
cor(mvr)

regDF$V37 <- mvr[,1]
regDF$V38 <- mvr[,2]
regDF$V39 <- mvr[,3]
regDF$V40 <- mvr[,4]

# tweak noise 1
class1 <- 10 + rnorm(7997, 1, 1)
class2 <- 9.25 + rnorm(2003, 1, .75)
classF <- ifelse(regDF$Class == 'Class1', class1, class2)
boxplot(classF ~ regDF$Class)
regDF$V41 <- classF

regDF$Noise03 <- regDF$Linear3   

hist(regDF$V32)
regDF$V32 <- ifelse(regDF$Class == 'Class1', regDF$V32, regDF$V32 + rnorm(1, 25, 1))
boxplot(regDF$V32 ~ regDF$Class)

regDF %>%
  dplyr::group_by(Class) %>%
  sjmisc::descr() -> regDescr

df1 <- regDescr[[1]]
df2 <- regDescr[[2]]
descr <- dplyr::bind_rows(df1, df2)
readr::write_csv(descr, 'descr.csv')

regDF$y <- ifelse(regDF$Class == 'Class2', 1, 0)
table(regDF$y)
regDF <- regDF[, -25]
colnames(regDF)

# correlation
my_data_cor <- cor(regDF)
high_cor <- caret::findCorrelation(my_data_cor, cutoff = 0.6)
high_cor
mycor <- as.data.frame(my_data_cor)
mycor$var1 <- row.names(mycor)
myDataFrame_correlation <- tidyr::gather(data = mycor, key = "var2", value = "correlation", -var1)


set.seed(1066)
index <- caret::createDataPartition(regDF$y, p = 0.7, list = F)
train <- regDF[index, ]
test <- regDF[-index, ]

x <- as.matrix(train[, -41])
y <- as.factor(train$y)


set.seed(1999)
cvg <- glmnet::cv.glmnet(x, y,
                         nfolds = 5,
                         type.measure = "auc",
                         alpha = 0,
                         family = "binomial") 

plot(cvg)

coef <- coef(cvg, s = "lambda.1se")
xyz <- data.frame(coef@Dimnames[[1]])
xyz$coef <- coef@x
View(xyz)

cvg$lambda.min  
cvg$lambda.1se

cvg_pred <- data.frame(predict(cvg, newx = x, type = "response", s = "lambda.1se"))
classifierplots::density_plot(y, cvg_pred$X1)
Metrics::auc(y, cvg_pred$X1) #.8514
ynum <- as.numeric(ifelse(y == "1", 1, 0))
Metrics::logLoss(ynum, cvg_pred$X1) #.3629


testpred1 <-
  data.frame(predict(cvg, newx = as.matrix(test[, -41]), type = 'response'), s = "lambda.1se")
classifierplots::density_plot(test$y, testpred1$X1)
Metrics::auc(test$y, testpred1$X1) #8442
Metrics::logLoss(test$y, testpred1$X1) #0.36865

set.seed(1876)
cvg2 <- glmnet::cv.glmnet(x, y,
                          nfolds = 5,
                          type.measure = "auc",
                          alpha = 1,
                          family = "binomial")

plot(cvg2)
cvg2$lambda.min
cvg2$lambda.1se
coef(cvg2, s = "lambda.1se") 
cvg2_pred <- data.frame(predict(cvg2, newx = x, type = "response", s = "lambda.1se"))
classifierplots::density_plot(y, cvg2_pred$X1)
Metrics::auc(y, cvg2_pred$X1) #.8127
Metrics::logLoss(ynum, cvg2_pred$X1) #.3958

testpred2 <-
  data.frame(predict(cvg2, newx = as.matrix(test[, -41]), type = 'response'), s = "lambda.1se")
classifierplots::density_plot(test$y, testpred2$X1)
Metrics::auc(test$y, testpred2$X1)#.8121
Metrics::logLoss(test$y, testpred2$X1)

grid <- expand.grid(.alpha = seq(0, 1, by = .2), .lambda = seq(0.02, 0.05,  by = 0.0025))
grid

Control <- caret::trainControl(method = 'cv', number = 5)
set.seed(2222)
ridgeTrain <- caret::train(x, y,
                           method = "glmnet",
                           tuneGrid = grid,
                           metric = "Kappa"
)
ridgeTrain
ridgeTrain$results
ridgeTrain$bestTune


best <- glmnet::glmnet(x, y,
                       alpha = 0.0, lambda = 0.02,
                       family = "binomial")
# no plot
coef(best)

rt_pred <- predict(ridgeTrain, train, type = "prob")
classifierplots::density_plot(y, rt_pred$`1`)
Metrics::auc(y, rt_pred$`1`)
Metrics::logLoss(ynum, rt_pred$`1`)

testpred3 <-
  predict(ridgeTrain, test, type = "prob")
classifierplots::density_plot(test$y, testpred3$`1`)
Metrics::auc(test$y, testpred3$`1`)
Metrics::logLoss(test$y, testpred3$`1`)