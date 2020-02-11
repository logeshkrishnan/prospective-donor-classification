# required libraries 
library(dplyr)
library(corrplot)
library(caret)
library(randomForest)

# correlation between continous variables
num_col <- trsf[sapply(trsf, is.numeric)]
summary(num_col)

# correlation matrix
num_cor <- cor(num_col)

# correlation plot
corrplot(cor(num_col), type = "upper", method = "number", tl.cex = 1)

# identifying highly correlated variables
highcor <- findCorrelation(num_cor, cutoff = 0.8)

# removing highly correlated variables
remove_cor <- num_col[,-c(17, 26, 16, 10, 21, 14, 13, 19, 5)]

# seperating categorical variables
cat_col <- trsf[sapply(trsf, is.factor)]

# standardising numerical columns before combining 
stan_col <- scale(remove_cor)
summary(arhcas)

# combining 
arhcas <- cbind(stan_col, cat_col)

# seperating train and test from train
set.seed(123)
ind <- sample(2, nrow(arhcas), prob = c(0.75, 0.25), replace = T)
newtrain <- arhcas[ind == 1, ]
newtest <- arhcas[ind == 2, ]
table(newtrain$TARGET_B)

# fitting random forest model
set.seed(333)
rf <- randomForest(TARGET_B~., data = newtrain)
print(rf)

# pred using random forest
prf <- predict(rf, newtest, type = "prob")  
prf <- as.data.frame(prf)

# metrics
cm <- confusionMatrix(prf, newtest$TARGET_B)
rc <- recall(cm$table)
pr <- precision(cm$table)
f1 <- (2* rc * pr/(rc + pr))

# roc curve evaluation
pred <- prediction(prf$`0`, newtest$TARGET_B)
eval <- performance(pred, "acc")
plot(eval)

roc <- performance(pred, "tpr", "fpr")
plot(roc)