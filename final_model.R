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
prf <- predict(rf, newtest[-19])  

# fitting logistic regression model
logitMod <- glm(TARGET_B ~ ., data = newtrain, family=binomial(link="logit"))
print(logitMod)

# pred using logistic regression
plr <- predict(logitMod, newtest, type = "response")  
plr <- as.data.frame(prf)

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