# importing required libraries
library(ggplot2) # for plots
library(dplyr) # for data manipulation
library(moments) # calculating skewness and kurtosis
library(naniar) # to replace ? with NA
library(caret) # one hot encoding 
library(e1071) # for confusion matrix
library(ROCR) # for roc curve
library(randomForest)

# loading the dataset 
mydata <- read.csv(".../Donor Raw Data_ML.csv")

# descriptive
head(mydata)
summary(mydata)
str(mydata)

# let's take the dependant variable and do some analysis
tab <- table(mydata$TARGET_B)
prop.table(tab)
ggplot(mydata, aes(x = TARGET_B, fill = TARGET_B) +
         geom_text(aes(label = TARGET_B), size = 3, hjust = 0.5, vjust = 3)) +
  geom_bar(stat = "count", color = "black") +
  labs(x = "TARGET_B", y = 'COUNT') +
  theme(axis.text.x = element_blank()) +
  scale_color_brewer(palette="Dark2")
# 1 - yes & 0 - no
# huge number of 0 than 1 implying imbalanced data
# 75% percentage of people didn't respond to solicitation

# Donor age analysis
# from summary few things to notice is mean age is 58.92, min is 0 and NA's 4795
# histogram of age
ggplot(mydata, aes(x = DONOR_AGE)) + 
  geom_histogram(binwidth = 5, color="black", fill="white")
# most people are older than 35
table(mydata$DONOR_AGE)

# In-House program
table(mydata$IN_HOUSE)
table(mydata[which(mydata$IN_HOUSE == 1), "TARGET_B"])
# 90% data 0 
# this feature doesn't offer information

# Urbanicity
table(mydata$URBANICITY)
# urbanicity vs target_b
ggplot(mydata, aes(x=URBANICITY, fill=factor(TARGET_B))) + 
  geom_bar() + 
  labs(fill = "TARGET_B") +
  ggtitle("Pclass vs Survival") + 
  theme_minimal()
# 454 unknown values
# all data is equally distributed 
# might be very useful

# SES and cluster code is not understandable

# home owner variable has lot of unknowns, better to delete

# gender has around 1000 unknown better to replace with mode

# income group might be important better to go in-depth
ggplot(mydata, aes(x = INCOME_GROUP)) +
  geom_histogram(stat = "count")
# 4392 missing values
# don't know how to handle missing values 

# published phone and overlay source might not be important 

# mor hit rate
ggplot(mydata, aes(x = MOR_HIT_RATE)) + 
  geom_histogram(binwidth = 1, color="black", fill="white")
prop.table(table(mydata$MOR_HIT_RATE))
# 60% of data is 0
# better to remove

# wealth rating has huge no of NA's 
# better to remove 

# median home value
# has 0 values but its very less
ggplot(mydata, aes(x = MEDIAN_HOME_VALUE)) + 
  geom_histogram(color="black", fill="white")
# it is right skewed 
# normalization 
skewness(mydata$MEDIAN_HOME_VALUE) # 2.456 indicates right skewness or positively skewed
kurtosis(mydata$MEDIAN_HOME_VALUE) # 9.99 indicates tail deviation from normal positive or negative values

# median household income
ggplot(mydata, aes(x = MEDIAN_HOUSEHOLD_INCOME)) + 
  geom_histogram(color="black", fill="red") +
  geom_density(color = 'blue')
# looks normal with little skewness
skewness(mydata$MEDIAN_HOUSEHOLD_INCOME) # 1.72
kurtosis(mydata$MEDIAN_HOUSEHOLD_INCOME) # 9.47

# pct_owner_occupied and per_capita_income
ggplot(mydata, aes(x = PCT_OWNER_OCCUPIED)) + 
  geom_density(color="black", fill="red")
ggplot(mydata, aes(x = PER_CAPITA_INCOME, fill = TARGET_B)) + 
  geom_density(color="black")
# 0 values in the data 
# not able to understand the importance

# all the pct_attributes doeesn't make sense

# pep star and recent star status AND FREQUENCY STATUS
ggplot(mydata, aes(x = RECENT_STAR_STATUS, fill = TARGET_B)) +
  geom_bar()
# people with star status seems to be contributing
# recent star status might be the same indicator and of more zero values
# frequency status is okay

# all the recent and lifetime variables are continous and seems useful 

# months since last from resp has negative values and na's

# all the other variables seems relevant for analysis

# DATA PREPROCESSING 
# from the initial analysis we were able to understand that some variables are not needed
# target d, control number, in house, home owner, published phone, overlay source, wealth rating, all pct attributes
intial_data <- subset(mydata, select = -c(TARGET_D, CONTROL_NUMBER, IN_HOUSE, HOME_OWNER, PUBLISHED_PHONE, OVERLAY_SOURCE,
                                          WEALTH_RATING,  PCT_ATTRIBUTE1, PCT_ATTRIBUTE2, PCT_ATTRIBUTE3, PCT_ATTRIBUTE4))
# change in data structure 
mydata$TARGET_B <- as.factor(mydata$TARGET_B)
mydata$INCOME_GROUP <- as.factor(mydata$INCOME_GROUP)
mydata$MONTHS_SINCE_LAST_PROM_RESP <- as.factor(mydata$MONTHS_SINCE_LAST_PROM_RESP)
income_data$DONOR_AGE <- as.integer(income_data$DONOR_AGE)

# replace missing data 
prop.table(colSums(is.na(intial_data)))
# replacing ? in urbanicity, ses and cluster code with na 
intial_data <- intial_data %>% replace_with_na(replace = list(URBANICITY = "?",
                                                              SES = "?",
                                                              CLUSTER_CODE = " .",
                                                              DONOR_GENDER = "U"))

# let's start with months since last prom resp
table(intial_data$MONTHS_SINCE_LAST_PROM_RESP)
# one few negative values and 246 missing replacing both with mode
# function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(intial_data$MONTHS_SINCE_LAST_PROM_RESP)
# mode is 18
# replacing values
intial_data[which(is.na(intial_data$MONTHS_SINCE_LAST_PROM_RESP)),]$MONTHS_SINCE_LAST_PROM_RESP <- getmode(intial_data$MONTHS_SINCE_LAST_PROM_RESP)
intial_data[which(intial_data$MONTHS_SINCE_LAST_PROM_RESP < 0),]$MONTHS_SINCE_LAST_PROM_RESP <- getmode(intial_data$MONTHS_SINCE_LAST_PROM_RESP)

# missing values in donor gender
# replacing with mode
table(intial_data$CLUSTER_CODE)
intial_data[which(is.na(intial_data$DONOR_GENDER)),]$DONOR_GENDER <- getmode(intial_data$DONOR_GENDER)
intial_data[which(intial_data$DONOR_GENDER == 'A'),]$DONOR_GENDER <- getmode(intial_data$DONOR_GENDER)


# missing values of urbanicity, ses and cluster code are also repalced with mode
intial_data[which(is.na(intial_data$URBANICITY)),]$URBANICITY <- getmode(intial_data$URBANICITY)
intial_data[which(is.na(intial_data$SES)),]$SES <- getmode(intial_data$SES)
intial_data[which(is.na(intial_data$CLUSTER_CODE)),]$CLUSTER_CODE <- getmode(intial_data$CLUSTER_CODE)


# 48% of income group data is NA, let's remove all the rows with missing values of income group that has target_b as 0. 
# this might help us with balancing data but also can loose some other information 
# make sure by applying this data to vanilla model and find out

income_na <- intial_data[which(!is.na(intial_data$INCOME_GROUP) & (intial_data$TARGET_B == "0")), ]
income_na1 <- intial_data[which(!is.na(intial_data$INCOME_GROUP) & (intial_data$TARGET_B == "1")), ]
income_data <- rbind(income_na, income_na1)

summary(income_data)

# creating age band 
ggplot(income_data, aes(x = DONOR_AGE)) +
  geom_bar()

income_data <- income_data %>%
  mutate('AGE_BAND' = case_when(DONOR_AGE < 18 ~ "child",
                                DONOR_AGE >= 18 & DONOR_AGE < 35 ~ "youth",
                                DONOR_AGE >= 35 & DONOR_AGE < 50 ~ "adult",
                                DONOR_AGE >= 50 & DONOR_AGE < 70 ~ "middle_aged",
                                DONOR_AGE >= 70 ~ "elderly"))
table(income_data$AGE_BAND)

# replacing missing values with mode 
income_data$AGE_BAND <- as.factor(income_data$AGE_BAND)
income_data[which(is.na(income_data$AGE_BAND)),]$AGE_BAND <- getmode(income_data$AGE_BAND)

# creating new dataframe for encoding variables
# removing donor_age variable
new_data <- subset(income_data, select = -c(DONOR_AGE))

# final check for missing values
sum(is.na(new_data))
# no more missing or wrong values in tha dataset

# ENCODING CATEGORICAL VARIABLES
# variables that need to be encoded
# one hot - urbanity, recency status 96nk
# normal - donor gender, age band

# changing variable type to character for the above variables
str(new_data)
new_data$URBANICITY <- as.character(new_data$URBANICITY)
new_data$RECENCY_STATUS_96NK <- as.character(new_data$RECENCY_STATUS_96NK)
new_data$AGE_BAND <- as.character(new_data$AGE_BAND)
new_data$DONOR_GENDER <- as.character(new_data$DONOR_GENDER)



# encoding gender
table(new_data$DONOR_GENDER)
new_data[which(new_data$DONOR_GENDER == "F"),]$DONOR_GENDER <- "0"
new_data[which(new_data$DONOR_GENDER == "M"),]$DONOR_GENDER <- "1"

# encoding age band
table(new_data$AGE_BAND)
new_data[which(new_data$AGE_BAND == "child"),]$AGE_BAND <- "0"
new_data[which(new_data$AGE_BAND == "youth"),]$AGE_BAND <- "1"
new_data[which(new_data$AGE_BAND == "adult"),]$AGE_BAND <- "2"
new_data[which(new_data$AGE_BAND == "middle_aged"),]$AGE_BAND <- "3"
new_data[which(new_data$AGE_BAND == "elderly"),]$AGE_BAND <- "4"

# changing all variables to integers except variables that needs to be one hot encoded
new_data$AGE_BAND <- as.integer(new_data$AGE_BAND)
new_data$DONOR_GENDER <- as.integer(new_data$DONOR_GENDER)
new_data$SES <- as.integer(new_data$SES)
new_data$CLUSTER_CODE <- as.integer(new_data$CLUSTER_CODE)
new_data$CLUSTER_CODE <- as.integer(new_data$CLUSTER_CODE)

# One Hot Encoding 
str(new_data)
hot_data <- new_data

# dummify the data
dmy <- dummyVars(" ~ .", data = hot_data)
trsf <- data.frame(predict(dmy, newdata = hot_data))
summary(trsf)
str(trsf)

trsf$AGE_BAND <- as.factor(trsf$AGE_BAND)
trsf$RECENT_STAR_STATUS <- as.factor(trsf$RECENT_STAR_STATUS)
trsf$RECENCY_STATUS_96NKA <- as.factor(trsf$RECENCY_STATUS_96NKA)
trsf$RECENCY_STATUS_96NKE <- as.factor(trsf$RECENCY_STATUS_96NKE)
trsf$RECENCY_STATUS_96NKF <- as.factor(trsf$RECENCY_STATUS_96NKF)
trsf$RECENCY_STATUS_96NKL <- as.factor(trsf$RECENCY_STATUS_96NKL)
trsf$RECENCY_STATUS_96NKN <- as.factor(trsf$RECENCY_STATUS_96NKN)
trsf$RECENCY_STATUS_96NKS <- as.factor(trsf$RECENCY_STATUS_96NKS)
trsf$FREQUENCY_STATUS_97NK <- as.factor(trsf$FREQUENCY_STATUS_97NK)

# vanilla model
# seperating train and test from train
set.seed(123)
ind <- sample(2, nrow(trsf), prob = c(0.75, 0.25), replace = T)
newtrain <- trsf[ind == 1, ]
newtest <- trsf[ind == 2, ]

# fitting random forest model
set.seed(333)
rf <- randomForest(TARGET_B~., data = newtrain)
print(rf)

# pred using random forest
prf <- predict(rf, newtest, type = "prob", cutoff = 0.05)  
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

# area under curve 
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))

# vanilla model performance is not good (auc = 0.3863)

# vanilla model had the following results
# accuracy = 0.7375  
# recall = 0.979235
# precision = 0.7454243
# f1 = 0.8464809
# still normalization, outlier, correlation and balancing needed

# trying logistic regression vanilla model
logitMod <- glm(TARGET_B ~ ., data = newtrain, family=binomial(link="logit"))
print(logitMod)

# pred using random forest
prf <- predict(logitMod, newtest, type = "response")  
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

