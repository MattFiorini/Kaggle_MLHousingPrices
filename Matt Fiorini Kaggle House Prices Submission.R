# Kaggle Housing Prices Regression Models
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques


trainFile = "C:\\Users\\Matt Fiorini\\OneDrive - purdue.edu\\R For Analytics - MGMT 59000\\Team Homework Assigments\\train.csv"
testFile = "C:\\Users\\Matt Fiorini\\OneDrive - purdue.edu\\R For Analytics - MGMT 59000\\Team Homework Assigments\\test.csv"

tr <- read.table(file=trainFile, header=T, sep=",")
te <- read.table(file=testFile, header=T, sep=",")

################################################################################
## Data Cleaning
################################################################################
# You cannot create variable names in R that begin with a number or underscore.
# However, since we pulled this data in from a database, it allowed a couple
# columns to slip by. This can cause us issues later on, so lets fix the following
# column names (1stFlrSF, 2ndFlrSF, 3SsnPorch) to (FstFlrSF, SecFlrSf, ThiSsnPorch)
names(tr)
library(tidyverse)

names(tr)[44] <- "FstFlrSF"; names(te)[44] <- "FstFlrSF"
names(tr)[45] <- "SecFlrSf"; names(te)[45] <- "SecFlrSf"
names(tr)[70] <- "ThiSsnPorch"; names(te)[70] <- "ThiSsnPorch"
names(tr)
names(te)
# Whenever we make changes to our kaggle 'train' set we need to make sure we
# do similar changes to our 'submission' dataset called 'te'.

# One thing you might notice is that some of the columns show NA implying that
# there are missing values. However, this is misleading as in the database they
# were stored as literal NA values instead of missing values. Some of the values
# indeed should be missing, and I show below how I corrected these for each
# field.

# Features where 'NA' is really a missing value, make them blank
# LotFrontage, MasVnrType, MasVnrArea, Electrical, GarageYrBlt
realBlanks <- c("LotFrontage", "MasVnrType", "MasVnrArea", "Electrical"
                , "GarageYrBlt")
# loop through each field and make 'NA' values missing values
for (i in 1:length(realBlanks)){
    tr[which(tr[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
    te[which(te[,realBlanks[i]]=='NA'),realBlanks[i]] <- NA 
}

# Features where 'NA' is a meaningful factor level, we will keep them. These are:
# Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu,
# GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature

str(tr)
# corece these fields to numeric vectors
tr$LotFrontage <- as.numeric(tr$LotFrontage)
tr$GarageYrBlt <- as.numeric(tr$GarageYrBlt)
tr$MasVnrArea <- as.numeric(tr$MasVnrArea)

te$LotFrontage <- as.numeric(te$LotFrontage)
te$GarageYrBlt <- as.numeric(te$GarageYrBlt)
te$MasVnrArea <- as.numeric(te$MasVnrArea)

# Write a for loop that coerces the following fields to factor types:
#"MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","Utilities",
#"LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType",
#"HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType",
#"ExterQual","ExterCond","Foundation", "BsmtQual","BsmtCond",
#"BsmtExposure", "BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir",
#"Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish",
#"GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType",
#"SaleCondition"

s <- c("MSSubClass","MSZoning","Street","Alley","LotShape","LandContour","Utilities",
       "LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType",
       "HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType",
       "ExterQual","ExterCond","Foundation", "BsmtQual","BsmtCond",
       "BsmtExposure", "BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir",
       "Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish",
       "GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType",
       "SaleCondition")
for (i in 1:length(s)){
    tr[,s[i]] <- as.factor(tr[,s[i]])
    te[,s[i]] <- as.factor(te[,s[i]])
}
str(tr)

# For the missing GarageYrBlt values, replace those with the YearBuilt value.
tr$GarageYrBlt <- ifelse(is.na(tr$GarageYrBlt), tr$YearBuilt, tr$GarageYrBlt)
te$GarageYrBlt <- ifelse(is.na(te$GarageYrBlt), te$YearBuilt, te$GarageYrBlt)

# MasVnrType and MasVnrArea both have missing values. Tweak your answers 
# in Q2 and Q3 to show that 1) MasVnrType NA values and MasVnrArea NA values
# correspond to the same rows, and 2) assume that the MasVnrType missing values
# should be adjusted to 'None' and MasVnrArea values to 0. The MasVnrType should
# still be a factor vector with the same factor levels as before.
sum(ifelse(is.na(tr$MasVnrType) & is.na(tr$MasVnrArea), 1, 0))
tr$MasVnrType <- as.factor(ifelse(is.na(tr$MasVnrType),as.character("None")
                                  ,as.character(tr$MasVnrType)))
tr$MasVnrArea <- ifelse(is.na(tr$MasVnrArea), 0, tr$MasVnrArea)

te$MasVnrType <- as.factor(ifelse(is.na(te$MasVnrType),as.character("None")
                                  ,as.character(te$MasVnrType)))
te$MasVnrArea <- ifelse(is.na(te$MasVnrArea), 0, te$MasVnrArea)

# remove the Id column in tr; Don't remove the id column from the 'te' set as 
# you will need it later to submit your predictions to Kaggle
tr = tr %>% select(-(Id))

rm(s, i, realBlanks)
###############################################################################
# Imputing missing values using a predictive model-based imputation approach for
# both the tr and te data.frames.
###############################################################################
# LotFrontage and Electrical features have missing values

sapply(tr, function(x) sum(is.na(x)))
sapply(te, function(x) sum(is.na(x)))

tr$Alley <- NULL
tr$PoolQC <- NULL
tr$Fence <- NULL
tr$MiscFeature <- NULL
tr$FireplaceQu <- NULL
# if you delete columns from train set, do so for test set too
te$Alley <- NULL
te$PoolQC <- NULL
te$Fence <- NULL
te$MiscFeature <- NULL
te$FireplaceQu <- NULL

integer_columns <- sapply(tr, function(col) is.integer(col))
(tr_integer_columns <- names(tr)[integer_columns])
tr_integer_columns = append(tr_integer_columns, names(tr)[3])

te_integer_columns <- sapply(te, function(col) is.integer(col))
(te_integer_columns <- names(te)[te_integer_columns])
te_integer_columns = append(te_integer_columns, names(tr)[4])

install.packages("mice")
library(mice)    # call the mice library so we can use the functions in this library
?mice            # tells you a bit about the mice() function

# here we use a decision tree predictive model approach to predict the missing
# values for LotFrontage and Electrical. The algorithm used is "cart" which stands 
# for "Classification And Regression Trees".

imputedValues <- mice(data=tr
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)
# impute the missing values in our tr data.frame
tr <- mice::complete(imputedValues,1) # completely fills in the missing

imputedValuesTe <- mice(data=te
                      , seed=2016     # keep to replicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)
# impute the missing values in our tr data.frame
te <- mice::complete(imputedValuesTe,1) # completely fills in the missing

# LotFrontage and Electrical features no longer have missing values
sapply(tr, function(x) sum(is.na(x)))
sapply(te, function(x) sum(is.na(x)))

# create a dataset called d so I don't have to change alot of code later in 
# in my script
d <- tr
names(d)
# Make target variable first column in dataset
d <- d[,c(75,1:74)]

# MakeNULL# Make target (SalePrice) column name "y"
names(d)[1] <- "y"
names(d)


# clean up my R environment
rm( drv, i, realBlanks, s, imputedValues)
################################################################################
## Predictive Modeling using caret
################################################################################
library(caret)
set.seed(1234)

# Using the dummyVars() function, create one-hot encoded variables for all
# the categorical variables. Do this for both the 'd' data.frame and 'te' set

names(d)
names(te)

#for d set
d = d %>% select(-(Utilities))
dummiesD = dummyVars(y ~ ., data=d)
ex = data.frame(predict(dummiesD, newdata = d))
names(ex) = gsub("\\.","",names(ex))
d = cbind(d$y, ex)


#for te set
str(te)
te = te %>% select(-(Utilities)) 
dummiesTe = dummyVars(Id ~ ., data=te)
ex1 = data.frame(predict(dummiesTe, newdata = te))
names(ex1) = gsub("\\.","",names(ex1))
te = cbind(te$Id, ex1)
rm(dummiesD, ex, dummiesTe, ex1)

# Identified correlated variables in the 'd' dataset that are less than or 
# greater than 80% and remove them. 

descCor = cor(d[,2:ncol(d)])
highlyCorDescr = findCorrelation(descCor, cutoff = 0.8)
filteredDesc = d[,2:ncol(d)][,-highlyCorDescr]
descCor2 = cor(filteredDesc)
summary(descCor2[upper.tri(descCor2)])
names(d)[1] = "y"
d = cbind(d$y, filteredDesc)
names(d)[1] = "y"

rm(descCor, highlyCorDescr, filteredDesc, descCor2)

# identified linear dependencies and removed them to reduce the issue of 
# perfect collinearity using the findLinearCombos() function.

y= d$y
d = cbind(rep(1, nrow(d)), d[2:ncol(d)])
names(d)[1] = "ones"
comboInfo = findLinearCombos(d)
d = d[, - comboInfo$remove]
d = d[,c(2:ncol(d))]
d = cbind(y , d)

rm(comboInfo)

# Removed features with limited variation using nearZeroVar() from the 'd'
# data.frame

nzv = nearZeroVar(d, saveMetrics = T)
head(nzv)
d = d[, c(T,!nzv$nzv[2:ncol(d)])]
rm(nzv)

# using preProcess(), standardized numeric features using a min-max 
# normalization. Do this for both the 'd' and 'te' data sets. 

# training numeric columns
tr_numeric_cols <- intersect(names(d), tr_integer_columns)


#for "d" data
preProcValues <- preProcess(d[,tr_numeric_cols]
                            , method = c("center","scale"))
d <- predict(preProcValues, d)

#for "te" data
preProcValues <- preProcess(te[,tr_numeric_cols]
                            , method = c("center","scale"))
te <- predict(preProcValues, te)

# Creating a 90/10 train/test set using createDataPartition().

set.seed(1234)
inTrain = createDataPartition(y = d$y, p = 0.9, list = F)
train = d[inTrain,]
test = d[-inTrain,]
?createDataPartition
#the function requires the target variable column to be specified because the variable
#is the column that will be divided into the test and train set. The kind of random sampling
# is stratified random sampling

# Using trainControl(), specifying a 5-fold cross-validation design.

ctrl = trainControl(method="cv", number = 5, classProbs = F, 
                    summaryFunction = defaultSummary, allowParallel = T)

# training a linear regession model on the train set using the train() function.
# Call your model 'm1'. Use RMSE as your metric.

m1 = train(y~.,data=train, method = "lm", trControl = ctrl, metric = "RMSE")
m1
summary(m1)

# training a linear regession model on the train set using the train() function.
# However, for this model attempting to take the log(y) as your target variable when
# you train your model. Call your model 'm2'. Using RMSE as your metric.

m2 = train(log(y)~.,data=train, method = "lm", trControl = ctrl, metric = "RMSE")
m2
summary(m2)

# using the defaultSummary() function, evaluated the performance of the train and 
# test sets for both models.

options(scipen=999)

#m1 train
defaultSummary(data=data.frame(obs=train$y, pred=predict(m1, newdata=train))
               , model=m1)
#m1 test
defaultSummary(data=data.frame(obs=test$y, pred=predict(m1, newdata=test))
               , model=m1)
#m2 train
defaultSummary(data=data.frame(obs=train$y, pred=predict(m2, newdata=train))
               , model=m2)
#m2 test
defaultSummary(data=data.frame(obs=test$y, pred=predict(m2, newdata=test))
               , model=m2)
#Even though the gap for Rsquared is closer with m2, the distance is not signifcantly greater
# than that of m1, additionally given that m1 has a greater Rsquared indicates a better fit

# generate predictions using the predict() function on the 'te' data.frame
# using the best model you found from the previous question. These values are
# saved in a numeric vector called 'preds'. Created a new data.frame
# called 'results' that has the 'Id' column from the 'te' data.frame in the first
# column and 'preds' in the second column. The column names for the results table
# should be c("Id","SalePrice"). Lastly, wrote this out into a comma-seperated
# file called "results.csv" using the write.table() function, to ensure Kaggle submission


names(te)[1] = "Id"

preds = predict(m1, newdata = te)

results = data.frame(Id = te$Id, SalePrice = preds)

names(results) = c("Id","SalePrice")

write.table(results, file="results.csv", quote = F, row.names = F, sep = ",")
getwd()
rm(y, s, realBlanks, preds, i, highlyCorDescr, preProcValues, preProcValuesTest, nzv, imputedValues, imputedValuesTe, filteredDesc, ex, ex1, dummiesD, dummiesTe, descCor, descCor2, comboInfo, ctrl)

################################################################################
# Non h2o submission based on steps above 

#Rank: 3330 Team: Matt Fiorini  Score: 0.15526 Entries: 3 Last: 27s

################################################################################
## Clustering
#
# Assuming your properly standardized your features using a min-max normalization
# in the previous prediction problem above, you are going to perform clustering here
# using the train and test set you already created.
################################################################################

# writing a for loop that performs k-means clustering on your train set, as
# well as your test set you created from your previous problems. The loop should
# run from k=1 to 10. Have the number of random starts be 25 and the maximum number 
# of algorithm iterations set at 100. Plot the total within sum of squared errors
# in an elbox plot. Use a seed of 123.

#run kmeans for diff values of k so we can identify performance by # of clusters
set.seed(123)  # set this to replicate results

cost_df <- data.frame() #accumulator for cost results
cost_df
for(k in 1:10){
  
  # train set
  kmeans_tr <- kmeans(x=train[,2:ncol(train)], centers=k, nstart=25, iter.max=100)
  
  # test set
  kmeans_te <- kmeans(x=test[,2:ncol(test)], centers=k, nstart=5, iter.max=100)
  
  # combine cluster number and cost together, write to cost_df
  cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss
                                  , kmeans_te$tot.withinss))
}

names(cost_df) = c("cluster","train","test")
cost_df
par(mfrow=c(1,1))
plot(x=cost_df$cluster, y=cost_df$train, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="SSE")
points(x=cost_df$cluster, y=cost_df$test, col="red", pch=19)

#clustering in the train and test set does not appear to be able to replicate, given the
# broad difference between the SSE between the train and test set. This degree of
#difference leads to the conclusion that the results are not replicable

# Writing another for loop that runs k-means algorithm on the entire 'd' dataset on
# the input variables. Number of starts and max iterations be the same as in the 
 #previous problem. Run this for k=1 to 100. Based on your elbow plot, identify 
# two possible k values that might lead to a good clustering. 

kmeansd = data.frame()
for(i in 1:100){
  kmeans_d <- kmeans(x=d[,2:ncol(d)], centers=i, nstart=25, iter.max=100)
  kmeansd = rbind(kmeansd, cbind(i,kmeans_d$tot.withinss))
}
names(kmeansd) = c("cluster","d")
rm(m1, m2, results, testD, trainD, tr_factors, inTrain, kmean_te, kmeans_tr)
plot(x=kmeansd$cluster, y=kmeansd$d, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="SSE")
#based on the positioning of the elbow, I've decided to select 4 and 7 for the k value.
#these both represent what appears to be the upper and lower range of what the true
#k value should be

#Perform k-means clustering on the 'd' dataset

kmeans_4 <- kmeans(x=d[,2:ncol(d)], centers=4, nstart=25, iter.max=100)
kmeans_7 <- kmeans(x=d[,2:ncol(d)], centers=7, nstart=25, iter.max=100)

# k=4 k-means model
kmeans_4$centers

# k=7 k-means model
kmeans_7$centers

#Creating silhoutte plots for the two k's you identified previously using the 
# same settings used previously. Based on the output of these plotsWe would chose
#to go with 4 clusters over 7, since the average width is greater for
# 4 cluster than for 7 clusters

install.packages("cluster")
library(cluster)

# kmeans (k=4)
km4 <- kmeans(x=d[,2:ncol(d)], centers=4, nstart=25, iter.max=100)
dist4 <- dist(x=d[,2:ncol(d)], method="euclidean")
sil4 <- silhouette(x=km4$cluster, dist=dist4)

# kmeans (k=7)
km7 <- kmeans(x=d[,2:ncol(d)], centers=7, nstart=25, iter.max=100)
dist7 <- dist(x=d[,2:ncol(d)], method="euclidean")
sil7 <- silhouette(x=km7$cluster, dist=dist7)


# silhoutte plots
par(mfrow=c(1,2))
install.packages("wesanderson")
library(wesanderson)
plot(sil4, col=wes_palette("Zissou1", 4, type = "continuous")
     , main="Silhouette plot (k=4) K-means", border=NA)
plot(sil7, col=wes_palette("Zissou1", 7, type = "continuous")
     , main="Silhouette plot (k=7) K-means", border=NA)
rm(km4,dist4, sil4, km7, dist7, sil7)

################################################################################
## Predictive Modeling using H2O
################################################################################

################################################################################
#Initialize h2o cluster, loaded in 'd' data.frame into the h2o cluster.

install.packages("h2o")
library(h2o)
h2o.init(nthreads = 12, max_mem_size = "64g")

data = as.h2o(d)

# Train a Random Forest, Deep Learning, and Gradient Boosting Machine model.
# Using h2o.performance() show the performance on the train and test sets for all 
# three models. Outputs included in the script and optimal fit was determined by RMSE

y = "y"
x = setdiff(names(data),y)
parts = h2o.splitFrame(data, 0.8, seed = 99)
train = parts[[1]]
test = parts[[2]]
#random forest
rf_model = h2o.randomForest(y=y, training_frame = train, seed = 99)

(rfTrain_perform = h2o.performance(model = rf_model, newdata = train))
#rf train output:
#H2ORegressionMetrics: drf

#MSE:  138064164
#RMSE:  11750.07
#MAE:  6759.2
#RMSLE:  0.06284162
#Mean Residual Deviance :  138064164

(rfTest_perform = h2o.performance(model = rf_model, newdata = test))
#rf test output:
#H2ORegressionMetrics: drf

#MSE:  1124407677
#RMSE:  33532.19
#MAE:  17557.61
#RMSLE:  0.1541309
#Mean Residual Deviance :  1124407677

#difference of 21782.12 between train and test set for RMSE
#difference of 0.0913 for RMSLE, which helps standardize the value.
#based on the rule of being within 10% to determine overfitting, this falls within the range
#which would make this model a candidate model to use, but still not optimal

#deep learning 
deepLearningTrain = h2o.deeplearning(y=y, training_frame = train, seed = 99)

(dlTrain_perform = h2o.performance(model = deepLearningTrain, newdata = train))
#dl train output:
#H2ORegressionMetrics: deeplearning

#MSE:  252451303
#RMSE:  15888.72
#MAE:  12545.83
#RMSLE:  0.09497865
#Mean Residual Deviance :  252451303

(dlTest_perform = h2o.performance(model = deepLearningTrain, newdata = test))

#dl test output:
#H2ORegressionMetrics: deeplearning

#MSE:  1117290247
#RMSE:  33425.89
#MAE:  20211.65
#RMSLE:  0.1610168
#Mean Residual Deviance :  1117290247

#RMSE difference of 17537.17, with RMSLE difference of 0.06603815, which would make
# the dl model a candidate model

#gradient boost
gbmTrain = h2o.gbm(y = y, training_frame = train, seed = 99)

(gbmTrain_perform = h2o.performance(model = gbmTrain, newdata = train))
#gbm train output:
#H2ORegressionMetrics: gbm

#MSE:  249743992
#RMSE:  15803.29
#MAE:  10636.67
#RMSLE:  0.09129595
#Mean Residual Deviance :  249743992

(gbmTest_perform = h2o.performance(model = gbmTrain, newdata = test))
#gbm test output:
#H2ORegressionMetrics: gbm

#MSE:  1153286270
#RMSE:  33960.07
#MAE:  18020.29
#RMSLE:  0.1530394
#Mean Residual Deviance :  1153286270

#RMSE difference of 18156.78, RMSLE difference of 0.06174345. Which would make the
# gbm a candidate model

# Based on the standardized RMSLE value, all 3 models are candidate models (as all 
#RMSLE fall within 10% of train/test). Given the value for determining optimal fit 
#is based on the RMSE is the Deep Learning model, given that number is the lowest
#between the train and test models. However if RMSE was not the determining variable
#for optimal model, there would be an emphasis towards RMSLE as the determining variable
# for the optimal model, given its ability to effectively interpret larger outlier data.
# Since we are looking at house prices, where there are signifcant outliers, the RMSLE
# variable may be more beneficial to use when determining the optimal model to select


# In both the train and test sets, transform the 'y' target column to 
# log(y). Then re-train a gradient boosting machine model. Comment out, but show
# the performance on the train and test sets.

train$y <- log(train$y)
test$y <- log(test$y)
gbmLogTrain = h2o.gbm(y = y, training_frame = train, seed = 99)

(gbmLogTrain_perform = h2o.performance(model = gbmLogTrain, newdata = train))

#log(y) gbm train output:
#H2ORegressionMetrics: gbm

#MSE:  0.005945379
#RMSE:  0.07710628
#MAE:  0.05546053
#RMSLE:  0.005994982
#Mean Residual Deviance :  0.005945379

(gbmLogTest_perform = h2o.performance(model = gbmLogTrain, newdata = test))

#log(y) gbm test output:
#H2ORegressionMetrics: gbm

#MSE:  0.02225988
#RMSE:  0.1491975
#MAE:  0.1016772
#RMSLE:  0.01156151
#Mean Residual Deviance :  0.02225988

#The difference of RMSE between the train/test models is 0.07209122, which would
# make this model a candadte model as it falls within the 10% range of determining
#overfit

# Using h2o's driverless AI functionality by using auto.ml. Set the max run
# time (in seconds) to at least 300 (5 minutes). Once the the best (i.e. champion)
# was obtained, generate predictions on the 'te' dataset using that model.
# Write our these predictions into a file called "h2o_results.csv". Report your score.

auto <- h2o.automl(x, y, train, max_runtime_secs=300)
auto
str(auto)
names(te)[1] = "Id"
data2 <- as.h2o(te)
names(data2)

# make predictions
p <- h2o.predict(auto, data2)
p <- as.data.frame(p)
head(p)
names(p) <- "predict"

h2o_results <- data.frame(Id=te$Id, SalePrice=p$predict)
write.table(x=h2o_results, sep=",", file="h2o_results.csv", row.names=F)

#Kaggle submission score
#Rank: 1571 Name: Matt Fiorini Score:0.13622 Submission Count:4 Last:21s
