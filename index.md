---
title: "Practical Machine Learning Course Project2"
author: "Peter D"
date: "2/3/2021"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---
### Synopsis

This project uses data from accelerometers on the belt, forearm, arm, and dumbell of 4 participants asked to perform barbell lifts correctly and incorrectly in 5 different ways. The data come from this source: http://groupware.les.inf.puc-rio.br/har. The goal is to predict the 5 different ways the participants performed the exercise, which is the 'classe' variable. The model is then used to predict 20 different test cases. 

Before training the model, only accelerometer data were kept. Omitted were columns 1-7 that contained descriptive data. Also omitted were columns that were not the actual accelerometer data but were statistics generated from the accelerometer data.

Random forests took too much computing time. Stochastic Gradient Boosting (gbm) with the train function in the caret package required less than an hour of computing time.

The in-sample accuracy for the gbm model using 5-fold cross-validation was 0.9638197. The out-of-sample accuracy for predicting the validation set was 0.9558199.

### Load libraries

```r
library(caret); library(gbm); library(tidyverse)
```

### Load data


```r
training <- read.csv("pml-training.csv",
               header=TRUE, check.names=TRUE, stringsAsFactors=F,
               skip = 0, na.strings=c("","NA", "null"))
testing <- read.csv("pml-testing.csv",
               header=TRUE, check.names=TRUE, stringsAsFactors=F,
               skip = 0, na.strings=c("","NA", "null"))
```

### Preliminary data wrangling
Remove variables that are almost all NAs (i.e., 19216/19622 NAs; ~98%) that are statistics of the sensor readings, while the readings themselves are the predictors. Also, removed the first 7 columns that are not sensor readings.

```r
set.seed(333)
# Training set: Remove mostly-NA variables from the 
no.NA <- colSums(is.na(training)) # show columns with mostly NA (i.e., the calculated statistics)
logical.no.NA <- no.NA==0 # make logical vector of columns to keep 
training.no.NA <- training[,logical.no.NA] # selects all columns with zero NA
training <- training.no.NA %>% select(-c(1:7)) # remove irrelevant variables

# From testing, remove columns that are mostly NA and unimportant (i.e., 1:7)
no.NA <- colSums(is.na(testing)) # show columns that are mostly NA (i.e., the calculated statistics)
logical.no.NA <- no.NA==0 # make logical vector of columns to keep 
testing.no.NA <- testing[,logical.no.NA] # selects all columns with zero NA
testing <- testing.no.NA %>% select(-c(1:7)) # remove irrelevant variables
```

### Divide data into building and validation data sets

```r
inBuild <- createDataPartition(y=training$classe, p=0.7 , list=FALSE )
# the validation set
validation <- training[-inBuild,] # validation has 0.3 of the data
# build
training.wo.validate <- training[inBuild,] # this is 0.7 of the data
```

### Train the model

```r
# train the gbm model using Stochastic Gradient Boosting
train_control <- trainControl('cv', 5) # set cross-validation
mod.gbm <- train(classe~., method="gbm", trControl=train_control, data=training.wo.validate, verbose=FALSE)
```

### Look at the model results

```r
saveRDS(mod.gbm, "mod.gbm.rds")
mod.gbm <- readRDS("mod.gbm.rds") # not running above lines to save time
print(mod.gbm)
```

```
## Stochastic Gradient Boosting 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 10988, 10990, 10991, 10989, 10990 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  Accuracy   Kappa    
##   1                   50      0.7535122  0.6876528
##   1                  100      0.8201928  0.7724223
##   1                  150      0.8528066  0.8137362
##   2                   50      0.8553548  0.8167761
##   2                  100      0.9063108  0.8814379
##   2                  150      0.9309882  0.9126757
##   3                   50      0.8960466  0.8683973
##   3                  100      0.9416899  0.9262105
##   3                  150      0.9638197  0.9542248
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.1
## 
## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 150, interaction.depth =
##  3, shrinkage = 0.1 and n.minobsinnode = 10.
```

<<<<<<< HEAD
### Look at the variable importance

```r
mod.gbm.Imp <- varImp(mod.gbm, scale = FALSE)
mod.gbm.Imp
```

```
## gbm variable importance
## 
##   only 20 most important variables shown (out of 52)
## 
##                   Overall
## roll_belt          3117.1
## pitch_forearm      1620.0
## yaw_belt           1577.8
## magnet_dumbbell_z  1188.0
## magnet_dumbbell_y   949.4
## roll_forearm        871.4
## pitch_belt          847.8
## magnet_belt_z       627.9
## gyros_belt_z        494.6
## accel_forearm_x     449.0
## roll_dumbbell       433.5
## accel_dumbbell_y    353.2
## magnet_forearm_z    347.1
## accel_forearm_z     321.2
## gyros_dumbbell_y    301.8
## yaw_arm             292.4
## magnet_dumbbell_x   285.4
## magnet_belt_x       265.8
## accel_dumbbell_z    257.9
## accel_dumbbell_x    240.4
```

### Use the model to predict 'classe' in the validation set

```r
# predict the gbm model on validation set
p.gbm <- predict(mod.gbm, newdata = validation)
# Gives the gbm accuracy of the prediction (i.e., out-of-sample error)
postResample(pred = p.gbm, obs = as.factor(validation$classe))
```

```
##  Accuracy     Kappa 
## 0.9558199 0.9441123
```

```r
# Here is another way to get prediction error in a way that is understandable
equalPredictions <- (p.gbm==validation$classe) # percent correct predictions 
AccuracyOfPrediction <- 1-(sum(equalPredictions==F)/sum(equalPredictions==T))
AccuracyOfPrediction
```

```
## [1] 0.9537778
```

### Predict 'classe' for the test cases

```r
# predict the gbm model on validation set
p.gbm <- predict(mod.gbm, newdata = testing)
p.gbm
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
