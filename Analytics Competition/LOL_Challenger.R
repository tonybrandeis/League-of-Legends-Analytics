library(corrplot)
library(randomForest)
data = read.csv("Challenger_Ranked_Games.csv")
head(data)
View(data)

####get rid of game ID
data = data[,-1]

####correaltion and colums removal
M = cor(data)
corrplot(M)

colnames(data)
M[upper.tri(M)] <- 0
diag(M) <- 0

# Above two commands can be replaced with 
# M[!lower.tri(M)] <- 0


data.new <- data[, !apply(M, 2, function(x) any(abs(x) > 0.99, na.rm = TRUE))]
head(data.new)
ncol(data.new) - ncol(data) #6 columns removed due to high correaltion 
data.new$blueWins = data$blueWins #adding back bluewins


#### split train and test
dataA = sample(c(TRUE, FALSE), nrow(data.new), replace=TRUE, prob=c(0.7,0.3))
train = data.new[dataA, ]
test  = data.new[!dataA, ]

#### train blue and train red
datablue = subset(train, select = -c(redWins))
datared = subset(train, select = -c(blueWins))
testblue = subset(test, select = -c(redWins))
testred = subset(test, select = -c(blueWins))


summary(data)

####logistics models
logblue = glm(blueWins==1 ~ ., data = datablue, family = "binomial")
logred = glm(redWins==1 ~ ., data = datared, family = "binomial")
summary(logblue)
summary(logred)

#### predicting test, add estimates to last column
testblue$log_pred = predict(logblue, testblue, type="response")
testred$log_pred = predict(logred, testred, type="response")

####random forest blue
blue_RF = randomForest(blueWins ~ ., data = datablue, nodesize= 5, ntree = 500)
summary(blue_RF)

testblue$RF_pred = predict(blue_RF, testblue, type = "response")



# Confusion Matrix
confusion_mtx_blue = table(testblue[, 43], testblue$RF_pred)
View(confusion_mtx_blue)

# Plotting model
plot(blue_RF)

# Importance plot
importance(blue_RF)

# Variable importance plot
varImpPlot(blue_RF)


####random forest red
red_RF = randomForest(redWins ~ ., data = datared, nodesize= 5, ntree = 500)
summary(red_RF)

testred$RF_pred = predict(red_RF, testred, type = "response")
View(testred)


# Confusion Matrix
confusion_mtx_red = table(testred[, 43], rf_red_pred)
View(confusion_mtx_red)

# Plotting model
plot(red_RF)

# Importance plot
importance(red_RF)

# Variable importance plot
varImpPlot(red_RF)


####Accuracy
# setting the accuracy variance threshold at 0.2
accuracy_log = 0
accuracy_RF = 0
for (i in 1:nrow(testblue)){
  if(abs(testblue[i, "blueWins"] - testblue[i,"log_pred"]) < 0.01){
    accuracy_log = accuracy_log +1
  }
  if(abs(testred[i, "redWins"] - testred[i,"log_pred"]) < 0.01){
    accuracy_log = accuracy_log +1
  }
  if(abs(testblue[i, "blueWins"] - testblue[i,"RF_pred"]) < 0.01){
    accuracy_RF = accuracy_RF +1
  }
  if(abs(testred[i, "redWins"] - testred[i,"RF_pred"]) < 0.01){
    accuracy_RF = accuracy_RF +1
  }
}

accuracy_log
accuracy_RF
