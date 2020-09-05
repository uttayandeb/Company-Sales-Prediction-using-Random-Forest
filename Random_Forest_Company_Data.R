
############ Packages Required ###################

install.packages("randomForest")
library(randomForest)
install.packages("MASS")
library(MASS)
install.packages("caret")
library(caret)

########### Reading and understanding the data #############

set.seed(123)#for getting same result everytime

Company_Data<-read.csv(file.choose())
View(Company_Data)
names(Company_Data)
nrow(Company_Data)
ncol(Company_Data)

str(Company_Data)
summary(Company_Data)


######## Exploratory Data analysis ###########

hist(Company_Data$Sales,main = "sales of Companydata",xlim= c(0,20),breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))

mean(Company_Data$Sales)#[1] 7.496325


highsales = ifelse(Company_Data$Sales<9, "No", "Yes")  # if less then 9 then sales is low neither its high
CD = data.frame(Company_Data[2:11], highsales)#removing the "sales" column and adding a extra column of "highsales"

View(CD)
str(CD)

table(CD$highsales)############ No Yes 
                              #  286 114 





######### Data Partition or Splitting of Data #############

set.seed(123)### for getting same results everytime

ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))

train <- CD[ind==1,]
nrow(train)###[1] 285

test  <- CD[ind==2,]
nrow(test)#####[1] 115

set.seed(213)
rf <- randomForest(highsales~., data=train)
rf  # Description of the random forest with no of trees

attributes(rf)


#$$$$  Prediction and Confusion Matrix for Training data  $$$$$$#
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)

confusionMatrix(pred1, train$highsales)
# more than 95% Confidence Interval(CI) 
# Sensitivity for Yes and No is 100 % 
## accuracy is also 1 ie 100%



#$$$$ Prediction with test data and confusion matrix$$$$$#

pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales) # ( Accuracy : 0.8435 ) ie 84 %



##### Error Rate in Random Forest Model ####
plot(rf)




# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)


rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1



pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales) ###Accuracy : 1 ie 100%
# Around 98% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 



# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales) #Accuracy : 0.8348 ,95% CI

#***** no of nodes of trees****#

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
# from the graph majority of the trees has an average number of 45 to 50 nodes. 





# Variable Importance 

varImpPlot(rf1)
# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say ShelveLoc is the most important variable for prediction.on looking at population,it has no value.
# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Price is very important and Urban is not that important.



varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")


# Quantitative values 
importance(rf1)

varUsed(rf)## to c which predictor variables are actually used in the random forest.


# Partial Dependence Plot 
partialPlot(rf1, train, Price, "Yes")
# On that graph, we can see that if the price is 100 or greater, than they are not buying those computers.



# Extract single tree from the forest 

getTree(rf, 1, labelVar = TRUE)



# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, CD$highsales)



