library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(caTools)
library(corrplot)
library(xgboost)
library(Matrix)
library(magrittr)
library(caret)
library(methods)
library(Metrics)
library(MASS)
library(GGally)
library(car)
library(rpart.plot)
lubridate:::.get_locale_regs()

#1 Read in Data
setwd("C:\\Users\\Chin\\Desktop\\R\\Data Sets\\BC2406")
store.dt <- fread('Team 5 store.csv')
train.dt <- fread('Team 5 train.csv')

train.dt$Date <- ymd(train.dt$Date)

#joining of train and store by storeID
train.dt <- left_join(train.dt,store.dt,by="Store")
setDT(train.dt)

#set class
train.dt$DayOfWeek = as.factor(train.dt$DayOfWeek)
train.dt$Promo2 = as.factor(train.dt$Promo2)
train.dt$StoreType = as.factor(train.dt$StoreType)
train.dt$Assortment = as.factor(train.dt$Assortment)
train.dt$DayOfWeek = as.factor(train.dt$DayOfWeek)
train.dt$Store = as.factor(train.dt$Store)
train.dt$PromoInterval = as.factor(train.dt$PromoInterval)
train.dt$StateHoliday=as.factor(train.dt$StateHoliday)

#Feature Engineering for train.dt

#creating new promosinceint feature
train.dt$Date <- ymd(train.dt$Date)
train.dt[,Promo2sinceDay:=1,]
train.dt[Promo2==0,Promo2sinceDay:=NA,]
train.dt[,Promo2SinceDate:=paste(train.dt$Promo2sinceDay,(floor(train.dt$Promo2SinceWeek/4)+1),train.dt$Promo2SinceYear,sep="-"),]
train.dt$Promo2SinceDate<-dmy(train.dt$Promo2SinceDate)
train.dt[,Promosinceint:=(Date-Promo2SinceDate)]

#creating new competitionsinceint feature
train.dt[,CompetitionOpenSinceDay:=1,]
train.dt[is.na(CompetitionOpenSinceMonth),CompetitionOpenSinceDay:=NA,]
train.dt[,CompetitionSinceDate:=paste(train.dt$CompetitionOpenSinceDay,CompetitionOpenSinceMonth,CompetitionOpenSinceYear,sep="-"),]
train.dt$CompetitionSinceDate<-dmy(train.dt$CompetitionSinceDate)
train.dt[,Competitionsinceint:=(Date-CompetitionSinceDate)]
train.dt[,CompetitionOpenSinceDay:=NULL,]
train.dt[,Promo2sinceDay:=NULL,]

#creating new is... feature
train.dt<-train.dt[order(Store,-Date)]
train.dt[,isweekendyest:=ifelse(DayOfWeek==1,1,0),]
train.dt[,isweekendtmr:=ifelse(DayOfWeek==5,1,0),]
train.dt[,isweekend:=ifelse(DayOfWeek==6|DayOfWeek==7,1,0),]
train.dt[,month:=as.numeric(format(Date,format="%m"))]
train.dt[,year:=as.numeric(format(Date,format="%Y"))]

train.dt[,isclosetmr:=ifelse(lag(Open)==0,1,0)]
train.dt[is.na(isclosetmr),isclosetmr:=0]
train.dt[,iscloseyest:=ifelse(lead(Open)==0,1,0)]
train.dt[is.na(iscloseyest),isclosetmr:=0]

train.dt[,isschholtmr:=ifelse(lag(SchoolHoliday)==1,1,0)]
train.dt[is.na(isschholtmr),isschholtmr:=0]
train.dt[,isschholyest:=ifelse(lead(SchoolHoliday)==1,1,0)]
train.dt[is.na(isschholyest),isschholyest:=0]


#Creating a feature of how many months since the last Long Promo Duration (Promo2)
table(train.dt$PromoInterval,train.dt$month)
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & month == 1, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & month == 4, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & month == 7, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & month == 10, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & (month < 4 & month != 1), SinceLastPromo2 := month - 1]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & (month > 4 & month < 7), SinceLastPromo2 := month - 4]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & (month > 7 & month < 10), SinceLastPromo2 := month - 7]
train.dt[PromoInterval == "Jan,Apr,Jul,Oct" & is.na(SinceLastPromo2), SinceLastPromo2 := month - 10]

train.dt[PromoInterval == "Feb,May,Aug,Nov" & month == 2, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & month == 5, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & month == 8, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & month == 11, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & (month < 5 & month != 2), SinceLastPromo2 := month - 2]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & (month > 5 & month < 8), SinceLastPromo2 := month - 5]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & (month > 8 & month < 11), SinceLastPromo2 := month - 8]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & month == 1, SinceLastPromo2 := 2]
train.dt[PromoInterval == "Feb,May,Aug,Nov" & month == 12, SinceLastPromo2 := 1]

train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & month == 3, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & month == 6, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & month == 9, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & month == 12, SinceLastPromo2 := 0]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & (month < 6 & month != 3), SinceLastPromo2 := month - 3]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & (month > 6 & month < 9), SinceLastPromo2 := month - 6]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & (month > 9 & month < 12), SinceLastPromo2 := month - 9]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & month == 1, SinceLastPromo2 := 1]
train.dt[PromoInterval == "Mar,Jun,Sept,Dec" & month == 2, SinceLastPromo2 := 2]

#Checking if any errors in the new factor
train.dt[is.na(SinceLastPromo2), .N]
train.dt[PromoInterval=='', .N]
summary(train.dt$SinceLastPromo2)

#generates a bar chart 
options(scipen=100)
ggplot(data=train.dt, aes(x=StoreType)) + geom_bar(fill="blue")
barplot(table(train.dt$Open),  main="Proportion of Closed Stores vs. Open Stores", col = c("lightblue", "mistyrose"), names.arg=c("Closed","Open"))
train.dt[Open==1, .N]
train.dt[Open==0, .N]
options(scipen=0)

#Select only 4500 training lines
train.dt<-train.dt[Sales>0&StoreType=="d"][order(-Date)]
totsales<-train.dt[,.(logmeansales=mean(log(Sales))),by=Store] 
#sort store by mean log daily sales
plot(density(totsales$logmeansales)) #visualise log sales

#boxplot of sales
ggplot(data=train.dt, aes(x="", y=Sales)) + 
  geom_boxplot(fill = "gold1", color="red2") + ggtitle("Boxplot of Sales")

#to split into 12 categories, we categorise the data into 12 divisions of logsales
#corresponding to 7.7, 15.4, 23.1, 30.8, 38.5, 46.2, 53.8, 61.5, 69.2, 76.9, 84.6, 92.3 percentiles


#from the subset of 348 stores, this corresponds to the
# 26th, 54th, 80th, 107th, 134th, 161st, 187th, 214th, 241st, 268th, 
# 294th and 321st store 
#in order of descending sales
totsales<-totsales[order(-logmeansales)]
totsales[26] #739
totsales[54] #298
totsales[80] #620
totsales[107] #922
totsales[134] #185
totsales[161] #887
totsales[187] #637
totsales[214] #202
totsales[241] #547
totsales[268] #334
totsales[294] #495
totsales[321] #584

#subset stores
train.dt<-train.dt[Store==739|Store==298|Store==620|Store==922|Store==185|Store==887|Store==637|Store==202|Store==547|Store==334|Store==495|Store==584]
#check for even distribution
train.dt[,.N,by=Store] #satisfactory
#label subcategories: 1 == most sales, 12 == least sales
train.dt$cat<-0
train.dt[, cat := ifelse(Store==739, 1, train.dt$cat)]
train.dt[, cat := ifelse(Store==298, 2, train.dt$cat)]
train.dt[, cat := ifelse(Store==620, 3, train.dt$cat)]
train.dt[, cat := ifelse(Store==922, 4, train.dt$cat)]
train.dt[, cat := ifelse(Store==185, 5, train.dt$cat)]
train.dt[, cat := ifelse(Store==887, 6, train.dt$cat)]
train.dt[, cat := ifelse(Store==637, 7, train.dt$cat)]
train.dt[, cat := ifelse(Store==202, 8, train.dt$cat)]
train.dt[, cat := ifelse(Store==547, 9, train.dt$cat)]
train.dt[, cat := ifelse(Store==334, 10, train.dt$cat)]
train.dt[, cat := ifelse(Store==495, 11, train.dt$cat)]
train.dt[, cat := ifelse(Store==584, 12, train.dt$cat)]
train.dt$cat<-factor(train.dt$cat)
train.dt<-train.dt[1:4500] #take most recent 4500 for processing purposes


#We create a function to calculate the number of days since a particular store has held a day-long promotion (Promo).
#The input parameters are the datatable itself, the storeno, and an offset called rowoffset.
#How the functions works is that using a variable called value, value will count how many days it has been since that
#store has held a Promo for that particular row entry. If Promo==1 for that entry, then the value is reset back to 0 and
#DaysSincePromo is also 0 for that row. Else if Promo==0, then the value is rolling and continues to add up how many days it has been since the last Promo.
#The function then iterates through for all rows of that particular store in the datatable. How it knows from which row to which row
#in the datatable is aided using the offset, which is a static variable that keeps track of which rows we have already itereated through.
train.dt[,DaysSincePromo:=0]
train.dt<-train.dt[order(Store,Date)]

f <- function(datatable,storeno,rowoffset){
  value <- 0
  counter <- datatable[Store == storeno, .N]
  for(i in 1:counter){
    if(train.dt[i+rowoffset,Promo]==1){
      value <- 0
    }
    else{
      train.dt[i+rowoffset, DaysSincePromo:=value+1]
      value <- value + 1
    }
    offset <<- offset + 1
  }
}

#We get the unique store IDs so that we can throw them into our function.
unique(train.dt$Store)
offset = 0
f(train.dt,185,0)
f(train.dt,202,offset)
f(train.dt,298,offset)
f(train.dt,334,offset)
f(train.dt,495,offset)
f(train.dt,547,offset)
f(train.dt,584,offset)
f(train.dt,620,offset)
f(train.dt,637,offset)
f(train.dt,739,offset)
f(train.dt,887,offset)
f(train.dt,922,offset)
train.dt[Promo==1, .N]
train.dt[DaysSincePromo==0,.N]

#boxplot after splitting
ggplot(data=train.dt, aes(x="", y=Sales)) + geom_boxplot(fill = "gold1", color="red2")
+ ggtitle("Boxplot of Sales") 

#deleting unused cols
train.dt$Customers=NULL
train.dt$CompetitionOpenSinceMonth=NULL
train.dt$CompetitionOpenSinceYear=NULL
train.dt$Promo2=NULL
train.dt$Promo2SinceDate=NULL
train.dt$Promo2SinceWeek=NULL
train.dt$Promo2SinceYear=NULL
train.dt$CompetitionSinceDate=NULL
train.dt$StoreType=NULL
unique(train.dt$Open)

#setting to factors 
train_linear <- train.dt
train_linear$Promo = factor(train_linear$Promo)
train_linear$isclosetmr = factor(train_linear$isclosetmr)
train_linear$iscloseyest = factor(train_linear$iscloseyest)
train_linear$StateHoliday = factor(train_linear$StateHoliday)
train_linear$SchoolHoliday = factor(train_linear$SchoolHoliday)
train_linear$isweekendyest = factor(train_linear$isweekendyest)
train_linear$isweekendtmr = factor(train_linear$isweekendtmr)
train_linear$isweekend = factor(train_linear$isweekend)
train_linear$month = factor(train_linear$month)
train_linear$year = factor(train_linear$year)
train_linear$isschholtmr = factor(train_linear$isschholtmr)
train_linear$isschholyest = factor(train_linear$isschholyest)
train_linear$Open=NULL

train_linear[order(-Date)]

train.dt <- train_linear

#renaming the above to remove the commas
train.dt[["PromoInterval"]] <- as.character(train.dt[["PromoInterval"]])
train.dt[train.dt == "Mar,Jun,Sept,Dec"] = "MarJunSeptDec"
train.dt[train.dt == "Feb,May,Aug,Nov"] = "FebMayAugNov"
train.dt[train.dt == "Jan,Apr,Jul,Oct"] = "JanAprJulOct"
train.dt[["PromoInterval"]] <- as.factor(train.dt[["PromoInterval"]])
levels(train.dt[["PromoInterval"]])

#find out which columns have NAs
colSums(is.na(train.dt)) == 0

train.dt$SinceLastPromo2 <- replace(train.dt[["SinceLastPromo2"]], which(is.na(train.dt[["SinceLastPromo2"]])), -1)
train.dt$Competitionsinceint <- replace(train.dt[["Competitionsinceint"]], which(is.na(train.dt[["Competitionsinceint"]])), -mean(train.dt$Competitionsinceint, na.rm = T))
train.dt$Promosinceint <- replace(train.dt[["Promosinceint"]], which(is.na(train.dt[["Promosinceint"]])), -mean(train.dt$Promosinceint, na.rm = T))

#check to see if above is done correctly
colSums(is.na(train.dt)) == 0

#conversion to factor
train.dt$SinceLastPromo2 <- as.factor(train.dt$SinceLastPromo2)
levels(train.dt$SinceLastPromo2)

#added this as stateholiday/date is a factor with only 1 level 
train.dt$StateHoliday = NULL
temp_table <- train.dt
train.dt[,Date:=NULL]
train.dt$Store=NULL

#splitting into test and train set
test.dt<-train.dt[1:1000]
train.dt<-train.dt[1001:4500]

#correlation plots 
rcorr(train.dt$Sales,train.dt$DayOfWeek)
rcorr(train.dt$Sales,train.dt$Promo)
rcorr(train.dt$Sales,train.dt$StateHoliday)
rcorr(train.dt$Sales,train.dt$SchoolHoliday)
rcorr(train.dt$Sales,train.dt$Assortment)
rcorr(train.dt$Sales,train.dt$CompetitionDistance)
rcorr(train.dt$Sales,train.dt$PromoInterval)
rcorr(train.dt$Sales,train.dt$Promosinceint)
rcorr(train.dt$Sales,train.dt$Competitionsinceint)
rcorr(train.dt$Sales,train.dt$isweekend)
rcorr(train.dt$Sales,train.dt$isweekendyest)
rcorr(train.dt$Sales,train.dt$isweekendtmr)
rcorr(train.dt$Sales,train.dt$month)
rcorr(train.dt$Sales,train.dt$year)
rcorr(train.dt$Sales,train.dt$isclosetmr)
rcorr(train.dt$Sales,train.dt$iscloseyest)
rcorr(train.dt$Sales,train.dt$isschholtmr)
rcorr(train.dt$Sales,train.dt$isschholyest)
rcorr(train.dt$Sales,train.dt$SinceLastPromo2)
rcorr(train.dt$Sales,train.dt$cat)
rcorr(train.dt$Sales,train.dt$DaysSincePromo)

corrmatrix <- cbind(-0.1,0.42,0.03,0.24,0.14,-0.41,-0.03,0.18,-0.06,0.19,0.05,-0.08,0.17,0.01,0.23,0.04,-0.04,0.03,-0.65,-0.34)
colnames(corrmatrix) <- c("DayOfWeek","Promo","SchoolHoliday","Assortment","CompetitionDistance","PromoInterval","Promosinceint",
                          "Competitionsinceint","isweekend","isweekendyest","isweekendtmr","month","year","isclosetmr","iscloseyest",
                          "isschholtmr","isschholyest","SinceLastPromo2","cat","DaysSincePromo")
rownames(corrmatrix) <- c("Sales")

#Plotting the correlation matrix
corrplot(corrmatrix, type="upper")

#splitting into validation and train set
set.seed(1234)
train <- sample.split(Y = train.dt$Sales, SplitRatio = 0.7)
trainset <- subset(train.dt, train == T)
testset <- subset(train.dt, train == F)

#using rpart to generate regression trees for the data 
library(rpart)
train_tree <- trainset
str(train_tree)
test_tree <- testset

#we center and scale the data 
test_tree_preProc <- test_tree
preProcValues <- preProcess(test_tree_preProc, method = c("center", "scale"))
test_tree <- predict(preProcValues, test_tree_preProc)

#center/scale for train 
test.dt_xgb <- test.dt #for xgb 
test.dt_preProc <- test.dt
preProcValues <- preProcess(test.dt_preProc, method = c("center", "scale"))
test.dt <- predict(preProcValues, test.dt_preProc)

#missing values so we attempt to use caret to predict
sum(is.na(train_tree))
train.preprocess <- preProcess(train_tree, method = c("center", "scale"))
train_tree <- predict(train.preprocess, newdata = train_tree)
sum(is.na(train_tree)) 

#model 1 - rpart, using all variables  
tree1 = rpart(Sales ~ ., data = train_tree)
predict_tree1 = predict(tree1, test.dt)
ggplot(aes(x = Sales, y = predict_tree1), data = test.dt) + geom_smooth()  
summary(tree1)

#judging our tree based on metrics
prp(tree1)
rmse(predict_tree1, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
rsq.rpart(tree1)
tmp <- printcp(tree1)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val

#we consider the variable importance in tree1 to generate our next tree
varImp(tree1)
tree1$variable.importance

#model 2 - rpart, we build using the important (top few) variables
tree2 <- rpart(Sales ~ cat + DayOfWeek + iscloseyest + PromoInterval 
               + DaysSincePromo, data = train_tree)
predict_tree2 = predict(tree2, test.dt)
ggplot(aes(x = Sales, y = predict_tree2), data = test.dt) + geom_smooth()  
summary(tree2)

#judging our tree based on metrics
prp(tree2) #seems to be the same as the above? might need relooking 
rmse(predict_tree2, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
rsq.rpart(tree2)
tmp <- printcp(tree2)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val
#RMSE(predict_tree2, testset)

#model 3 - rpart, growing the tree to the maximum
tree3 <- rpart(Sales ~., data = train_tree, control = rpart.control(minsplit = 2, cp = 0))
printcp(tree3)

#model 4 - pruning the tree 
cp.opt <- tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"]
tree4 <- prune(tree3, cp.opt)
prp(tree4) #tree plotted out is still extremely big - possibility of overfitting 
summary(tree4)
nodes <- as.numeric(rownames(tree4$frame))
max(rpart:::tree.depth(nodes)) #depth of 12 - risk of overfitting 
predict_tree4 = predict(tree4, test.dt)

#judging our tree based on metrics
ggplot(aes(x = Sales, y = predict_tree4), data = test.dt) + geom_smooth()  
printcp(tree4)
tree4
rmse(predict_tree4, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
rsq.rpart(tree4)
tmp <- printcp(tree4)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val

#model 5 - training our tree - we exploit the fact that CP is already tuned to adjust depth.
fitControl.rpart <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#generation of actual model 
tree5 <- train(Sales ~ ., method = "rpart2", data = train_tree, tuneLength = 20)

#judging our tree based on metrics
tree5 
tree5$finalModel
tree5$finalModel$cptable
nodes.model5 <- as.numeric(rownames(tree5$finalModel$frame))
max(rpart:::tree.depth(nodes.model5)) #finding depth of tree
prp(tree5$finalModel)
predict_tree5 <- predict(tree5, newdata = test.dt)

#rmse of validation and test set 
rmse(predict_tree5, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
tree5$results$RMSE[14]/(max(train_tree$Sales) - min(train_tree$Sales))

#checking variable importance
varImp(tree5)

#using randomforest
library(randomForest)
train_forest <- train_tree
test_forest <- test_tree

#to solve weird error 
train_forest$SinceLastPromo2.a <- train_forest$`SinceLastPromo2.-1`
test_forest$SinceLastPromo2.a <- test_forest$`SinceLastPromo2.-1`

train_forest$`SinceLastPromo2.-1` <- NULL
test_forest$`SinceLastPromo2.-1` <- NULL

#model 1 - simple randomForest
forest1 = randomForest(Sales~ ., data = train_forest)
predict_forest1 = predict(forest1, test.dt)
plot(forest1)
ggplot(aes(x = Sales, y = predict_forest1), data = test.dt) + geom_smooth() 
forest1

#VarImp
varImpPlot(forest1)

#metrics for judging the model
rmse(predict_forest1, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
forest1$rsq 

#model 2 - using a random search to select parameters for rf 
fitControl.rf <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
forest2 <- train(Sales ~ ., data = train_forest, method = "rf", tuneLength = 10, 
                 trControl = fitControl.rf, na.action = na.omit)
print(forest2) #takes forever to run
forest2

#metrics for judging the model
predict_forest2 = predict(forest2, test.dt)
rmse(predict_forest2, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
forest2$rsq 

#model 3 - using a tuning grid to select parameters for rf 
fitControl.rf2 <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tuneGrid.rf <- expand.grid(mtry = c(2,4,6,9,13,17,20)) 
forest3 <- train(Sales ~ ., data = train_forest, method = "rf", tuneGrid = tuneGrid.rf,
                 trainControl = fitControl.rf2, na.action = na.omit)
forest3

#metrics for judging the model
predict_forest3 = predict(forest3, test.dt)
rmse(predict_forest3, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
forest3$finalModel

#model 4 - using oob to train the model
fitControl.rf3 <- trainControl(method = "oob") #takes forever
forest4 <- train(Sales ~ ., method = "rf", data = train_forest, tuneLength = 20, 
                 trainControl = fitControl.rf3, na.action = na.omit)
forest4

#metrics for judging the model
predict_forest4 = predict(forest4, test.dt)
rmse(predict_forest4, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
forest4$rsq 

#model 5 - using tuneRF to adjust mtry parameter
temp_sales <- test_forest$Sales
test_forest$Sales <- NULL #done to prevent error 
forest5 <- tuneRF(Sales ~ ., test_forest, mtryStart = 2, ntreeTry = 500, stepFactor = 1.5, 
                  improve = 0.001, doBest = T, data = train_forest, plot = T)
match(max(forest5$rsq), forest5$rsq) #find where max occurs, can tune if needed 
summary(forest5)
forest5$forest #check if there is a randomForest object in the wrapper 

#we know that the best mtry is at 6 (according to the tuning); we have to build a forest as tuneRF 
#does not return a forest object (checked above from forest)
forest5 <- randomForest(Sales ~., data = train_forest, mtry = 6)

#metrics for judging the model
test_forest$Sales <- temp_sales
predict_forest5 = predict(forest5, test.dt)
rmse(predict_forest5, test.dt$Sales)/(max(test.dt$Sales) - min(test.dt$Sales)) 
(forest5$mse[500]) ^ 0.5/(max(train_forest$Sales) - min(train_forest$Sales)) 
forest5
varImpPlot(forest5)

#generation of xgb model 
set.seed(1234)
#Converting the dataset into xgb.DMatrix
trainm <-as.matrix(trainset[,-c("Sales")])
testm <-as.matrix(testset[,-c("Sales")])

trainm<-as(trainm,"sparseMatrix")
testm<-as(testm,"sparseMatrix")

train_label<-trainset$Sales
train_label <- as.numeric(train_label)
test_label<-testset$Sales
test_label <- as.numeric(test_label)
train_matrix <- xgb.DMatrix(data=trainm,label=train_label)
test_matrix <- xgb.DMatrix(data=testm,label=test_label)

#Parameter tuning for XgBoost
best_param = list()
best_seednumber = 1234
best_rmse = Inf
#Run iteration 100 times which sample the parameter combinations randomly
for (iter in 1:100) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=train_matrix, params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = F, early_stopping_rounds=8, maximize=FALSE)
  
  min_rmse = min(mdcv$evaluation_log[, test_rmse_mean])
  
  if (min_rmse < best_rmse) {
    best_seednumber = seed.number
    best_param = param
  }
}
#Use the best seed number obtained in the iteration
set.seed(best_seednumber)
#Transfering the best parameter obtained into params2
params2 <- list(booster="gbtree",objective="reg:linear", eta=0.2146533,gamma=0.0282414,
                max_depth=10,min_child_weight=26,subsample=0.8611443,colsample_bytree=0.7506555)
#Running nrounds of 1000 to find the optimal nrounds
xgbcv <- xgb.cv(params=params2,data=train_matrix,nrounds=1000,nfold=5,showsd=T,stratified = T,print_every_n = 10,early_stopping_rounds=40,maximize=F)
#Using the optimal nrounds of 79 to train the model
xgb1 <- xgb.train (params = params2, data = train_matrix, nrounds = 79, watchlist = list(val=test_matrix,train=train_matrix), print_every_n = 10, early_stopping_rounds = 10, maximize = F )

#Testing with validation set
xgbpred <- predict (xgb1,test_matrix)
k=data.frame(xgbpred,testset$Sales)
rmse(testset$Sales,xgbpred)/(max(testset$Sales)-min(testset$Sales)) #Normalised RMSE

#Finding the variable importance
mat <- xgb.importance (feature_names = colnames(trainm),model = xgb1)
par(mar=c(1,1,1,1))
xgb.plot.importance (importance_matrix = mat[1:10])

#Testing with the test set (test.dt)
testr <-as.matrix(test.dt[,-c("Sales")])
testr<-as(testr,"sparseMatrix")
testr_label<-test.dt_xgb$Sales #use denormalised test set 
testr_label <- as.numeric(testr_label)
testr_matrix <- xgb.DMatrix(data=testr,label=testr_label)
xgbpred2 <- predict (xgb1,testr_matrix)
rmse(test.dt_xgb$Sales,xgbpred2)/(max(test.dt_xgb$Sales)-min(test.dt$Sales)) #Normalised RMSE

#Linear Model
set.seed(1234)
#Creating base linear model
mlinear <- lm(Sales~DayOfWeek+Promo+CompetitionDistance+SchoolHoliday+Assortment
              +PromoInterval+Promosinceint+isweekendyest+isweekendtmr+isweekend+month+year+
                isclosetmr+iscloseyest+isschholtmr+isschholyest+SinceLastPromo2+cat+DaysSincePromo,data=trainset)
#Performing stepwise AIC for feature selection
mlinear <- step(mlinear)
#Check F-test, if variables are significant
summary(mlinear)
#Indicates there is high multicollinearity among variables
vif(mlinear)
#Take out high VIF variables, take out SinceLastPromo2 because it is insignificant.
test.dt_linear <- test.dt_xgb
trainset$cat <- as.numeric(trainset$cat)
testset$cat <- as.numeric(testset$cat)
test.dt_linear$cat <- as.numeric(test.dt_xgb$cat)

#Improved model based on the above considerations.
mlinear2 <- lm(Sales~Promo+CompetitionDistance+SchoolHoliday+Assortment+month+year+isclosetmr+iscloseyest+isschholyest+cat+DaysSincePromo, data = trainset)
#Checking for F-statistic, significant variables and multicollinearity.
summary(mlinear2)
vif(mlinear2)
#Retrieve most important variables for model
varImp(mlinear2, scale = FALSE)
#Diagnostic plots for the model
par(mfrow = c(2,2))
plot(mlinear2)  
par(mfrow = c(1,1))

#Predicting on train set, validation set and test set using mlinear2
predict.trainset <- predict(mlinear2, newdata = trainset)
RMSE.trainset <- rmse(predict.trainset, trainset$Sales) / (max(trainset$Sales)-min(trainset$Sales))
RMSE.trainset

predict.validationset <- predict(mlinear2, newdata = testset)
RMSE.validationset <- rmse(predict.validationset, testset$Sales) / (max(testset$Sales)-min(testset$Sales))
RMSE.validationset

predict.testset <- predict(mlinear2, newdata = test.dt_linear)
RMSE.testset <- rmse(predict.testset, test.dt_linear$Sales)
RMSE.testset <- RMSE.testset / (max(test.dt_linear$Sales)-min(test.dt_linear$Sales))
RMSE.testset

#Model based on variables most highly correlated with Sales (>0.3)
mlinear_corr <- lm(Sales~Promo+PromoInterval+cat+DaysSincePromo, data=trainset)
#Checking F-statistics, significant variables and multicollinearity
summary(mlinear_corr)
vif(mlinear_corr)

#Predicting on train set, validation set and test set using mlinear_corr
predict.trainset <- predict(mlinear_corr, newdata = trainset)
RMSE.trainset <- rmse(predict.trainset, trainset$Sales) / (max(trainset$Sales)-min(trainset$Sales))
RMSE.trainset

predict.validationset <- predict(mlinear_corr, newdata = testset)
RMSE.validationset <- rmse(predict.validationset, testset$Sales) / (max(testset$Sales)-min(testset$Sales))
RMSE.validationset

predict.testset <- predict(mlinear_corr, newdata = test.dt_linear)
RMSE.testset <- rmse(predict.testset, test.dt_linear$Sales)
RMSE.testset <- RMSE.testset / (max(test.dt_linear$Sales)-min(test.dt_linear$Sales))
RMSE.testset

#aggregation starts here
set.seed(1234)

#generation of test set for ensemble
test_ensemble <- temp_table[Store == "887"]
test_ensemble <- test_ensemble[382:431]

#storing and cleaning
test_ensemble_date <- test_ensemble$Date
test_ensemble$Date = NULL
test_ensemble$Store = NULL 

#generation of predicted values for xgb
test_xgbEns<-as.matrix(test_ensemble[,-c("Sales")])
test_xgbEns<-as(test_xgbEns,"sparseMatrix")
test_xgbEns_label<-test_ensemble$Sales #use denormalised test set 
test_xgbEns_label <- as.numeric(test_xgbEns_label)
test_xgbEns_matrix <- xgb.DMatrix(data=test_xgbEns,label=test_xgbEns_label)
xgbpred_Ens <- predict(xgb1,test_xgbEns_matrix)
xgbpred_Ens <- as.data.table(xgbpred_Ens)
colnames(xgbpred_Ens) <- "xgb"

#generation of predicted values for linear 
test_ensemble_linear <- test_ensemble
test_ensemble_linear$cat <- as.numeric(test_ensemble_linear$cat)
predict_ensemble_linear <- predict(mlinear2, newdata = test_ensemble_linear)
predict_ensemble_linear <- as.data.table(predict_ensemble_linear)
colnames(predict_ensemble_linear) <- "linear"

#join 
final = predict_ensemble_linear 
final[,xgb:= xgbpred_Ens]

#aggregation 
agg = (predict_ensemble_linear + xgbpred_Ens) / 2
final <- final[, aggregate := agg]
final <- final[, actual := test_ensemble_linear$Sales]

#we join the dates together and check final against test_ensemble to see if it is performed correctly
final <- final[, Date := test_ensemble_date]
test_ensemble <- test_ensemble[, Date := test_ensemble_date]

#rmse 
rmse(final$xgb, final$actual)
rmse(final$linear, final$actual)
rmse(final$aggregate, final$actual)

#plot graph 
ggplot(aes(x = Date), data = final) + geom_line(aes(y=final$actual, color="Actual")) + geom_line(aes(y=final$aggregate, color="Predicted")) + ggtitle("Ensemble Performance on Store Sales against Date") + ylab("Sales")
ggplot(aes(x = Date), data = final) + geom_line(aes(y=final$actual, color="Actual")) + geom_line(aes(y=final$xgb, color="Predicted")) + ggtitle("XgBoost Performance on Store Sales against Date") + ylab("Sales")
