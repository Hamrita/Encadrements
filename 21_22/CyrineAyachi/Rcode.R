library(readxl)
base=read_excel("C:/Users/DELL/Desktop/base de données.xlsx",col_types = c("text", "date", "date","numeric", "numeric", "text", "date", "numeric", "numeric", "date", "text","date", "numeric", "numeric", "numeric","text"))    
data=base
str(data)
data$SIN.2021<-as.factor(data$SIN.2021)
table(data$SIN.2021)
anyNA(data)
#########################################
########################################
#          Sinistres
#######################################
dats=data[,-c(11,12)]


#knn 
library(caTools)
sample <- sample(c(TRUE, FALSE), nrow(dats), replace=TRUE, prob=c(0.7,0.3))
train  <- dats[sample, ]
test   <- dats[!sample, ]
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
iris_norm <- as.data.frame(apply(dats[,-c(1,5)],2, 'nor'))
summary(iris_norm)
sample <- sample(c(TRUE, FALSE), nrow(iris_norm), replace=TRUE, prob=c(0.7,0.3))
train_norm  <- iris_norm[sample, ]
test_norm   <- iris_norm[!sample, ]
iris_target_category <- data[1:263,14]
iris_test_category <- data[264:397,14]
library(class, lib.loc = "C:/Program Files/R/R-4.2.1/library")
pr1 <- knn(train_norm,test_norm,cl=iris_target_category,k=20)
table(iris_test_category$test,pr)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#svm
library(ggplot2)
qplot(AGE.CONDUCTEUR,AGE.OBJET,data = data,color= SIN.2021)
library(e1071)
mymodel<- svm(SIN.2021~.,data = data)
summary(mymodel)
plot(mymodel,data=data,AGE.CONDUCTEUR~AGE.OBJET,slice=list(AGE.CONDUCTEUR=30,AGE.OBJET=3))
#confusion matrix and misclassification error
pred <- predict(mymodel,data)
tab<- table(predectied= pred , actual= data$SIN.2021)
1-sum(diag(tab))/sum(tab)
mymodel<- svm(SIN.2021~.,data = data,kernel= "linear")
mymodel<- svm(SIN.2021~.,data = data, kernel= "polynomial")
mymodel<- svm(SIN.2021~.,data = data, kernel= "sigmoid")
#tuning
set.seed(123)
tmodel<-tune(svm,SIN.2021~.,data = data,ranges = list(epsilon= seq(0,1,0.1),cost=2^(2:9)))
plot(tmodel)
summary(tmodel)
#best model
mymodel<-tmodel$best.model
summary((mymodel))








#random forest
library(randomForest)
set.seed(222)
rf<- randomForest(SIN.2021~.,data=train)
print(rf)
attributes(rf)

#prediction with train data 
library(caret)
p1<-predict(rf,train)
head(p1)
head(train$SIN.2021)
confusionMatrix(p1,train$SIN.2021)
#prediction with test data
p2<-predict(rf,test)
head(p2)
head(test$SIN.2021)
confusionMatrix(p2,test$SIN.2021)
#error rate 
plot(rf)
#tune mtry
tune.randomForest(train[-14],train[14],
                  stepFactor=1,
                  plot=TRUE,
                  ntreeTry=300,
                  trace=TRUE,
                  improve=0.05)
rf<- randomForest(SIN.2021~.,data=train,
                  ntree=300,
                  mtry=8,
                  importance=TRUE,
                  proximity=TRUE)
p1<-predict(rf,train)
confusionMatrix(p1,train$SIN.2021)

p2<-predict(rf,test)
confusionMatrix(p2,test$SIN.2021)
#variable importance
varImpPlot(rf,
           sort= T,
           n.var=10,
           main= "top 10 - Variable Impotance")
importance(rf)
varUsed(rf)
#partial dependance plot
particalplot(rf,train,,1)