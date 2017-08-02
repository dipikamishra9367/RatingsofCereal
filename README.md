# RatingsofCereal
Predicting Rating of Cereals 
library ("rpart")
library("rpart.plot")
library("ggplot2")
library("GGally")
library("tree")

cereals <- read.csv(file.choose(), header = TRUE)

columns <-  c()


#The data contains columns that are of numerical 
#and character types. We will just concentrate on 
#extracting the numerical variables. We store the resulting data in temp.


for(i in 1:dim(cereals)[2])
{
  if(is.numeric(cereals[,i])|| is.integer(cereals[,i]))
  {
    columns[i]=T
  }
  else
    
    
  {
    columns[i]=F
  }
}
t1 <- na.omit(cereals[,columns])

scale(t1)
ggplot(t1, aes(x=rating)) + geom_histogram()

correlation <- c()
for(i in 1:dim(temp)[2])
{
  correlation[i] <- cor(t1[,i],temp[,'rating'])
}

correlation


ggplot(temp, aes(x=protein, y=rating)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[7]))
ggplot(temp, aes(x=fiber, y=rating)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")+ggtitle(paste('R:',correlation[2]))

t <- t1[-train,]
lmfit = lm(rating~protein+fiber,data=t1_train)
summary(lmfit)
pred <- predict(lmfit,t1_test)

mean((t1_test$rating-pred)^2)

?rpart

set.seed(3)
m.rpart <- rpart(rating~.,data=t1_train)
m.rpart

rpart.plot(m.rpart,digits = 3)

p.rpart <- predict(m.rpart,t1_test)
rpart.plot(m.rpart,digits = 3)
par(mfrow=c(1,1))
tree_dataframe <- data.frame(p.rpart,t1_test$rating)



ggplot(tree_dataframe, aes(x=p.rpart)) + geom_histogram(fill="white", colour="black")
ggplot(tree_dataframe, aes(x=t1_test.rating)) + geom_histogram(fill="white", colour="black")

cor(p.rpart,t1_test$rating)
mean((p.rpart-temp_test$rating)^2)
set.seed(5)
library(randomForest)
rf <- randomForest(rating~.,data=t1[train,],ntree=500,mtry=floor(dim(t1)[2]/3))
pred_rf <- predict(rf,t1[-train,])
mean((pred_rf-t1[-train,]$rating)^2)


array_ntree<- c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500)
mse <- c()
j<-1
for(i in array_ntree)
{ set.seed(5)
  rf <- randomForest(rating~.,data=t1[train,],ntree=i,mtry=floor(dim(temp)[2]/3))
  pred_rf <- predict(rf,t1[-train,])
  mse[j]<-mean((pred_rf-t1[-train,]$rating)^2)
  j=j+1
  
}

data_mse <- data.frame(array_ntree,mse)

ggplot(data_mse, aes(x=(array_ntree), y=mse)) + geom_line() + geom_point()

set.seed(5)

rf <- randomForest(rating~.,data=temp[train,],ntree=data_mse$array_ntree[data_mse$mse==min(data_mse$mse)],mtry=floor(dim(temp)[2]/3))


pred_rf <- predict(rf,t1[-train,])

mean((pred_rf-temp[-train,]$imdb_score)^2)


