train <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/train.csv/train.csv")
test <- read.csv("C:/Users/User/OneDrive/Desktop/New folder/test.csv/test.csv")
library(data.table)
library(mltools)
library(standardize)
library("dplyr")

train$target=as.factor(train$target)
train_s<-train[,-1]%>%mutate_if(is.numeric,scale)

install.packages("flexclust")
library("flexclust")
library("randomForest")

train_11 = train_s

set.seed(1)
kbest = kcca(train_11[,-94], k=15, kccaFamily("kmeans"))
train_11$cluster<-as.character(kbest@cluster)

#cluster 1
clu1<-subset(train_11,train_11$cluster == "1")#no winner, train rf
table(clu1$target) # 3    1758    1519      10      17      67      63      86       1
clu1$target=as.factor(clu1$target)
RF_400_CL1 <- randomForest(target ~., data=clu1,ntree = 400, ntry=9
                           ,importance=F)


#cluster 2
clu2<-droplevels(subset(train_11,train_11$cluster == "2"))#class 9
table(clu2$target)# 64       6       8       8       0     148       5       2     985 

clu2$target=as.factor(clu2$target)
RF_400_CL2 <- randomForest(target ~., data=clu2,ntree = 400, ntry=9
                           ,importance=F)

#cluster 3
clu3<-droplevels(subset(train_11,train_11$cluster == "3"))#class 6
table(clu3$target)#43       1       1      28       0    4404      67     104      75 
clu3$target=as.factor(clu3$target)
RF_400_CL3 <- randomForest(target ~., data=clu3,ntree = 400, ntry=9
                           ,importance=F)
#cl4
clu4<-droplevels(subset(train_11,train_11$cluster == "4"))#class 8
table(clu4$target)#1       1       0       0       0      44       4     271       0 

clu4$target=as.factor(clu4$target)
RF_400_CL4 <- randomForest(target ~., data=clu4,ntree = 400, ntry=9
                           ,importance=F)

#cluster 5
clu5<-droplevels(subset(train_11,train_11$cluster == "5"))#class 7
table(clu5$target) #0       0       0       0       0      24     188       0       0  
clu5$target=as.factor(clu5$target)
RF_400_CL5 <- randomForest(target ~., data=clu5,ntree = 400, ntry=9
                           ,importance=F)

#clu6
clu6<-subset(train_11,train_11$cluster == "6")#no winner, rf
table(clu6$target) # 642      56      18       3       4     469     379    1588    2006  
clu6$target=as.factor(clu6$target)
RF_400_CL6 <- randomForest(target ~., data=clu6,ntree = 400, ntry=9
                           ,importance=F)


#cluster 7: 
clu7<-droplevels(subset(train_11,train_11$cluster == "7"))# no winner, run rf
table(clu7$target) #   2      60      15       3       0       1       5       8      85 
clu7$target=as.factor(clu7$target)
RF_400_CL7 <- randomForest(target ~., data=clu7,ntree = 400, ntry=9
                           ,importance=F)


#cluster 8
clu8<-droplevels(subset(train_11,train_11$cluster == "8"))#class 6
table(clu8$target) #0       0       0       1       2    5018       2      64      15  
clu8$target=as.factor(clu8$target)
RF_400_CL8 <- randomForest(target ~., data=clu8,ntree = 400, ntry=9
                           ,importance=F)

#cluster 9
clu9<-droplevels(subset(train_11,train_11$cluster == "9"))#class 8
table(clu9$target)#  24       1       1       0       0      47      83    1508       2  

clu9$target=as.factor(clu9$target)
RF_400_CL9 <- randomForest(target ~., data=clu9,ntree = 400, ntry=9
                           ,importance=F)


#cluster 10
clu10<-droplevels(subset(train_11,train_11$cluster == "10"))#no winner rf
table(clu10$target)# 0    1419     497     306       0       0       2       1       1 

clu10$target=as.factor(clu10$target)
RF_400_CL10 <- randomForest(target ~., data=clu10,ntree = 400, ntry=9
                            ,importance=F)


#cluster 11
clu11<-droplevels(subset(train_11,train_11$cluster == "11"))#no winner, RF
table(clu11$target)# 75       0       0       0       0       2       2     268       4 

clu11$target=as.factor(clu11$target)
RF_400_CL11 <- randomForest(target ~., data=clu11,ntree = 400, ntry=9
                            ,importance=F)


#clu12
clu12<-subset(train_11,train_11$cluster == "12")#no winner
table(clu12$target)#1386    6130    2732    1135    2702    3895    1551    4875    2273

clu12$target=as.factor(clu12$target)

RF_400_CL12 <- randomForest(target ~., data=clu12,ntree = 400, ntry=9
                            ,importance=F)


#clu13
clu13<-subset(train_11,train_11$cluster == "13")#no winner, rf
table(clu13$target)#21    6390    2589    1007      18      28     180       5      67 

clu13$target=as.factor(clu13$target)

RF_400_CL13 <- randomForest(target ~., data=clu13,ntree = 400, ntry=9
                            ,importance=F)

#clu14
clu14<-droplevels(subset(train_11,train_11$cluster == "14"))#no winner, rf
table(clu14$target)# 10     296     593     159       0      91     620      75       8
clu14$target=as.factor(clu14$target)
RF_400_CL14 <- randomForest(target ~., data=clu14,ntree = 400, ntry=9
                            ,importance=F)


#clu15
#clu15<-droplevels(subset(train_11,train_11$cluster == "15"))#class 6
#table(clu15$target)# 1       0       0       0       0     105       0       0       1 

#clu15$target=as.factor(clu15$target)
#RF_400_CL15 <- randomForest(target ~., data=clu15,ntree = 400, ntry=9
                         #   ,importance=F)



test_s<-test[,-1]%>%mutate_if(is.numeric,scale)
predict_test=predict(kbest,test_s)
test_s$cluster=predict_test
test_s=cbind(test[,1],test_s)

new = test_s
new$prediction = NA
new$prediction=ifelse(new$cluster=="15","Class_6",NA)

new_final=data.frame(new[,"prediction"])
a=na.omit(new_final)
a<- a %>% rownames_to_column("id")

a$new....prediction..=as.factor(a$new....prediction..)
a<-one_hot(as.data.table(a), cols = c("new....prediction.."))

Class_1=NA
Class_2=NA
Class_3=NA
Class_4=NA
Class_5=NA
Class_7=NA
Class_8=NA
Class_9=NA

new_final=cbind(Class_1,Class_2,Class_3,Class_4,Class_5,a,Class_7,Class_8,Class_9)
names(new_final)=c("Class_1","Class_2","Class_3","Class_4","Class_5","id","Class_6","Class_7", "Class_8","Class_9")
new_final=new_final[,c("id","Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]


pred_cl1=data.frame(predict(RF_400_CL1,newdata = new[which(new$cluster == "1"),],type="prob"))

pred_cl2=data.frame(predict(RF_400_CL2,newdata = new[which(new$cluster == "2"),],type="prob"))
pred_cl2$Class_5=integer(nrow(pred_cl2))
pred_cl2=pred_cl2[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl3=data.frame(predict(RF_400_CL3,newdata = new[which(new$cluster == "3"),],type="prob"))
pred_cl3$Class_2=integer(nrow(pred_cl3))
pred_cl3$Class_3=integer(nrow(pred_cl3))
pred_cl3$Class_5=integer(nrow(pred_cl3))
pred_cl3=pred_cl3[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl4=data.frame(predict(RF_400_CL4,newdata = new[which(new$cluster == "4"),],type="prob"))
pred_cl4$Class_3=integer(nrow(pred_cl4))
pred_cl4$Class_4=integer(nrow(pred_cl4))
pred_cl4$Class_5=integer(nrow(pred_cl4))
pred_cl4=pred_cl4[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl5=data.frame(predict(RF_400_CL5,newdata = new[which(new$cluster == "5"),],type="prob"))
pred_cl5$Class_1=integer(nrow(pred_cl5))
pred_cl5$Class_2=integer(nrow(pred_cl5))
pred_cl5$Class_3=integer(nrow(pred_cl5))
pred_cl5$Class_4=integer(nrow(pred_cl5))
pred_cl5$Class_5=integer(nrow(pred_cl5))
pred_cl5$Class_8=integer(nrow(pred_cl5))
pred_cl5$Class_9=integer(nrow(pred_cl5))
pred_cl5=pred_cl5[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl6=data.frame(predict(RF_400_CL6,newdata = new[which(new$cluster == "6"),],type="prob"))
pred_cl7=data.frame(predict(RF_400_CL7,newdata = new[which(new$cluster == "7"),],type="prob"))
pred_cl7$Class_5=integer(nrow(pred_cl7))
pred_cl7=pred_cl7[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl8=data.frame(predict(RF_400_CL8,newdata = new[which(new$cluster == "8"),],type="prob"))
pred_cl8$Class_1=integer(nrow(pred_cl8))
pred_cl8$Class_2=integer(nrow(pred_cl8))
pred_cl8$Class_3=integer(nrow(pred_cl8))
pred_cl8=pred_cl8[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl9=data.frame(predict(RF_400_CL9,newdata = new[which(new$cluster == "9"),],type="prob"))
pred_cl9$Class_4=integer(nrow(pred_cl9))
pred_cl9$Class_5=integer(nrow(pred_cl9))
pred_cl9=pred_cl9[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl10=data.frame(predict(RF_400_CL10,newdata = new[which(new$cluster == "10"),],type="prob"))
pred_cl10$Class_1=integer(nrow(pred_cl10))
pred_cl10$Class_5=integer(nrow(pred_cl10))
pred_cl10$Class_6=integer(nrow(pred_cl10))
pred_cl10$Class_7=integer(nrow(pred_cl10))
pred_cl10=pred_cl10[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]


pred_cl11=data.frame(predict(RF_400_CL11,newdata = new[which(new$cluster == "11"),],type="prob"))
pred_cl11$Class_2=integer(nrow(pred_cl11))
pred_cl11$Class_3=integer(nrow(pred_cl11))
pred_cl11$Class_4=integer(nrow(pred_cl11))
pred_cl11$Class_5=integer(nrow(pred_cl11))
pred_cl11=pred_cl11[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

pred_cl12=data.frame(predict(RF_400_CL12,newdata = new[which(new$cluster == "12"),],type="prob"))

pred_cl13=data.frame(predict(RF_400_CL13,newdata = new[which(new$cluster == "13"),],type="prob"))
pred_cl14=data.frame(predict(RF_400_CL14,newdata = new[which(new$cluster == "14"),],type="prob"))
pred_cl14$Class_5=integer(nrow(pred_cl14))
pred_cl14=pred_cl14[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]

#pred_cl15=data.frame(predict(RF_400_CL15,newdata = new[which(new$cluster == "15"),],type="prob"))
#pred_cl15$Class_2=integer(nrow(pred_cl15))
#pred_cl15$Class_3=integer(nrow(pred_cl15))
#pred_cl15$Class_4=integer(nrow(pred_cl15))
#pred_cl15$Class_5=integer(nrow(pred_cl15))
#pred_cl15$Class_7=integer(nrow(pred_cl15))
#pred_cl15$Class_8=integer(nrow(pred_cl15))
#pred_cl15=pred_cl15[,c("Class_1","Class_2","Class_3","Class_4","Class_5","Class_6","Class_7","Class_8","Class_9")]



predictions_Rf=rbind(pred_cl1,pred_cl2,pred_cl3,pred_cl4,pred_cl5,pred_cl6,pred_cl7,pred_cl8,pred_cl9,pred_cl10,pred_cl11,pred_cl12,pred_cl13,pred_cl14)
pred_rf<- predictions_Rf %>% rownames_to_column("id")

FINAL_predictions=rbind(pred_rf,new_final)
FINAL_predictions[is.na(FINAL_predictions)] <- 0

write.csv(data.frame(FINAL_predictions),"fin.csv",row.names=FALSE)



