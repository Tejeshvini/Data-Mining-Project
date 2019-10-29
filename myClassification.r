library(readr)
library(caret)
library(party)
library(class)
require(partykit)
ilpd_df <- readRDS(file="ilpd_preprocessed.Rda")
ilpd_df<-data.frame(ilpd_df)

#Assign numeric values to Male and Female in Gender column
ilpd_df$Gender[ilpd_df$Gender=="Male"]<- 1
ilpd_df$Gender[ilpd_df$Gender=="Female"]<- 0
ilpd_df$Gender<-as.factor(ilpd_df$Gender)
ilpd_df$Class<-as.factor(ilpd_df$Class)
set.seed(45)

##Divide the data into test and training datasets
indices <- sample(2, nrow(ilpd_df),replace = TRUE, prob = c(0.7, 0.3))
train <- ilpd_df[indices==1,]
test <- ilpd_df[indices==2,]
sapply(ilpd_df,class)

##Building and plotting a classification tree
myformula <- Class~ Age+Gender+TB+DB+Alkphos+Sgpt+Sgot+TP+Albumin+AG_Ratio
myformula
tree<-ctree(myformula, data = train)
png(filename = "Tree_default.png", width = 1200, height = 900)
plot(tree, main= "Tree with default parameters")
dev.off()

#predicting the class labels from the above tree
tree_predict<-predict(tree,test)
table<-table(tree_predict,test$Class)
table
tn<-table[1,1]
tp<-table[2,2]
fp<-table[2,1]
fn<-table[1,2]
accuracy<-(tp+tn)/(tp+tn+fp+fn)
precision<-tp/(tp+fp)
recall<-tp/(tp+fn)
fmeasure <- (2*precision*recall)/(precision+recall)
accuracy
precision
recall
fmeasure

##tree with control parameters
ctree_cc<-ctree(myformula, data= train,control = ctree_control(teststat=c("max"),testtype=c("Teststatistic"),mincriterion = 0.6, minsplit = 50, minbucket = 50))
png(filename = "tree_control.png", width = 1000, height = 800)
plot(ctree_cc, main= "Tree with control parameters")
dev.off()

##k-nn classification
prediction_knn  <- knn(train[,-c(11)], test[,-c(11)], train$Class, k=1, prob=TRUE)
knn_table<-table(prediction_knn,test$Class)
knn_table
tn_knn<-knn_table[1,1]
tp_knn<-knn_table[2,2]
fp_knn<-knn_table[2,1]
fn_knn<-knn_table[1,2]
accuracy_knn<-(tp_knn+tn_knn)/(tp_knn+tn_knn+fp_knn+fn_knn)
precision_knn<-tp_knn/(tp_knn+fp_knn)
recall_knn<-tp_knn/(tp_knn+fn_knn)
fmeasure_knn <- (2*precision_knn*recall_knn)/(precision_knn+recall_knn)
accuracy_knn
precision_knn
recall_knn
fmeasure_knn

#knn for k =2
prediction_knn2  <- knn(train[,-c(11)], test[,-c(11)], train$Class, k=2, prob=TRUE)
knn2_table<-table(prediction_knn2,test$Class)
knn2_table
tn_knn2<-knn2_table[1,1]
tp_knn2<-knn2_table[2,2]
fp_knn2<-knn2_table[2,1]
fn_knn2<-knn2_table[1,2]
accuracy_knn2<-(tp_knn2+tn_knn2)/(tp_knn2+tn_knn2+fp_knn2+fn_knn2)
precision_knn2<-tp_knn2/(tp_knn2+fp_knn2)
recall_knn2<-tp_knn2/(tp_knn2+fn_knn2)
fmeasure_knn2 <- (2*precision_knn2*recall_knn2)/(precision_knn2+recall_knn)
accuracy_knn2
precision_knn2
recall_knn2
fmeasure_knn2

#knn for k =3
prediction_knn3  <- knn(train[,-c(11)], test[,-c(11)], train$Class, k=3, prob=TRUE)
knn3_table<-table(prediction_knn3,test$Class)
knn3_table
tn_knn3<-knn3_table[1,1]
tp_knn3<-knn3_table[2,2]
fp_knn3<-knn3_table[2,1]
fn_knn3<-knn3_table[1,2]
accuracy_knn3<-(tp_knn3+tn_knn3)/(tp_knn3+tn_knn3+fp_knn3+fn_knn3)
precision_knn3<-tp_knn3/(tp_knn3+fp_knn3)
recall_knn3<-tp_knn3/(tp_knn3+fn_knn3)
fmeasure_knn3 <- (2*precision_knn3*recall_knn3)/(precision_knn3+recall_knn)
accuracy_knn3
precision_knn3
recall_knn3
fmeasure_knn3

#knn for k =4
prediction_knn4  <- knn(train[,-c(11)], test[,-c(11)], train$Class, k=4, prob=TRUE)
knn4_table<-table(prediction_knn4,test$Class)
knn4_table
tn_knn4<-knn4_table[1,1]
tp_knn4<-knn4_table[2,2]
fp_knn4<-knn4_table[2,1]
fn_knn4<-knn4_table[1,2]
accuracy_knn4<-(tp_knn4+tn_knn4)/(tp_knn4+tn_knn4+fp_knn4+fn_knn4)
precision_knn4<-tp_knn4/(tp_knn4+fp_knn4)
recall_knn4<-tp_knn4/(tp_knn4+fn_knn4)
fmeasure_knn4 <- (2*precision_knn4*recall_knn4)/(precision_knn4+recall_knn)
accuracy_knn4
precision_knn4
recall_knn4
fmeasure_knn4

#knn for k =5
prediction_knn5  <- knn(train[,-c(11)], test[,-c(11)], train$Class, k=5, prob=TRUE)
knn5_table<-table(prediction_knn5,test$Class)
knn5_table
tn_knn5<-knn5_table[1,1]
tp_knn5<-knn5_table[2,2]
fp_knn5<-knn5_table[2,1]
fn_knn5<-knn5_table[1,2]
accuracy_knn5<-(tp_knn5+tn_knn5)/(tp_knn5+tn_knn5+fp_knn5+fn_knn5)
precision_knn5<-tp_knn5/(tp_knn5+fp_knn5)
recall_knn5<-tp_knn5/(tp_knn5+fn_knn5)
fmeasure_knn5 <- (2*precision_knn5*recall_knn5)/(precision_knn5+recall_knn)
accuracy_knn5
precision_knn5
recall_knn5
fmeasure_knn5