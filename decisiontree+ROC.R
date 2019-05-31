#-----------load-data-------------
#set the working path
setwd("~/Desktop/大三下/data mining")
#load the data 
d= read.csv("train.csv",header=TRUE)
#clean the data, delete N/A (even if there is no none here)
dc= d[complete.cases(d),]
#divide data into several groups:grouping first-->avoid uneven sampling
hist(d$y)#分布#优先分组y防止抽样不均
#这步的目的是为了可以将y的各种数值（就是统计中的按权重分配）
d0 = d[d$y==0,]
d1 = d[d$y==1,]
d2 = d[d$y==2,]
d3 = d[d$y==3,]

#----------testset&trainset-----------
#create labels
label0 = sample(c(1:10),dim(d0[1]),replace=TRUE)
label1 = sample(c(1:10),dim(d1[1]),replace=TRUE)
label2 = sample(c(1:10),dim(d2[1]),replace=TRUE)
label3 = sample(c(1:10),dim(d3[1]),replace=TRUE)

#select test set and train set according to label = 5
d0_train = d0[label0<=5,]
d0_test = d0[label0>5,]
d1_train = d1[label1<=5,]
d1_test = d1[label1>5,]
d2_train = d2[label2<=5,]
d2_test = d2[label2>5,]
d3_train = d3[label3<=5,]
d3_test = d3[label3>5,]
#将产生的测试集和训练集分别粘在一起
d_train = rbind(d0_train,d1_train,d2_train,d3_train)
d_test = rbind(d0_test,d1_test,d2_test,d3_test)

#-------------modeling--------------
#binomial代表二分类 glm不适应与此分析，-id表示除去id这一列
#------以下为testing-----
 ##re_glm = glm(y~.-id,data = d_train,family = binomial)
#--------以下为多分类---------
#多分类的原因：不止两个分支
library(nnet)
re_log = multinom(y~.-id,data=d_train) #建模
pred_log = predict(re_log,newdata=d_test) #预测
summary(pred_log)
tab_log= table(d_test$y,pred_log)
tab_log #行代表真正的数据（2092+11406+……）

#---------decision tree--------------
library(rpart)#ID3 decision tree
re_id3_mistake = rpart(y~.-id,data = d_train)#将数据默认为连续变量
re_id3 = rpart(y~.-id,data = d_train,method = "class",parms = list(split="information"))#将数据当成分类变量
plot(re_id3)
text(re_id3)
re_id3#末端是整数是需要的

#----------gini-----------
re_CART = rpart(y~.-id,data = d_train,method = "class",parms = list(split="gini"))
pred_id3 = predict(re_id3,newdata = d_test,type = "class")
table(d_test$y,pred_id3)
pred_CART = predict(re_CART,newdata = d_test,type = "class")
table(d_test$y,pred_CART)


re_id3$cptable#说明cp（α）越大越难以分解，选择交叉验证误差最低的cp值，默认最小值0.01
re_CART = rpart(y~.-id,data = d_train,method = "class",parms = list(split="gini"),control = rpart.control(cp=0.0001))
re_CART$cptable
table(d_test$y,pred_CART)
min = which.min(re_CART$cptable[,4])
re_CART_f = prune(re_CART,cp=re_CART$cptable[min,1])
pred_CART = predict(re_CART_f,newdata = d_test,type = "class")
table(d_test$y,pred_CART)
plot(re_CART_f)


install.packages("randomForest")
library(randomForest)
d_train$y = as.factor(d_train$y)
re_rf = randomForest(y~.-id,data = d_train,ntree = 5)
pred_rf = predict(re_rf,newdata = d_test,type="prob")

#------plot ROC curve-----------
install.packages("prediction")
library("prediction")
library("gplots")
install.packages("ROCR")
library("ROCR")
pred <- prediction(pred_rf[,2],d_test$y)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


install.packages("gplots")

#--------------------------------
d_train$y[d_train$y>=1]=1
d_test$y[d_test$y>=1]=1
library(RWeka)#C4.5需安装java



