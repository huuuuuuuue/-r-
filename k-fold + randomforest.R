
library(plyr)
CVgroup <- function(k,datasize,seed){
    cvlist <- list()
    set.seed(seed)
    n <- rep(1:k,ceiling(datasize/k))[1:datasize]    #将数据分成K份，并生成的完成数据集n
    temp <- sample(n,datasize)   #把n打乱
    x <- 1:k
    dataseq <- 1:datasize
    cvlist <- lapply(x,function(x) dataseq[temp==x])  #dataseq中随机生成k个随机有序数据列
    return(cvlist)
}

data <- iris
pred <- data.frame()   #存储预测结果

k <- 10
datasize <- nrow(iris)
cvlist <- CVgroup(k = k,datasize = datasize,seed = 1206)
cvlist

library(randomForest)
n = seq(1:20)
m <- seq(60,500,by = 20)  #如果数据量大尽量间隔大点，间隔过小没有实际意义
for(a in n){
for(j in m){   #j指的是随机森林的数量

    for (i in 1:k){
        train <- data[-cvlist[[i]],]  #刚才通过cvgroup生成的函数
        test <- data[cvlist[[i]],]
        model <-randomForest(Sepal.Length~.,data = train,ntree = j, mtry = a)   #建模，ntree=j 指的树数
        prediction <- predict(model,subset(test,select = -Sepal.Length))   #预测
        randomtree <- rep(j,length(prediction))   #随机森林树的数量
        kcross <- rep(i,length(prediction))   #i是第几次循环交叉，共K次
        temp <- data.frame(cbind(subset(test,select = Sepal.Length),prediction,randomtree,kcross))#真实值、预测值、随机森林树数、预测组编号捆绑在一起组成新的数据框tenp
        pred <- rbind(pred,temp)   #temp按行和pred合并
    }
}
}