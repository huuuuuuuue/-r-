#-------------------------
setwd("~/Desktop/大三下/data mining")
d= read.csv("train.csv",header=T) #原数据读区
#删除不需要的值,pca不需要y值，还有非数值变量
dc=d[,2:9] #没做标准化的数据

#--------------不用包来实现pca-----------------
#scale标准化
sdc=scale(dc) #做过标准化的数据
mean(sdc[,1])
cov_sdc = cov(sdc) #协方差矩阵
eigen(cov_sdc) #算出的是八个特征向量各自的维度##求特征值，8个特征值，累加结果大于等于85%
#直接做主成分分析的
princomp(dc)
#不考虑其它方法，用手动发，和加载stats的包，这三个结果是否一样

#-----------------用stats这个包实现pca--------------------
prcomp(dc)
prcomp(sdc)
#由于eigen(cov_sdc)和prcomp(sdc)结果是一样的，所以该函数默认是没有做过矩阵的标准化的，需要指定标准化


d = read.csv("train.csv",header = TRUE)
dc = d[,2:9]
sdc = scale(dc)#标准化
mean(sdc[,1])
cov_sdc = cov(sdc)
eigen(cov_sdc)
princomp(dc)
library("stats",lib.loc = "C:/Program Files/Microsoft/R Open/R-3.4.0/library")
prcomp(dc)
prcomp(sdc)#说明该算法不支持标准化 需提前标准化
#factomineR 算法
