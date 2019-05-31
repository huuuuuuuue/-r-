d = read.csv("hmeq.csv",na.strings="") ##na.strings 用来标记空值
 ##bad=1，没还钱；bad=0，还钱，xy都有空值，所以
dc = d[complete.cases(d),] ##只会返回x和y都完整的纪录，sparse稀疏，指原数据不满，用于图像识别（不重要）
dc1 = dc[dc$BAD==1,] 
dim(d1) 
dc0 = dc[dc$BAD==0,]
dim(d0)              #先把bad切成两组
?mahalanobis
#用假设检验，建立正态分布，左右三个西格玛的值，落在三个σ之外的就是异常值（rejection region） 
#有标准去判断，多大程度上是这一类，多大程度不是eg：3σ
#马氏距离，x1-均值，描述距离需要*协方差矩阵的逆矩阵，服从卡方分布，自由度是p，百分之99的概率都应该小于xp square，如果超出这个值，百分之99不是这个分布的
#去异常值,只是统计意义上的异常值，在现实中可能是特殊事件导致的正常结果，eg：双十一销售量
mdc1 = mdist(dc1[,-c(1,5,6)])
mdc0 = mdist(dc0[,-c(1,5,6)])
summary(mdc1)
summary(mdc0)
###此时用的是卡方分布，
c = qchisq(0.99,10) ##自由度是10 小于c的正常，大于c的异常
x1 = dc1[mdc1<c,]##y值=1的数 
x0 = dc0[mdc0<c,]## y值=0的数
dim(x1)
dim(x0)
#把数据拼起来
x= rbind(x0,x1)###最终清洗后结果！！！！！！！
mdist = function(x){ #自定义函数
t = as.matrix(x) #强制数据类型转换，转成矩阵
p = dim(t)[2] ###
m = apply(t,2,mean) ##按列求均值
s = var(t)
return(mahalanobis(t,m,s))#返回n条函数的马氏距离，运行后没有任何结果
}##用内部变量做运算
mdc = mdist(dc[,-c(1,5,6)])###bad中1和0的分布不应该一样
dim(mdc)
length(mdc)##
#####################建立逻辑回归
lg.fit= glm(BAD~.,data=x,family=binomial)
summary(lg.fit$fitted.values)
pred=predict(lg.fit)
pred1=predict(lg.fit,type="response")
pred1-lg.fit$fitted.values
pred1[pred1>0.5]<- 1##以0.5为分界线，大于0.5为1，小于0.5为0
pred1[pred1<=0.5]<- 0
s1=table(pred1,x$BAD)###对角线估的都是估对的，反对角线都是估错的
##为了让更多不还钱的人被发现，如果pred1变成0.4
pred2=predict(lg.fit,type="response")
pred2[pred2>0.4]<- 1
pred2[pred2<=0.4]<- 0
s2=table(pred2,x$BAD)##行为真值，列为预测值。
###混淆矩阵，如果做了调整，会牺牲总的准确率，但提高某些值的准确率，所以如果权重不一样时可以调整
#不让它是0.5，把从0到1的所有可能值都去计算，知道精确概率值，在所有的矩阵的关注准确率，于是产生了ROC曲线
#下面面积越大，性能更优，如果不知道临界点，通过roc曲线面积反应性能
error1=(s1[1]+s1[4])/sum(s1)
error2=(s2[1]+s2[4])/sum(s2)###牺牲准确率
#hw：4.7黑线是总准确率，是变化规律，橘黄的点和蓝线
#roc曲线是怎么来的，线上的点代表了什么
#怎么用R画roc曲线