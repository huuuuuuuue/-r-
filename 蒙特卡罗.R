####书P190
#用多元正态
n=100
for (i in 1:1000)
{
mu1=c(0,0)
sigma1=matrix(c(1,0.5,0.5,1.25),nrow=2)
rand1=mvrnorm(n=100,mu=mu1,Sigma=sigma1)
rand1
X= rand1[,1]
Y= rand1[,2]
alpha[i]=(var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
}
##有放回抽样
{
mu1=c(0,0)
sigma1=matrix(c(1,0.5,0.5,1.25),nrow=2)
rand1=mvrnorm(n=100,mu=mu1,Sigma=sigma1)
rand1
X= rand1[,1]
Y= rand1[,2]
alpha[i]=(var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
}
label=c(1:100)
rand=cbind(rand1,label)
lab=sample(c(1:100),1,replace=TRUE)
ran=rand1[label==lab,]
##每组数抽编号，先确定第一行，把他们拿下放在下方
#又放回的抽，有几组数就抽多少个

for(j in 1:1000)
{
ran=rand1[sample(c(1:100),100,replace=TRUE),]
X=ran[,1]
Y=ran[,2]
alpha[j]=(var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
}
alpha
mean(alpha)
var(alpha)
sqrt(var(alpha))
#方法2
rand1[sample(c(1:100),100,replace=TRUE),]
sample(c(1:100),100,replace=TRUE)