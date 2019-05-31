r <- 1:100000
r <- c(1:100000)
r = sample(c(1:100000),size=100000,replace=T)
plot(r)
summary(r)

n <- 100000
x <- sample(1:100000, size = n)
Mboot <- replicate(100000, expr = {
    y <- sample(x, size = n, replace = TRUE)
    median(y)
})
print(var(Mboot))
--------------------- 
    作者：yujunbeta 
来源：CSDN 
原文：https://blog.csdn.net/yujunbeta/article/details/24142545 
版权声明：本文为博主原创文章，转载请附上博文链接！

print(var(r))

store = rep(NA, 10000)
for (i in 1:10000){
    store[i] = sum(sample(1:100, rep=TRUE) == 4) >0
}
mean(store)
View(store)

a = sample(1:100, rep=TRUE)
View(a)

b = sum(sample(1:100, rep=TRUE) == 4) >0
View(b)

#--------------
mean = rep(NA,20)
for (j in 1:20) {
    
    
    store = rep(NA, 10000)
    for (i in 1:10000){
        store[i] = sum(sample(1:j, size = j, rep=TRUE) == 1) >0
        mean[j] = mean(store)
        j = j+1
    }
}
#-------------------------------
mean = rep(NA,100000)

for (i in 1:100000){
    store[i] = 1-(1-1/i)^i
}
plot(store, type = "b", ylim = c(0:1))
mean(store)
?plot