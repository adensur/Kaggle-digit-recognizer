##KNN
data<-read.csv("train.csv")
km<-kmeans(data[,2:785], centers=10, iter.max=1000, nstart=10)
for(i in 1:10){
        data2<-data[km$cluster==i,1]
        f2<-as.factor(data2)
        print(paste(c("Accuracy percent = ", max(summary(f2))/length(f2),
              " Number = ", names(which.max(summary(f2)))),collapse=""))
}
M<-matrix(km$centers[9,],ncol=10,nrow=10)
image(M, col=grey(seq(1,0,length=256)))

##areas check
K=1000
data<-read.csv("train.csv",nrow=1000)
source("areas.R")
ar<-data.areas(data)
data2<-cbind(data[,1],ar)
data8<-data2[data2[,1]==8,]
data8

l=160
##View(matrix(data[l,2:785],ncol=28,nrow=28))
data[l,1]
datan<-unlist(data[l,2:785])
matr<-matrix(ncol=28,nrow=28)
for(i in 1:28){
        for(j in 1:28){
                matr[i,j]<-datan[i*28+j]
        }
}
image(matr, col=grey(seq(1,0,length=256)))



##R kNN. [1] 0.9599524
library(class)
k=1        ##number of nearest neightbours
K=1000      ##number of data points compared
half<-K%/%2 ## index of the middle
data<-read.csv("train.csv",nrow=K)
data.train<-data[1:half,2:785]
data.test<- data[(half+1):K,2:785]
class<-as.factor(data[1:half,1])
system.time(kNN<-knn(data.train,data.test,cl=class))

class.test<-as.factor(data[(half+1):K,1])
equal<-kNN==class.test
result<-sum(equal)/length(equal)
result


##my KNN
k=1        ##number of nearest neightbours
K=1000     ##number of data points compared. max = 42000
half<-K%/%2 ## index of the middle
data<-read.csv("train.csv",nrow=K)
train<-data[1:half,2:785]
test<-data[(half+1):K,2:785]
cl<-as.factor(data[1:half,1])
##
mykNN<-myknn(train,test,cl,k)

##
system.time(mykNN<-myknn(train,test,cl,k))
system.time(mykNN2<-myknn2(train,test,cl,k))
##
myclass.test<-as.factor(data[(half+1):K,1])
myequal<-mykNN==myclass.test
my.result<-sum(myequal)/length(myequal)
my.result
##
myequal2<-mykNN2==myclass.test
my.result2<-sum(myequal2)/length(myequal2)
my.result2

