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
data<-read.csv("train.csv")
data.train<-data[1:21000,2:785]
data.test<- data[21001:42000,2:785]
class<-as.factor(data[1:21000,1])
kNN<-knn(data.train,data.test,cl=class)
class.test<-as.factor(data[21001:42000,1])
equal<-kNN==class.test
sum(equal)/length(equal)
