myknn<-function(train, test, cl, k=1){
        train<-as.matrix(train)
        test<-as.matrix(test)
        cl2<-NULL
        for(n2 in 1:(dim(test)[1])){
                dist<-NULL
                for(n1 in 1:(dim(train)[1])){
                        dist<-c(dist, sum((test[n2,]-train[n1,])^2))##calculate distance between n2 row of test and n1 row of train
                }
                k.near<-order(dist)[1:k]     ##choose indexes of k nearest neightbours
                classes<-cl[k.near]
                cl2<-c(cl2,names(which.max(summary(classes))))   ##add to cl2 the name of the most popular class
        }
        cl2
}