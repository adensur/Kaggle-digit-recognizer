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

##analog of myknn, but should work faster. I am trying to fix speed by dynamically ordering the data
##and forgetting the useless (i.e. biggest) vectors at once instead of keeping the entire set of data.

myknn2<-function(train, test, cl, k=1){
        train<-as.matrix(train)
        test<-as.matrix(test)
        cl2<-factor(levels=levels(cl))
        if(k==1){
                for(n2 in 1:(dim(test)[1])){
                        x<-sum((test[n2,]-train[1,])^2)
                        ind=1
                        for(n1 in 2:(dim(train)[1])){
                                y<-sum((test[n2,]-train[n1,])^2)##calculate distance between n2 row of test and n1 row of train
                                if(y<x){
                                        x<-y
                                        ind<-n1
                                }
                        }
                        cl2[n2]<-cl[ind]   ##add to cl2 the class of the nearest neightbour
                }
        }
        else                     {
        for(n2 in 1:(dim(test)[1])){
                dist<-NULL
                for(i in 1:k){
                        dist<-cbind(dist,sum((test[n2,]-train[i,])^2))
                }                       ##create vector of initial distances
                dist<-cbind(t(dist),cl[1:k])
                dist<-dist[order(dist[,1]),]
                for(i in (k+1):(dim(train)[1])){
                        x<-sum((test[n2,]-train[i,])^2)
                        ##now we start comparing new vectors one by one with the existing set.
                        if(x>dist[k,1]){next}
                        for(l in (k-1):1){
                                if(x>dist[l,1]){
                                        dist[(l+1):k,1]<-c(x    ,dist[(l+1):(k-1),1])
                                        dist[(l+1):k,2]<-c(cl[i],dist[(l+1):(k-1),2])
                                        break
                                }
                        }
                }
                ##as we stopped circling through the train set, now calculate the classes of 
                ##dist and return the most popular one
                classes<-as.factor(dist[,2])
                cl2<-c(cl2,names(which.max(summary(classes))))       
        }
        }
        cl2
}