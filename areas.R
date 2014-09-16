#function that tells for a given pixel, if any of the surrounding 8 pixels are equal to x
#datan is a matrix of pixels, x is an integer number, i and j are the indexes that define the pixel in question
neightbours<-function(datan,x,i,j){
        equal=FALSE
        data2<-NULL
        if(i>=2 & j>=2  ){if(datan[i-1,j-1]==x)equal=TRUE}
        if(i>=2         ){if(datan[i-1,j]==x)equal=TRUE}
        if(i>=2 & j<=27 ){if(datan[i-1,j+1]==x)equal=TRUE}
        if(j>=2         ){if(datan[i,j-1]==x)equal=TRUE}
        if(j<=27        ){if(datan[i,j+1]==x)equal=TRUE}
        if(i<=27 & j>=2 ){if(datan[i+1,j-1]==x)equal=TRUE}
        if(i<=27        ){if(datan[i+1,j]==x)equal=TRUE}
        if(i<=27 & j<=27){if(datan[i+1,j+1]==x)equal=TRUE}
        return(equal)
}

##the function that searches for the first zero or non-zero element in a matrix, and returns its index. It returns -1
##if it fails to find any
first<-function(datan, type="zero"){
        ##type = "zero" or "non-zero"
        if(type=="zero"){
                br=FALSE
                for(i1 in 1:28){
                        for(j1 in 1:28){
                                if(datan[i1,j1]==0){
                                        br=TRUE
                                        break
                                }
                        }
                        if(br){
                                break
                        }
                }
                if(!br){
                        return(-1)
                }
                else return(c(i1,j1))
        }
        else if(type=="non-zero"){
                br=FALSE
                for(i1 in 1:28){
                        for(j1 in 1:28){
                                if(datan[i1,j1]>0){
                                        br=TRUE
                                        break
                                }
                        }
                        if(br){
                                break
                        }
                }
                if(!br){
                        return(-1)
                }
                else return(c(i1,j1))
        }
}

#this function is analog of first(), but instead of looking for zero or non-zero elements, it searches for element
#in an interval from m1 to m2
first2<-function(datan,m1,m2){
        br=FALSE
        for(i1 in 1:28){
                for(j1 in 1:28){
                        if(datan[i1,j1]<m2 & datan[i1,j2]>=m1){
                                br=TRUE
                                break
                        }
                }
                if(br){
                        break
                }
        }
        if(!br){
                return(-1)
        }
        else return(c(i1,j1))
}

areas<-function(datan){
        areas=0
        bedingung=TRUE
        while(bedingung){
                f<-first(datan,type="zero")
                if(length(f)>1){
                        i1<-f[1]
                        j1<-f[2]
                        datan[i1,j1]=-1
                        changed=TRUE
                        while(changed==TRUE){
                                changed=FALSE
                                for(i in 1:28){
                                        for(j in 1:28){
                                                if(datan[i,j]==0 & neightbours(datan, x=-1,i,j)){
                                                        datan[i,j]=-1 ##if the pixel is adjacent to -1 and equals to zero, change it to -1 aswell
                                                        changed=TRUE
                                                }
                                        }
                                }
                        }
                        areas<-areas+1
                }
                else{bedingung=FALSE}
        }
        bedingung=TRUE
        while(bedingung){
                f<-first(datan,type="non-zero")
                if(length(f)>1){
                        i1<-f[1]
                        j1<-f[2]
                        datan[i1,j1]=-1
                        changed=TRUE
                        while(changed==TRUE){
                                changed=FALSE
                                for(i in 1:28){
                                        for(j in 1:28){
                                                if(datan[i,j]>0 & neightbours(datan, x=-1,i,j)){
                                                        datan[i,j]=-1 ##if the pixel is adjacent to -1 and equals to non-zero, change it to -1 aswell
                                                        changed=TRUE
                                                }
                                        }
                                }
                        }
                        areas<-areas+1
                }
                else{bedingung=FALSE}
        }
        areas
}

#this function returns the number of areas in the picture and number of pixels in each area.
#if x<-areas2(), then x[1] - number of areas, x[2] - number of pixels in first area, and so on
#m - number of times we split the color interval 0:256 into parts.
areas2<-function(datan, m=2){
        
}
##returns a vector of areas of rows in a data
data.areas<-function(data){
        ar<-NULL
        K<-dim(data)[1]
        for(k in 1:K){
                datan<-data[k,2:785]
                datan<-matrix(datan,nrow=28,ncol=28)
                ar<-c(ar,areas(datan))
        }
        ar
}


