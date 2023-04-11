rm(list=ls())
set.seed(2)
#Problem A
       #Generate a simulated data set with 20 observations in each of
       #three classes (i.e. 60 observations total), and 50 variables.
x<-matrix(rnorm(20*3*50,mean=0,sd=0.1),ncol=50)
x[1:20,2]<-1
x[21:40,1]<-2
x[21:40,2]<-2
x[41:60,1]<-1
true.labels<-c(rep(1,20),rep(2,20),rep(3,20))
#Problem B
       #Perform PCA on the 60 observations and plot the first two principal 
       #component score vectors. Use a different color to indicate
       #the observations in each of the three classes. If the three classes
       #appear separated in this plot, then continue on to part (c). If
       #not, then return to part (a) and modify the simulation so that
       #there is greater separation between the three classes. Do not
       #continue to part (c) until the three classes show at least some
       #separation in the first two principal component score vectors.
pca<-prcomp(x)
plot(pca$x[,1:2],col=1:3,xlab="Z1",ylab="Z2",pch=20)
#Problem C
       #Perform K-means clustering of the observations with K = 3.
       #How well do the clusters that you obtained in K-means 
       #clustering compare to the true class labels?
km.clustering <- kmeans(x, 3, nstart = 20)
table(true.labels, km.clustering$cluster)
#Problem D
       #Perform K-means clustering with K = 2. Describe your results.
km.clustering.2 <- kmeans(x, 2, nstart = 20)
table(true.labels, km.clustering.2$cluster)
#Problem E
       #Now perform K-means clustering with K = 4, and describe your results.
km.clustering.4<- kmeans(x, 4, nstart = 20)
table(true.labels, km.clustering.4$cluster)
#Problem F
       #Now perform K-means clustering with K = 3 on the first two
       #principal component score vectors, rather than on the raw data.
       #That is, perform K-means clustering on the 60 × 2 matrix of
       #which the first column is the first principal component score
       #vector, and the second column is the second principal component
       #score vector. Comment on the results.
km.pca<-kmeans(pca$x[,1:2],3,nstart=20)
table(true.labels,km.pca$cluster)
#Problem G
       #Using the scale() function, perform K-means clustering with
       #K = 3 on the data after scaling each variable to have standard
       #deviation one. How do these results compare to those obtained
       #in (b)? Explain.
kmscale<-kmeans(scale(x),3,nstart=20)
table(true.labels,kmscale$cluster)