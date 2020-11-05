#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
library(readr)
library(cluster)
library(kselection)
library(fpc) # cluster plot
data<-read.csv("F://ExcelR//Assignment//Clustering//crime_data.csv")
View(data)
summary(data)
datanorm<-scale(data[,-1])
View(datanorm)
d<-dist(datanorm, method= "euclidean")
d
plot(d)
#model building
fit<-hclust(d, method = "ward.D2")
plot(fit) # 4 clusters
#model building
fitwd<-hclust(d, method = "ward.D") 
plot(fitwd) # 4 clusters
#"method= complete"
fitc<-hclust(d, method = "complete")
plot(fitc)
#"method= single"
fits<-hclust(d, method = "single") # single branch is sheltering all the sub branches
plot(fits) 
#"method= average"
fita<-hclust(d, method = "average") # multiple groups under one branch
plot(fita) 
#"method= mcquitty"
fitmq<-hclust(d, method = "mcquitty") # multiple groups under one branch bu tbetter clustered than "average" method
plot(fitmq) 
#"method= median"
fitm<-hclust(d, method = "median") # multiple groups under one branch, multiple small groups are formed under one branch
plot(fitm)
#"method= centroid"
fitcen<-hclust(d, method = "centroid") # multiple groups under one branch, multiple small groups are formed under one branch
plot(fitcen) 

# plotting for "ward.D2 type"

plot(fit, hang = -1)
groups<-cutree(fit, k=4)
member<-as.matrix(groups)
member
table(member)
final<-data.frame(data, member)
View(final)
attach(final)
plotcluster(datanorm, member) # plot  cluster

View(aggregate(final,by=list(member),FUN=mean))

#K means
# elbow curve
wss = (nrow(datanorm)-1)*sum(apply(datanorm, 2, var)) # Determine number of clusters by scree-plot 
for (i in 1:10) wss[i] = sum(kmeans(datanorm, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") # elbow point at 4 and 8, we go for 4
title(main = "K-Means Clustering Scree-Plot")

# model Building
kfit<- kmeans(datanorm, 4) # 4 cluster solution
str(kfit)
print(kfit) # bss/tss = 71.2%
table(kfit$cluster) # determine how many members in each of the 4 clusters
kfinal<- data.frame(data, kfit$cluster) # append cluster membership
View(kfinal)
plotcluster(datanorm,kfit$cluster) # plot the cluster
View(aggregate(kfinal,by=list(kfit$cluster),FUN=mean))

complete_data<-data.frame(data, kfit$cluster, member)
View(complete_data)
table(complete_data$kfit.cluster, complete_data$member)

# kmediod(PAM:Partition around medoid)
newdf<-pam(datanorm,4)
pam.res <- pam(datanorm, 4)
clusplot(datanorm, newdf$cluster, color = T, main = 'Cluster Plot')
print(newdf) # get mediod values and  available components
kmdf <- cbind(datanorm, cluster = newdf$cluster)
head(kmdf, n = 5)
newdf$medoids # mediod value of all crimes for 4 clusters
head(newdf$clustering)
all_clust<-data.frame(data, kfit$cluster, member,newdf$cluster)
View(all_clust)
table(all_clust$kfit.cluster,all_clust$newdf.cluster, all_clust$member)


