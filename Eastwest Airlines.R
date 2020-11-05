#Perform clustering (Both hierarchical and K means clustering) for the airlines data. 
#Obtain optimum number of clusters.

#Agglomerative clustering 
library(ggplot2)
library(readr)
library(readxl)
library(cluster)
library(dendextend) # color dendogram branches
library(fpc) 
airlines<-read_excel("F://ExcelR//Assignment//Clustering//EastWestAirlines.xlsx",2) 
View(airlines)
summary(airlines)
normalized_data<-scale(airlines[,2:11]) 
View(normalized_data)
d <- dist(normalized_data, method = "euclidean") # distance matrix
d
# Model Building
fit <- hclust(d, method="ward.D2")
plot(fit)
plot(fit, hang=-1)
groups <- cutree(fit, k=4) # cut tree into 4 clusters
class(groups) #integers
rect.hclust(fit, k=4, border="red")
membership<-as.matrix(groups)
table(membership)
final <- data.frame(airlines, membership)
View(final)
plotcluster(normalized_data,membership)
View(aggregate(final,by=list(membership),FUN=mean))# states average values for each group
ggplot(final, aes(x=Bonus_miles, y=membership, color=membership)) + geom_point(size=5)
ggplot(final, aes(x=Flight_miles_12mo, y=membership, color=membership)) + geom_point(size=5) # important plot
plotcluster(membership, airlines)

#explore setcolorder for repositioning the columns in R
# Also install the package "data.table"
install.packages("data.table")
library(data.table)
setcolorder(final,c("membership"))
View(final)

# K means algorithm, use elbow curve to identify clusters

library(kselection)
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var)) # Determine number of clusters by scree-plot 
for (i in 2:11) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:11, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") # elbow point at 4 and 8, we go for 4
title(main = "K-Means Clustering Scree-Plot")

# model Building
kfit<- kmeans(normalized_data, 4) # 4 cluster solution
str(kfit)
table(kfit$cluster) # determine how many members in each of the 4 clusters
kfinal<- data.frame(airlines, kfit$cluster) # append cluster membership
View(kfinal)
ggplot(kfinal, aes(x=Flight_miles_12mo, y=kfit.cluster, color=kfit.cluster)) + geom_point(size=2)

# agglomerative and k means cluster
final_clust<-data.frame(airlines, kfit$cluster, membership)
View(final_clust)
kmeans<-final_clust$kfit.cluster
table(kmeans) # 1:1573, 2:1003, 3:1268, 4:155
aggclust<-final_clust$membership
table(aggclust) # 1:2850, 2:1010, 3:43, 4:96






