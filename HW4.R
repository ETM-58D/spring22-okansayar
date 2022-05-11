

library(data.table)
library(ggplot2)
library(factoextra)

#Read Movies with divide in to two groups based on names & years, 
#Add column names Year and Title
#Remove unnecessary variables which is Year
#Rotate the data.frame so that the rows become the columns (Transpose)

movies1 = read.csv("/Users/okansayar/Desktop/R/RStudio Directory/HW4_data/ETM58D_Spring22_HW4_movie_titles.txt", header=FALSE,sep="|")
colnames(movies1) = c("Title")
##str(movies1)
##movies1

moviesdata=read.table("/Users/okansayar/Desktop/R/RStudio Directory/HW4_data/ETM58D_Spring22_HW4_Netflix_data.dat", header=FALSE, sep="")
##str(moviesdata)
##moviesdata

#Merging the data sets (by column value  movie title with consumer rate)

mdata <- data.frame(c1 = movies1,  
                   c2 = moviesdata)

##mdata
##str(mdata)

## Compute euclidean distance & Implement hierarchical clustering algorithm
distances = dist(mdata[2:100], method = "euclidean")

set.seed(123) 

#Implement hierarchical clustering algorithm
#creates groups such that variance is minimized within clusters
clusterMovies = hclust(distances, method = "ward.D2")
plot(clusterMovies)


#creates groups such that mean linkage method alternative way
hc.complete=hclust(dist(moviesdata), method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", cex=.9)
#cutree(hc.complete,k=2)


#cluster evaluation

fviz_nbclust(moviesdata, kmeans, method = "silhouette")


#visualization for rectangular compartments of each cluster 
# Cut tree into 4 groups
subgrp <- cutree(clusterMovies, k = 2)
plot(clusterMovies)
rect.hclust(clusterMovies , k = 2, border = 2:6)
