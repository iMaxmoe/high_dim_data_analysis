# R code for financial companies
data1 = read.table("Cluster.txt", header=FALSE)
data = as.matrix( data1[, 4:10] )
data = scale(data)
# hierarical
hc.centroid = hclust(dist(data), method ='centroid')
plot(hc.centroid, hang = -1, ylab = "Distance")
# K-means
fit = kmeans(data, 3, nstart = 100)
 