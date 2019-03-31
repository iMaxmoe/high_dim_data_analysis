# read in
sample_stocks = read.csv('sample_stocks.csv')
attach(sample_stocks)

# ordinary scatterplot
plot(returns, dividendyield, main = "Scatter Plot", xlab = "Returns", ylab = "Dividend Yield")
abline(lm(dividendyield ~ returns),col = "blue")
# lines(lowess(returns,dividendyield),col = "blue")

# enhanced version of scatterplot
library(car)
scatterplot(dividendyield ~ returns, main = "Scatter Plot", xlab = "Returns", ylab = "Dividend Yield")

# determine the number of clusters
wss = (nrow(sample_stocks)-1)*sum(apply(sample_stocks, 2, var))
for (i in 2:10) 
  wss[i] = sum(kmeans(sample_stocks, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares", 
     main ="Plot for SS_Within")

# K-Means Cluster Analysis
fit = kmeans(sample_stocks, 3) # 3 cluster solution

# get cluster means
aggregate(sample_stocks, by=list(fit$cluster), FUN=mean)
# append cluster assignment
sample_stocks = data.frame(sample_stocks, fit$cluster)
sample_stocks
sample_stocks$fit.cluster = as.factor(sample_stocks$fit.cluster)

# plot the clustering result
library(ggplot2)
ggplot(sample_stocks, aes(x=returns, y=dividendyield, color = sample_stocks$fit.cluster)) +
  geom_point() + labs(color = "Clusters")+ ggtitle("Clustering Result") +xlab("Returns") +
  ylab("Dividend Yield")


# hierarchical clustering
hc.complete = hclust(dist(sample_stocks), method = 'complete')
plot(hc.complete,hang = -1,ylab = "Distance")