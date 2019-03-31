#--------------------------------Do PCA (2 methods)---------------------------#
brownie.data = read.csv('brownie.csv')
demographic = brownie.data

# method I: use eigen-decomposition
#----------------------- With Evectors and Eigenvalues -----------------------#
cov_matrix = cov(demographic)
#finding eigendvectors and eigendvalues; eigendvalues are always returned in decreasing order
#values are associated (location) with vectors
eigendStuff = eigen(cov_matrix)
eVectors = eigendStuff$vectors
eValues = eigendStuff$values 
eigendStuff #print eigendvalues and vectors

# method II: use the R function
#------------------------- PCA with Package ----------------------------------#
br.pca <- prcomp(demographic)
br.pca    #print pca

# plot eigenvalues
cumu = cumsum(eValues)/sum(eValues)

jpeg('eigen.jpg')
par(mfrow=c(1, 2))
plot(1:8, cumu, type='o', pch=3, xlab='Principle Component Number', ylab='Cumulative percentage')
plot(1:8, eValues, type='o', pch=2, xlab='Principle Component Number', ylab='Eigenvalues')
dev.off()

#install_github("ggbiplot", "vqv")
library(devtools)
library(ggbiplot)
jpeg(file='pca_market.jpg')
g <- ggbiplot(br.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
dev.off()





       
