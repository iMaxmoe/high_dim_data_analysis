# PCA and factor analysis on the depression data

# read the data from .txt
Depress = read.table(file='Depress.txt', header = F)
CESD_data = Depress[, 9:30]
NAME = as.character(1:20)
NAME = c(NAME, 'CESD', 'CASES')
colnames(CESD_data) = NAME

# use correlation matrix to do PCA
CESD.pca <- prcomp(CESD_data[, 1:20], center = TRUE, scale. = TRUE) 
# analyzing the results
print(CESD.pca)
# plot method
plot(CESD.pca, type = "l")
# summary method
summary(CESD.pca)
# Predict PCs
predict(CESD.pca, newdata = tail(CESD_data[, 1:20], 2))
# biplot
biplot(CESD.pca)
# 
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
library(vqv)
#jpeg(file='Depress.jpg')
g <- ggbiplot(CESD.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(CESD_data[, 22]), ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
#dev.off()

# factor analysis for depression data
# PCA+VARIMAX+Factor_scores
library(psych)
data = CESD_data[, 1:20]
R = cor(data)
fitmlr=fa(data, 4, rotate="varimax",fm="ml")
fitmlr$loadings
# factor scores
fsregr=factor.scores(R,fitmlr,method=c("Thurstone")) #regression scores
fsregr$weights
#plot of factor scores
fsreg=as.matrix(data)%*%fsregr$weights 
plot(fsreg[,1],fsreg[,2], pch=19, xlab="Scores for Factor 1",ylab="Scores for Factor 2") 


