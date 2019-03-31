# Preprocessing, centering and scaling first
data_original = read.csv('germancredit_reduced.csv')
data_original = as.data.frame(data_original)
logit1 = glm(Default ~ ., data = data_original, family = binomial(link = "logit"))
summary(logit1)

# select 9 variables that are significant at alpha = 0.05
data1 = as.matrix(data_original[,c('checkingstatus','duration','history','amount',
                                   'employ','installment','property','cards','tele','Default')])
boxplot.matrix(data1)

data1 = as.data.frame(data1)
logit2 = glm(Default ~ ., data = data1, family = binomial(link = "logit"))
summary(logit2)

# find the correlation
data_original = as.data.frame(data_original)
cor(data1[,'Default'], data1)
cor(data_original[,'Default'],  data_original)

# delete all the outliers
#for (i in 1:1){
#  data1 = data1[!data1[,'amount'] %in% boxplot.stats(data1[,'amount'])$out,]
#  data1 = data1[!data1[,'duration'] %in% boxplot.stats(data1[,'duration'])$out,]
#  data_original = data_original[!data_original[,'amount'] %in% boxplot.stats(data_original[,'amount'])$out,]
#  data_original = data_original[!data_original[,'duration'] %in% boxplot.stats(data_original[,'duration'])$out,]
#  # boxplot(data1[,'amount'])
#}

# cor(data1[,'Default'], data1)
# cor(data_original[,'Default'],  data_original)


# boxplot.matrix(data1)
# data1[,'amount'] = scale(data1[,'amount'], center = TRUE, scale = TRUE)

# scale the data
# data1 = as.matrix(cbind(scale(data1[,1:6], center = TRUE, scale = TRUE),data1[,7]))
boxplot.matrix(as.matrix(data1[,1:9]), main = 'Boxplots')

set.seed(123)

############K-means Clustering ###############
data1 = as.matrix(data1)
km_out = kmeans(data1[,1:9], 2, nstart = 100)
ones = rep(1,length(data1[,1]))
cluster = as.matrix(km_out$cluster - ones)
for(i in 1: length(cluster)){
  if(cluster[i] == 1) {
    cluster[i] = 0
  }
  else{
    cluster[i] = 1
  }
}

print(sum(cluster == data1[,'Default']))
print(km_out$betweenss / km_out$totss)

print(km_out$totss)
print(km_out$tot.withinss)
print(km_out$betweenss)

TP = 0 # actual: default = 1, predict: default = 1
FN = 0 # actual: default = 1, predict: default = 0
TN = 0 # actual: default = 0, predict: default = 0
FP = 0 # actual: default = 0, predict: default = 1
for(i in 1: length(data1[,1])){
  if(data1[i,'Default'] == 1){
    if(cluster[i] == 1){
      TP = TP + 1
    }
    else{
      FN = FN + 1
    }
  }
  else{
    if(cluster[i] == 1){
      FP = FP + 1
    }
    else{
      TN = TN + 1
    }
  }
}
# TP=72, FN=228, FP 101, TN 599
# > km_out$totss
# [1] 7960027369
# > km_out$tot.withinss
# [1] 2405315502
# > km_out$betweenss
# [1] 5554711867


############Hierarchical Clustering ###############

hc.complete = hclust(dist(data1), method = 'complete')
cluster = cutree(hc.complete, 2) - ones
print(sum(cluster == data1[,'Default']))

TP = 0 # actual: default = 1, predict: default = 1
FN = 0 # actual: default = 1, predict: default = 0
TN = 0 # actual: default = 0, predict: default = 0
FP = 0 # actual: default = 0, predict: default = 1
for(i in 1: length(data1[,1])){
  if(data1[i,'Default'] == 1){
    if(cluster[i] == 1){
      TP = TP + 1
    }
    else{
      FN = FN + 1
    }
  }
  else{
    if(cluster[i] == 1){
      FP = FP + 1
    }
    else{
      TN = TN + 1
    }
  }
}

# TP 31, FN 269, FP 25, TN 675
