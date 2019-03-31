library(MASS)
library(klaR)

data = read.csv('banknote.csv')

x = data[,1:6] # six measurements
summary(x) # summary statistics

Type = as.factor(data[,7])

# scatterplot matrix
pairs(data[,1:6], main = "Scatterplot Matrix", upper.panel = NULL)

##### Checking the performance of LDA
n = 200
n_train = 100
iteration = 10
set.seed(123)

# LDA on the whole dataset
fit = lda(Type ~ . , data, prior = c(0.01,0.99))
print(fit)
plot(fit)

# one iteration
train = sample(1:n, n_train)
fit = lda(Type ~ . , data[train,], prior = c(0.01,0.99))
print(table(data$Type[-train],predict(fit, data[-train,])$class))
print(fit)

# ten more iterations
for(i in 1:iteration){
  train = sample(1:n, n_train)
  fit = lda(Type ~ . , data[train,], prior = c(0.01,0.99))
  table = table(data$Type[-train],predict(fit, data[-train,])$class)
  aper = (table[2,1]+table[1,2])/100
  print(aper)
}

partimat(x, Type, method = "lda")