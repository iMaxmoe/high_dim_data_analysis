library(readxl, reshape2)

EXPxHH = read_excel("TimexHHIDxExp.xlsx")
EXPxHH = EXPxHH[2:115] # The firts column is the Household ID
BrandxWeek = read_excel("BrandxWeekxAdvertise.xlsx")
#OtherxHH = read_excel("Common_to_week_vary_HH.xlsx")

#---------------------- Because BrandxWeek only has 47 unique dates -----------------#
#Get the factors
EXP_new = EXPxHH[, unique(unlist(BrandxWeek[,1]))]
p = dim(EXP_new)[1]
n = dim(EXP_new)[2]

cov_matrix = cov(EXP_new)
e = eigen(cov_matrix)

# choose 17 eigenvectors
E_value = e$values
CSUM = cumsum(e$values)/sum(e$values)
jpeg(file='eigen.jpg')
par(mfrow=c(2, 1))
plot(1:47, CSUM, type='o', pch=2, ylab='Cumulative Percentage')
plot(1:47, E_value, type='o', pch=1, ylab='Eigenvalue')
dev.off()

#------------- Get factors and advertisement vector ------------#
factor_matrix = e$vectors[,1:3]
# create a observe factor: adverting fees
brands_without_dates = BrandxWeek[, 2:ncol(BrandxWeek)]
advertising_sum_vector = rowSums(brands_without_dates) #vector of advertising minutes summed

#-------------------- perform OLS --------------------------#
#sort matricies based on times before regression
linear_reg <- lm(advertising_sum_vector ~ factor_matrix, 
                 data = data.frame(advertising_sum_vector, factor_matrix))
summary(linear_reg)