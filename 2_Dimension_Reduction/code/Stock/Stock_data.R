library(psych)
# use command fa for mle. Principal components from E9-4r.
stock=read.table("T8-4.DAT")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
fit=fa(stock,2,rotate="none",fm="ml",residuals=TRUE)
fit                #slight differences from text
fit$loadings  #loadings, communalities, and specific variances
residuals(fit)  #residuals but with 0 replaced by specific variances on diagonals

fitpc=principal(stock,nfactors=2,residuals=TRUE,rotate="none")
fitpc
fitpc$loadings  #loadings, communalities, and specific variances
residuals(fitpc)



# use command fa form psych library for mle
library(psych)
stock=read.table("T8-4.dat")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
fit=fa(stock,2,rotate="none",fm="ml",residuals=TRUE)
fit$loadings  #loadings, communalities, and specific variances
residuals(fit)   #comunalities on diagonal
fitr=fa(stock,2,rotate="varimax",fm="ml",residuals=TRUE)
fitr$loadings  #loadings, communalities, and specific variances
# loadings slightly  different from text


# use command fa form psych library for mle
library(psych)
stock=read.table("T8-4.dat")
names(stock)=c("JpMorgan","Citibank","WellsF","RDShell","ExonMob")
R=cor(stock)
fitmlr=fa(stock,2,rotate="varimax",fm="ml")
fitmlr$loadings  #loadings, communalities, and specific variances
z=c(.5,-1.4,-.2,-.7,1.4)  #the argument at which to evaluate factor scores
fsregr=factor.scores(R,fitmlr,method=c("Thurstone")) #regression scores
t(fsregr$weights)%*%z
fsBartr=factor.scores(R,fitmlr,method=c("Bartlett"))
t(fsBartr$weights)%*%z  #both sets of factor scores change a little when more digits
# are carried in the calculation plot
#plot of factor scores
fsreg=as.matrix(stock,103,5)%*%fsregr$weights        #  create regression factor scores
plot(fsreg[,1],fsreg[,2], pch=19, xlab="Scores for Factor 1",ylab="Scores for Factor 2") 