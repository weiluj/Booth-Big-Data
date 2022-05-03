# Simulation

p<-100

n<-200

n_signals<-20

q<-0.2

X<-matrix(rnorm(n*p),n,p)

# Scenario 1
# 20 signals and 80 noise coefficients

beta<-c(rep(3,n_signals),rep(0,p-n_signals))

# Scenario 2
# 100 noise coefficients

beta<-c(rep(0,n_signals),rep(0,p-n_signals))


Y<-X%*%beta+rnorm(n,0,1)

pvals<-numeric(p)

for(i in (1:p)){
	
	model<-glm(Y~X[,i])
	pvals[i]<-summary(model)$coefficients[2,4]
		
}

hist(pvals,col="lightblue",breaks=10)

pvals_ordered<-pvals[order(pvals,decreasing=F)]

plot(pvals_ordered,pch=19)

abline(0,1/p)

source("fdr.R")

cutoff <- fdr_cut(pvals, q)
cutoff

abline(h=cutoff,lty=2,col=3,lwd=3)

abline(0,q/p,col=2,lwd=2)

signif <- pvals_ordered <= cutoff  

points(pvals_ordered,
	   col=signif+1,pch=19) # The red dots are discoveries

table(pvals<=cutoff) # number of discoveries and non-discoveries

(1:p)[pvals<=cutoff]

t<-table(beta,pvals<=cutoff)

t

# what is FDP (false discovery proportion)

t[1,2]/(sum(t[,2]))
