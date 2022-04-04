rm(list = ls())

SC = read.csv('C:/Users/kurtl/Downloads/semiconductor.csv')

full = glm(FAIL~.,data = SC, family = binomial)

#regression
1-full$deviance/full$null.deviance

summary(full)

pvals=summary(full)$coef[-1,4]

hist(pvals)

#apply FDR (Benjiman-Hochberg algo false discovery control)#

fdr_cut=function(pvals, q=0.1){
  pvals=sort(pvals[!is.na(pvals)])
  N=length(pvals)
  k=rank(pvals, ties.method = "min")
  alpha=max(pvals[pvals<=(q*k/(N+1))])
  
  plot(pvals, log = "xy", xlab = "order", main = sprintf("FDR of %g",q),
       ylab = "p-value", bty="n", col=c(8,2)[(pvals<=alpha)+1], pch=20)
  lines(1:N, q*(1:N)/(N+1))
  
  return(alpha)
}
fdr_cut(pvals)

(signifigant=which(pvals<=0.01217043))

names(signifigant)

cut=glm(FAIL~.,data = SC[, c("FAIL", names(signifigant))], family = "binomial")

summary(cut)