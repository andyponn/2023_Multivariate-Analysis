crime = read.table("C:/Users/User/Desktop/citycrime.txt") 
pairs(crime)
cor(crime)
library(stats) 
pca.crime<-princomp(crime,cor=TRUE) 
summary(pca.crime) 
loadings(pca.crime)
#Calculate the PC scores.
pcs.crime<-predict(pca.crime)
#Check out the screeplot.
eigen<-eigen(cor(crime))
plot(eigen$values,type="h")
#Plot the first 2 PCs.
plot(pcs.crime[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs.crime[,1:2],row.names(crime)) 
#Plot also the biplot. 
biplot(pca.crime,scale=1) 


#Permutation Test
sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  
  # run PCA
  
  pc.out<-princomp(x,cor=cor,...)
  
  # the proportion of variance of each PC
  
  pve=(pc.out$sdev^2/m)[1:m]
  
  # a matrix with R rows and m columns that contains
  
  # the proportion of variance explained by each pc
  
  # for each randomization replicate.
  
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  
  for(i in 1:R){
    
    # permutation each column
    
    x.perm<-apply(x,2,sample)
    
    # run PCA
    
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    
    # the proportion of variance of each PC.perm
    
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
    
  }
  
  # calcalute the p-values
  
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  
  return(list(pve=pve,pval=pval))
  
}


###The above function implements R=1000 replicates of the data (permutation within columns) and reports the p-values for all observed (£fi/m) (i.e., proportion of total variance explained by the i-th PC) based on the permutation distribution.

install.packages('RCurl')
library(RCurl)
sign.pc(crime,cor=T)

#The p-values show that merely the 1st PC is significant!