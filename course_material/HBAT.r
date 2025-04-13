#
#   HBAT.r    2023/03/05
#

#########################################################

HBAT <- read.csv('d:\\Lecture\\Multivariate2023\\HBATN.csv',header=T)
HBAT <- HBATN
head(HBAT)

summary(HBAT$Satisfaction)

hist(HBAT$Quality,xlab=expression(paste(X[6],sep='  ','Product Quality')),col=4)           # p.36

hist(HBAT$Quality,xlab=expression(paste(X[6],sep='  ','Product Quality')),col=3,prob=T) 
lines(density(HBAT$Quality), col = "blue",lwd=2)


library(lattice)
library(RColorBrewer)
HBATc712 <- HBAT[,c(7:12)]

summary(HBATc712)

head(HBATc712)

boxplot(HBATc712,col=c(2:8))      # p. 38

boxplot(Quality~as.factor(Customer),xlab='Customer type',data=HBAT,col=c(2:8))

qqnorm(HBAT$Quality,xlab=expression(paste(X[6],sep='  ','Product Quality')))    # p.  79
qqline(HBAT$Quality)

pairs(HBATc712)        
pairs(HBATc712, upper.panel = panel.cor,col=c(2:8))

splom(HBATc712)
####################  p. 37

splom(~HBATc712,lower.panel = panel.splom,
         upper.panel = function(x, y, ...) {
              panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y) * 4 + 5)])
              cpl <- current.panel.limits()
              ## translate upward 
              panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y),2), font=2,adj=c(0.5,-0.6))
              ## translate downward
             panel.text(mean(cpl$xlim), mean(cpl$ylim), round( cor.test(x,y)$p.value, 2), font=1, adj=c(0.5,0.6),col='blue')
          },
          scales = list(x = list( draw = TRUE, cex=0.1)), type = c("g", "p", "smooth"),layout = c(1, 1), pscales=0, pch=1
)


###############p. 67
plot(Quality~Dilivery,data=HBAT,pch=19,col=4)

library(car) 
M820 <- colMeans(HBAT[,c(20,8)])
V820 <- var(HBAT[,c(20,8)])
plot(Ecommerce~ Satisfaction,data=HBAT,pch=19,col=4,xlim=c(4,10),ylim=c(1.8,6))
ellipse(M820, shape=V820 , radius=sqrt(qchisq(0.95,2)), col="blue", lty=2)
#橢圓

D820mah <- mahalanobis(HBAT[,c(20,8)] ,M820, V820)

qqnorm(D820mah)

qqplot(qchisq(ppoints(100), df =2), sort(D820mah), ylab='Mahalanobis distance',xlab = expression("Theoretical Quantilesfor" ~~ {chi^2}[nu == 2]))
qqline(D820mah, distribution = function(p) qchisq(p, df = 2), probs = c(0.1, 0.6), col = 2)
abline(h=qchisq(0.95, df = 2),lty=2,col=3)



#############################################
#   p. 128
#
HBATc719 <- HBAT[,c(7:19)]


splom(~HBATc719,lower.panel = panel.splom,
         upper.panel = function(x, y, ...) {
              panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y) * 4 + 5)])
              cpl <- current.panel.limits()
              ## translate upward 
              panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y),2), font=2,adj=c(0.5,-0.6))
              ## translate downward
             panel.text(mean(cpl$xlim), mean(cpl$ylim), round( cor.test(x,y)$p.value, 2), font=1, adj=c(0.5,0.6),col='blue')
          },
          scales = list(x = list( draw = TRUE, cex=0.1)), type = c("g", "p", "smooth"),layout = c(1, 1), pscales=0, pch=1
)



pairs(HBAT[,c(7:19)])

var(HBAT[,c(7:19)])

HBAT.cor <- cor(HBAT[,c(7:19)]) 
round(HBAT.cor,3)                                                             # p. 128, Table 4

install.packages('EFAtools')
library('EFAtools')

HBAT.kmo <- KMO(HBAT.cor)                                                       # p. 129  Table 4
HBAT.bart <- BARTLETT(HBAT.cor,N=100)     
BARTLETT(HBATc719)


install.packages('corpcor')
library('corpcor')
round(cor2pcor(cov(HBAT[,c(7:19)])) ,3)                                                 # p. 129  Table 4

#############
HBAT.cor2 <- cor(HBAT[,c(7:15,17,19)]) 
round(HBAT.cor2,3)                                                                # p. 131, Table 5

HBAT.kmo2 <- KMO(HBAT.cor2)                                                       # p. 131  Table 5

round(cor2pcor(cov(HBAT[,c(7:15,17,19)])) ,3)                                                 # p. 131  Table 5

#############
HBAT.prinA <- princomp(HBAT[,c(7:15,17,19)])  
HBAT.prin <- princomp(HBAT[,c(7:15,17,19)],cor=T)  
HBAT.prin$sdev^2                                                 # p. 132  Table 6
HBAT.prin$sdev^2/11
cumsum(HBAT.prin$sdev^2/11)

plot(HBAT.prin)                                                 # p. 132  Figure 9

HBAT.prin$loadings



HBATc719.perr <- PARALLEL(HBAT.cor,N=100)


install.packages(psych)    
library(psych)
HBATc719.perr <- fa.parallel(HBAT.cor,n.obs=100)


HBAT.fa4 <- factanal(HBAT[,c(7:15,17,19)],factors=4,rotation ='none',scores = "regression")
 pairs(HBAT.fa4$scores)
cor(HBAT.fa4$scores)

HBAT.fa4A <- factanal(HBAT[,c(7:15,17,19)],factors=4)                                                # p. 135  Table 8


HBAT.fa4B <- factanal(HBAT[,c(7:11,13:15,17,19)],factors=4)                                    # p. 135  Table 8,     p. 137


varimax(loadings(HBAT.fa4B), normalize = FALSE)
promax(loadings(HBAT.fa4B))

HBAT.fa4C <- fa(HBAT[,c(7:15,17,19)],nfactors=4,rotate='varimax',fm='ml')

# HBAT.fa4C <- fa(HBAT[,c(7:11,13:15,17,19)],nfactors=4,rotate='oblimin',fm='minres')

print(HBAT.fa4C$loadings,cutoff = 0.4)

fa.diagram(HBAT.fa4C)



HBAT.fa4C$valid          # p. 140
HBAT.fa4C$TLI

cor(HBAT.fa4C$scores)     # p. 142

########################################################################

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)

  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}






