# LOAD THE PACKAGE
library(xtable)
library(dlnm) ; library(mixmeta) ;library(mvmeta) ; library(splines)
library(tsModel) ; library(mgcv)
library(foreach) ; library(doParallel)
library(MASS) ; library(abind)
library(dplyr) ; library(data.table)
library(magrittr) ; library(scales)
library(Epi)
library(ggplot2)
library(gnm)
library(readxl)

mv <- mvmeta(ymat,Slist,method="ml")
summary(mv)
tmean <- seq(bound[1],bound[2],length=30)
btmean <- onebasis(tmean,fun=type,degree=degree,knots=knots,Bound=bound)

(mvreml <- mvmeta(ymat,Slist,method="reml"))
cpreml <- crosspred(btmean,coef=coef(mvreml),vcov=vcov(mvreml),
  model.link="log",by=1,cen=cen)
cp <- crosspred(btmean,coef=coef(mv),vcov=vcov(mv),model.link="log",
  by=1,cen=cen)
(sqrt(diag(vcov(mvreml))) - sqrt(diag(vcov(mv)))) / sqrt(diag(vcov(mv))) * 100

par(mar=c(5,4,2,1)+0.1,cex.axis=0.7)
layout(1)
plot(cp,"overall",ci="lines",col=2,ylab="RR9",ylim=c(0,2),xlim=c(60,650),
  xlab="Temperature (C)")
lines(cpreml,"overall",ci="lines",col=4)
legend("top",c("ML","REML"),col=c(2,4),lty=1)

cor(ymat)

cov2cor(mv$Psi)


xlab <- expression(paste("EPFRs(10^12 spins/m3)"))
par(mar=c(5, 4, 4, 3))
plot(cp,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",cex.axis=1.2)
ind1 <- cp$predvar<=30
ind2 <- cp$predvar>=30
lines(cp$predvar[ind1],cp$allRRfit[ind1],col="#F8AC8C",lwd=3)
lines(cp$predvar[ind2],cp$allRRfit[ind2],col="#F8AC8C",lwd=3)
axis(2,at=1:5*0.5,cex.axis=1.2)
breaks <- c(min(data$tmean,na.rm=T)-1,seq(cp$predvar[1],
                                          cp$predvar[length(cp$predvar)],length=30),max(data$tmean,na.rm=T)+1)
hist <- hist(data$tmean,breaks=breaks,plot=F)
hist$density <- hist$density/max(hist$density)*0.7
prop <- max(hist$density)/max(hist$counts)
counts <- pretty(hist$count,3)
par(mar=c(5, 4, 4, 3))
plot(hist,ylim=c(0,max(hist$density)*3.5),axes=F,ann=F,col="#F0FAEE",
     breaks=breaks,freq=F,add=T)
par(mar=c(5, 4, 4, 3))
axis(4,at=counts*prop,labels=counts,cex.axis=1.2)
