

### R code from vignette source 'exc-sol.rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: exc-sol.rnw:2-5
###################################################
options( width=90,
         SweaveHooks=list( fig=function()
           par(mar=c(3,3,1,1),mgp=c(3,1,0)/1.6,las=1,bty="n") ) )


###################################################
### code chunk number 2: exc-sol.rnw:40-41 (eval = FALSE)
###################################################
## sum( width * fval )


###################################################
### code chunk number 3: exc-sol.rnw:52-54
###################################################
library( Epi )
print( sessionInfo(), l=F )


###################################################
### code chunk number 4: exc-sol.rnw:60-63
###################################################
data( DMepi )
str( DMepi )
summary( DMepi )


###################################################
### code chunk number 5: exc-sol.rnw:66-67 (eval = FALSE)
###################################################
## ?DMepi


###################################################
### code chunk number 6: exc-sol.rnw:79-80
###################################################
head( DMepi )


###################################################
### code chunk number 7: exc-sol.rnw:96-99
###################################################
w15 <- subset( DMepi, sex=="F" & P==2015 )
w15 <- w15[order(w15$A),]
head( w15 )


###################################################
### code chunk number 8: exc-sol.rnw:106-110
###################################################
w15 <- transform( w15, mW =        D.nD / Y.nD,
                  iW =           X / Y.nD,
                  mD = pmax(0,D.DM / Y.DM,na.rm=TRUE),
                  mT =  (D.nD+D.DM)/(Y.nD+Y.DM) )


###################################################
### code chunk number 9: exc-sol.rnw:115-117
###################################################
str( w15 )
summary( w15 )


###################################################
### code chunk number 10: rates-e
###################################################
with( w15, matplot( A, cbind( mW, mD, mT, iW)*1000,
                    log="y", lwd=3, type="l", lty=1,
                    col=c("red","blue","limegreen","black") ) )
text( rep(5,4), 500*0.6^c(3,1,2,4), c("mort Well","mort DM","mort Total","DM inc"),
      col=c("red","blue","limegreen","black"), adj=0 )


###################################################
### code chunk number 11: survall
###################################################
with( w15, matplot( surv1( 1, mW )[,1], 
                    cbind( surv1( 1, mW )[,2], 
                           surv1( 1, mD )[,2],
                           surv1( 1, mT )[,2] ), 
                    lwd=3, type="l", lty=1, yaxs="i", ylim=0:1,
                    col=c("red","blue","limegreen") ) )


###################################################
### code chunk number 12: c50surv
###################################################
with( w15, surv1( 1, mW, A=50 ) )[17:23,]
with( w15, matplot( surv1( 1, mW, A=50 )[,1], 
                    cbind( surv1( 1, mW, A=50 )[,3], 
                           surv1( 1, mD, A=50 )[,3],
                           surv1( 1, mT, A=50 )[,3] ), 
                    lwd=3, type="l", lty=1, yaxs="i", ylim=0:1,
                    xlab="Age", ylab="Conditional survival given age 50",
                    col=c("red","blue","limegreen"), xlim=c(50,100) ) )


###################################################
### code chunk number 13: c50surv4
###################################################
with( w15, matplot( surv1( 1, mW, A=50 )[,1], 
                    cbind( surv1( 1, mW, A=50 )[,3], 
                           surv1( 1, mD, A=50 )[,3],
                           surv1( 1, mT, A=50 )[,3], 
                           surv2( 1, mW, mD, iW, A=50 )[,3] ), 
                    lwd=3, type="l", lty=c(1,1,1,2), yaxs="i", ylim=0:1,
                    xlab="Age", ylab="Conditional survival given age 50",
                    col=c("red","blue","limegreen","magenta"), xlim=c(50,100) ) )
text( 95, seq(0.9,0.7,,4), c("","","",""),
      col=c("red","blue","limegreen","magenta") )


###################################################
### code chunk number 14: exc-sol.rnw:226-227
###################################################
with( w15, yll( int=1, muW=mW, muD=mD, lam=iW, A=c(40,50,60,70,80) ) )


###################################################
### code chunk number 15: exc-sol.rnw:234-237
###################################################
with( w15, yll( int=1, muW=mW, muD=mD, lam=iW, A=c(40,50,60,70,80) ) )
with( w15, yll( int=1, muW=mW, muD=mD, A=c(40,50,60,70,80), n=F ) )
with( w15, yll( int=1, muW=mT, muD=mD, A=c(40,50,60,70,80), n=F ) )


###################################################
### code chunk number 16: exc-sol.rnw:248-250
###################################################
yllf2015 <- with( w15, yll( int=1, muW=mW, muD=mD, lam=iW, A=c(40:90) ) )
plot( 40:90, yllf2015[-1], type="l", lwd=3 )


###################################################
### code chunk number 17: exc-sol.rnw:254-256
###################################################
yllf2015x <- with( w15, yll( int=1, muW=mW, muD=mD, A=c(40:90) ) )
lines( 40:90, yllf2015x[-1], type="l", lwd=3, lty="22" )


###################################################
### code chunk number 18: exc-sol.rnw:259-261
###################################################
yllf2015t <- with( w15, yll( int=1, muW=mT, muD=mD, A=c(40:90), note=F ) )
lines( 40:90, yllf2015t[-1], type="l", lwd=3, lty="44" )


###################################################
### code chunk number 19: compyll
###################################################
plot( 40:90, yllf2015 [-1], type="l", lwd=3, ylim=c(0,8), yaxs="i" )
lines( 40:90, yllf2015x[-1], type="l", lwd=3, lty="12" )
lines( 40:90, yllf2015t[-1], type="l", lwd=3, lty="53" )


###################################################
### code chunk number 20: exc-sol.rnw:326-332
###################################################
data( DMepi )
DMepi <- transform( DMepi, A = A + 0.5,
                    P = P + 0.5,
                    D.T = D.nD + D.DM,
                    Y.T = Y.nD + Y.DM )
DMepi <- subset( DMepi, A>30 )


###################################################
### code chunk number 21: exc-sol.rnw:342-346
###################################################
( a.kn <- seq(40,95,,7) )
( i.kn <- seq(35,85,,9) )
( p.kn <- seq(1997,2015,,5) )
( c.kn <- seq(1910,1975,,7) )


###################################################
### code chunk number 22: exc-sol.rnw:348-357 (eval = FALSE)
###################################################
## # Alternative using total deaths to determine knots
## allA <- with( DMepi, rep(  A,D.T) ) ; allA <- allA + runif(length(allA),-0.5,0.5)
## aliA <- with( DMepi, rep(  A,X  ) ) ; aliA <- aliA + runif(length(aliA),-0.5,0.5)
## allP <- with( DMepi, rep(P  ,D.T) ) ; allP <- allP + runif(length(allP),-0.5,0.5)
## allC <- with( DMepi, rep(P-A,D.T) ) ; allC <- allC + runif(length(allC),-0.5,0.5)
## ( a.kn <- quantile( allA, (1:9-0.5)/9 ) )
## ( i.kn <- quantile( aliA, (1:9-0.5)/9 ) )
## ( p.kn <- quantile( allP, (1:5-0.5)/5 ) )
## ( c.kn <- quantile( allC, (1:6-0.5)/6 ) )


###################################################
### code chunk number 23: exc-sol.rnw:361-369
###################################################
ae <- xtabs( cbind(D.T,D.nD,D.DM,X) ~ cut(A,c(30,a.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(ae,1), col.vars=3:2 )
ie <- xtabs( cbind(D.T,D.nD,D.DM,X) ~ cut(A,c(30,i.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(ie,1), col.vars=3:2 )
pe <- xtabs( cbind(D.T,D.nD,D.DM,X) ~ cut(P,c(1990,p.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(pe,1), col.vars=3:2 )
ce <- xtabs( cbind(D.T,D.nD,D.DM,X) ~ cut(P-A,c(-Inf,c.kn,Inf)) + sex, data=DMepi )
ftable( addmargins(ce,1), col.vars=3:2 )


###################################################
### code chunk number 24: exc-sol.rnw:379-393
###################################################
mW.m <- glm( D.nD ~ Ns(A  ,knots=a.kn,int=TRUE) +
               Ns(  P,knots=p.kn,ref=2005) +
               Ns(P-A,knots=c.kn,ref=1950), 
             offset = log(Y.nD),
             family = poisson,
             data = subset( DMepi, sex=="M" ) )
mD.m <- update( mW.m,  D.DM ~ . , offset=log(Y.DM) )
mT.m <- update( mW.m,  D.T  ~ . , offset=log(Y.T ) )
iW.m <- glm(    X ~ Ns(A  ,knots=i.kn,int=TRUE) +
                  Ns(  P,knots=p.kn,ref=2005) +
                  Ns(P-A,knots=c.kn,ref=1950), 
                offset = log(Y.nD),
                family = poisson,
                data = subset( DMepi, sex=="M" ) )


###################################################
### code chunk number 25: exc-sol.rnw:397-401
###################################################
mW.f <- update( mW.m, data = subset( DMepi, sex=="F" ) )
mD.f <- update( mD.m, data = subset( DMepi, sex=="F" ) )
mT.f <- update( mT.m, data = subset( DMepi, sex=="F" ) )
iW.f <- update( iW.m, data = subset( DMepi, sex=="F" ) )


###################################################
### code chunk number 26: exc-sol.rnw:406-415
###################################################
nd <- data.frame( A = 30:99+0.5,
                  P = 2015.5,
                  Y.nD = 1,
                  Y.DM = 1,
                  Y.T  = 1 )
muW.f <- ci.pred( mW.f, nd )
muD.f <- ci.pred( mD.f, nd )
muT.f <- ci.pred( mT.f, nd )
lam.f <- ci.pred( iW.f, nd )  


###################################################
### code chunk number 27: rates-es
###################################################
clr <- c("red","blue","limegreen","black")
with( w15, matplot( A, cbind( mW, mD, mT, iW)*1000, lwd=3,
                    log="y", type="l", lty=1,
                    col=clr ) )
matlines( nd$A, cbind(muW.f,muD.f,muT.f,lam.f)*1000,
          lty=1, lwd=c(3,1,1),
          col=rep(clr,each=3) )
cn <- par("usr")
text( rep( cn[1:2]%*%c(8,2)/10, 4 ),
      10^((cbind(9:6,1:4)/10)%*%cn[4:3])[c(3,1,2,4)],
      c("no DM mort","DM mort","Total mort","DM inc"), adj=1,
      col=clr )


###################################################
### code chunk number 28: exc-sol.rnw:439-443
###################################################
yllf2015s <- with( w15, yll( int=1, muW=mW, muD=mD, lam=iW, 
                             age.in=0, A=c(40:90) ) )
yllf2015i <- with( w15, yll( int=1, muW=mW, muD=mD, age.in=0, A=c(40:90) ) )
yllf2015t <- with( w15, yll( int=1, muW=mT, muD=mD, age.in=0, A=c(40:90), note=F ) )


###################################################
### code chunk number 29: exc-sol.rnw:447-450
###################################################
yllsf2015s <- yll( int=1, muW=muW.f[,1], muD=muD.f[,1], lam=lam.f[,1], age.in=30, A=c(40:90) )
yllsf2015i <- yll( int=1, muW=muW.f[,1], muD=muD.f[,1], age.in=30, A=c(40:90) ) 
yllsf2015t <- yll( int=1, muW=muT.f[,1], muD=muD.f[,1], age.in=30, A=c(40:90), note=F ) 


###################################################
### code chunk number 30: compyll2
###################################################
plot( 40:90, yllf2015s [-1], type="l", lwd=3, ylim=c(0,8), yaxs="i", col=gray(0.6) )
lines( 40:90, yllf2015i [-1], type="l", lwd=3, lty="12", col=gray(0.6) )
lines( 40:90, yllf2015t [-1], type="l", lwd=3, lty="53", col=gray(0.6) )
# smoothed estimates
lines( 40:90, yllsf2015s[-1], type="l", lwd=3 )
lines( 40:90, yllsf2015i[-1], type="l", lwd=3, lty="12" )
lines( 40:90, yllsf2015t[-1], type="l", lwd=3, lty="53" )


###################################################
### code chunk number 31: exc-sol.rnw:475-485
###################################################
# ages and dates for computing YLL
a.ref <- 30:90
p.ref <- 1996:2016
# array to hold results
aYLL <- NArray( list( type = c("Sus","Imm","Tot","I-S","I-S Ex%","T-S","T-S Ex%"),
                      sex = levels( DMepi$sex ),
                      age = a.ref,
                      date = p.ref ) )
str( aYLL )
dim( aYLL ) ; prod( dim( aYLL ) )


###################################################
### code chunk number 32: exc-sol.rnw:507-530
###################################################
system.time(
  for( ip in p.ref )
  {
    nd <- data.frame( A = seq(30.2,100,0.2)-0.1,
                      P = ip,
                      Y.nD = 1,
                      Y.DM = 1,
                      Y.T  = 1 )
    muW.m <- ci.pred( mW.m, nd )[,1]
    muD.m <- ci.pred( mD.m, nd )[,1]
    muT.m <- ci.pred( mT.m, nd )[,1]
    lam.m <- ci.pred( iW.m, nd )[,1]
    muW.f <- ci.pred( mW.f, nd )[,1]
    muD.f <- ci.pred( mD.f, nd )[,1]
    muT.f <- ci.pred( mT.f, nd )[,1]
    lam.f <- ci.pred( iW.f, nd )[,1]
    aYLL["Imm","M",,paste(ip)] <- yll( int=0.2, muW.m, muD.m, lam=NULL , A=a.ref, age.in=30, n=F )[-1]
    aYLL["Imm","F",,paste(ip)] <- yll( int=0.2, muW.f, muD.f, lam=NULL , A=a.ref, age.in=30, n=F )[-1]
    aYLL["Tot","M",,paste(ip)] <- yll( int=0.2, muT.m, muD.m, lam=NULL , A=a.ref, age.in=30, n=F )[-1]
    aYLL["Tot","F",,paste(ip)] <- yll( int=0.2, muT.f, muD.f, lam=NULL , A=a.ref, age.in=30, n=F )[-1]
    aYLL["Sus","M",,paste(ip)] <- yll( int=0.2, muW.m, muD.m, lam=lam.m, A=a.ref, age.in=30, n=F )[-1]
    aYLL["Sus","F",,paste(ip)] <- yll( int=0.2, muW.f, muD.f, lam=lam.f, A=a.ref, age.in=30, n=F )[-1]
  } )


###################################################
### code chunk number 33: exc-sol.rnw:534-538
###################################################
aYLL["I-S"    ,,,] <-  aYLL["Imm",,,] - aYLL["Sus",,,]
aYLL["I-S Ex%",,,] <- (aYLL["Imm",,,] - aYLL["Sus",,,]) / aYLL["Sus",,,] * 100
aYLL["T-S"    ,,,] <-  aYLL["Tot",,,] - aYLL["Sus",,,]
aYLL["T-S Ex%",,,] <- (aYLL["Tot",,,] - aYLL["Sus",,,]) / aYLL["Sus",,,] * 100


###################################################
### code chunk number 34: exc-sol.rnw:544-546
###################################################
round( ftable( aYLL[,,seq(11,51,10),seq(1,21,5)], col.vars=c(3,2), row.vars=c(4,1) ), 1 )
round( ftable( aYLL[,,seq(11,51,10),seq(1,21,5)], col.vars=c(2,3), row.vars=c(1,4) ), 1 )


###################################################
### code chunk number 35: ImmYll
###################################################
par( mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, bty="n", las=1 )

matplot( a.ref, aYLL["Sus","M",,],
         type="l", lty=1, col="blue", lwd=1:2,
         ylim=c(0,12), xlab="Age",
         ylab="Years lost to DM", yaxs="i" )
abline(v=50,h=1:10,col=gray(0.7))
text( 90, 11, "Men", col="blue", adj=1 )
text( 40, aYLL["Sus","M","40","1996"], "1996", adj=c(0,0), col="blue" )
text( 43, aYLL["Sus","M","44","2016"], "2016", adj=c(1,1), col="blue" )

matplot( a.ref, aYLL["Sus","F",,],
         type="l", lty=1, col="red", lwd=1:2,
         ylim=c(0,12), xlab="Age",
         ylab="Years lost to DM", yaxs="i" )
abline(v=50,h=1:10,col=gray(0.7))
text( 90, 11, "Women", col="red", adj=1 )
text( 40, aYLL["Sus","F","40","1996"], "1996", adj=c(0,0), col="red" )
text( 43, aYLL["Sus","F","44","2016"], "2016", adj=c(1,1), col="red" )


###################################################
### code chunk number 36: ImmYll
###################################################
plyll <- function(wh){
  par( mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, bty="n", las=1 )
  
  matplot( a.ref, aYLL[wh,"M",,],
           type="l", lty=1, col="blue", lwd=1:2,
           ylim=c(0,12), xlab="Age",
           ylab="Years lost to DM", yaxs="i" )
  abline(v=50,h=1:12,col=gray(0.7))
  text( 90, 11.5, "Men", col="blue", adj=1 )
  text( 40, aYLL[wh,"M","40","1996"], "1996", adj=c(0,0), col="blue" )
  text( 43, aYLL[wh,"M","44","2016"], "2016", adj=c(1,1), col="blue" )
  
  matplot( a.ref, aYLL[wh,"F",,],
           type="l", lty=1, col="red", lwd=1:2,
           ylim=c(0,12), xlab="Age",
           ylab="Years lost to DM", yaxs="i" )
  abline(v=50,h=1:12,col=gray(0.7))
  text( 90, 11.5, "Women", col="red", adj=1 )
  text( 40, aYLL[wh,"F","40","1996"], "1996", adj=c(0,0), col="red" )
  text( 43, aYLL[wh,"F","44","2016"], "2016", adj=c(1,1), col="red" )
}
plyll("Imm")


###################################################
### code chunk number 37: TotYll
###################################################
plyll("Tot")


###################################################
### code chunk number 38: SusYll
###################################################
plyll("Sus")


###################################################
### code chunk number 39: Yllc
###################################################
par( mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, bty="n", las=1 )
matplot( a.ref, cbind(aYLL["Imm","M",,],aYLL["Imm","F",,]),
         type="l", lty=1, col=rep(c("blue","red"),each=18), lwd=1:2,
         ylim=c(0,12), xlab="Age",
         ylab="Years lost to DM", yaxs="i" )
abline(v=50,h=1:12,col=gray(0.7))
text( 40, aYLL["Imm","F","40","1996"], "1996", adj=c(0,0) )
text( 43, aYLL["Imm","F","44","2016"], "2016", adj=c(1,1) )
mtext( "Immunity to DM", side=3 )

matplot( a.ref, cbind(aYLL["Sus","M",,],aYLL["Sus","F",,]),
         type="l", lty=1, col=rep(c("blue","red"),each=18), lwd=1:2,
         ylim=c(0,12), xlab="Age",
         ylab="Years lost to DM", yaxs="i" )
abline(v=50,h=1:12,col=gray(0.7))
text( 40, aYLL["Sus","F","40","1996"], "1996", adj=c(0,0) )
text( 43, aYLL["Sus","F","44","2016"], "2016", adj=c(1,1) )
mtext( "Susceptible to DM", side=3 )


###################################################
### code chunk number 40: CmpYll
###################################################
cpyll <- 
  function(wh)
  {
    par( mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(3,1,0)/1.6, bty="n", las=1 )
    for( sx in c("M","F") )
    { 
      matplot( a.ref, t(aYLL[c("Sus","Tot","Imm"),sx,,paste(wh[1])]),
               col="transparent",
               ylim=c(0,12), xlab="Age", ylab="Years lost to DM", yaxs="i" )
      for( yy in wh ) 
        matlines( a.ref, t(aYLL[c("Sus","Tot","Imm"),sx,,paste(yy)]),
                  type="l", lty=c("solid","63","22"), lend="butt",
                  col=if(sx=="M") "blue" else "red", lwd=2 )
      abline(h=1:12,v=50,col=gray(0.7))
      text( 90, 11.5, sx, col=if( sx=="M") "blue" else "red", adj=1 )
    }
  }
cpyll( seq(1996,2016,10) )



