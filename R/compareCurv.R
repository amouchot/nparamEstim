#' Centile curves using local polynomial compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the method chosen to calculate bandwidth. Could be cross validation or Plug-in. Default is set to CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#' @param data the noise data we want to compare
#' @return Plots centile curves with local constant polynomial and displays them on the same figure as the noise data to be compared
#'
#' @importFrom locpol locCteWeightsC gaussK
#' @import graphics
#' @import stats
#'
#' @export
#' @examples
#' #create an example data frame
#' example<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' localCst0(x,y,bandwidth.method="Plug-in",cents=c(0.03,0.25,0.50,0.75,0.97),data=abnormal)
#'
#'
localCst0<-function(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data){
  x_seq=seq(min(x),max(x),0.01)
  bandwidth<-window(x,y,method=bandwidth.method)
  lcw<-locpol::locCteWeightsC(x,x_seq,bandwidth,gaussK)$locWeig
  m= lcw %*% y
  v= lcw %*% (y*y)-m^2
  s=sqrt(v)
  centiles<- data.frame(sapply(cents, function(x) m+s*qnorm(x)))
  result<-c(bandwidth,centiles)
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",lwd=1,col="white",ylim=c(400,6000))
  title(main='Local constant polynomial')
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  for (value in centiles){
    j=j+1
    lines(x_seq,value,type="l",lty=lty[j],lwd=2,col=colors[j])
  }
  points(data,col="black",pch=1,cex=0.5)
  legend("bottomright", legend=c("Abnormal data"),
         col=c("black"), pch=1, cex=0.4)
}
#' Centile curves using local linear polynomial compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the method chosen to calculate bandwidth. Could be cross validation or Plug-in. Default is set to CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @param data the noise data we want to compare
#' @return Plots centile curves with local linear polynomial using a Gaussian kernel and displays them on the same figure as the noise data to be compared
#'
#' @importFrom locpol locLinWeightsC gaussK
#' @import graphics
#' @export
#'
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' localLin0(x,y,bandwidth.method="Plug-in",cents=c(0.03,0.25,0.50,0.75,0.97),data=abnormal)
#'
#'
localLin0<-function(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data){
  x_seq=seq(min(x),max(x),0.01)
  bandwidth<-window(x,y,method=bandwidth.method)
  lcw<-locpol::locLinWeightsC(x,x_seq,bandwidth,gaussK)$locWeig
  m= lcw %*% y
  v= lcw %*% (y*y)-m^2
  s=sqrt(v)
  centiles<- data.frame(sapply(cents, function(x) m+s*qnorm(x)))
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",col="white",ylim=c(400,6000))
  title(main='Local linear polynomial')
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  for (value in centiles){
    j=j+1
    lines(x_seq,value,type="l",lty=lty[j],lwd=2,col=colors[j])
  }
  points(data,col="black",pch=1,cex=0.5)
  legend("bottomright", legend=c("Abnormal data"),
         col=c("black"), pch=1, cex=0.4)
}

#' Centile curves using cubic splines compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#' @param data the noise data to be compared to
#'
#' @return Plots centile curves with cubic splines and displays them on the same figure as the noise data
#'
#' @importFrom fields qsreg
#' @import graphics
#' @export
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,10,rep=TRUE),sample(800:5000,10,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' cubicSpline0(x,y,cents=c(0.03,0.25,0.5,0.75,0.97),data=abnormal)
#'
#
cubicSpline0<-function(x,y,cents=c(0.03,0.25,0.5,0.75,0.97),data){
  col=c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",main = "Cubic Splines",col="white",ylim=c(400,6000))
  i=0
  for(cent in cents){
    i=i+1
    fit.robust<-fields::qsreg(x,y,alpha =cent)
    lines(fit.robust$predicted$x,fit.robust$predicted$y[,25],type="l",lty=lty[i],lwd=3,col=col[i])
  }
  points(data,col="black",pch=1,cex=0.5)
  legend("bottomright", legend=c("Abnormal data"),
         col=c("black"), pch=1, cex=0.4)}

#' Centile curves using B-splines compared to noise data
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param data the noise data to be compared to
#' @param lambdas to be set by user. Can be a vector or a single numeric value. Tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots centile curves with B-splines of different differential orders (d) and displays them on the same figure as the noise data
#' @export
#'
#' @importFrom quantregGrowth ps gcrq
#' @import graphics
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,30,rep=TRUE),sample(800:5000,30,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' abnormal<-data.frame(sample(30:42,6,rep=TRUE),sample(800:5000,6,rep=TRUE))
#' colnames(abnormal)<-c("Gestational Age in weeks","Weight in gramms")
#' BSplines0(x,y,lambdas=1,abnormal)
#'
#'
BSplines0<-function(x,y,lambdas,data,cents=c(0.03,0.25,0.5,0.75,0.97)){
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot.new()
  title(main='Cubic B-splines d=3')
  j=0
  for (cent in cents){
    j=j+1
    fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=3),tau=cent)
    par(new=TRUE)
    plot(fit, res=FALSE, lty=lty[j],lwd=2,col=colors[j],xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",ylim=c(400,6000))
  }
  points(data,col="black",pch=1,cex=0.5)
  legend("bottomright", legend=c("Abnormal data"),
         col=c("black"), pch=1, cex=0.4)
  plot.new()
  title(main='Cubic B-splines d=2')
  j=0
  for (cent in cents){
    j=j+1
    fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=2),tau=cent)
    par(new=TRUE)
    plot(fit, res=FALSE, lty=lty[j],lwd=2,col=colors[j],xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",ylim=c(400,6000))
  }
  points(data,col="black",pch=1,cex=0.5)
  legend("bottomright", legend=c("Abnormal data"),
         col=c("black"), pch=1, cex=0.4)
  plot.new()
  title(main='Cubic B-splines d=1')
  j=0
  for (cent in cents){
    j=j+1
    fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=1),tau=cent)
    par(new=TRUE)
    plot(fit, res=FALSE, lty=lty[j],lwd=2,col=colors[j],xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",ylim=c(400,6000))
  }
  points(data,col="black",pch=1,cex=0.5)
  legend("bottomright", legend=c("Abnormal data"),
         col=c("black"), pch=1, cex=0.4)
}
#' Centile curves according to different methods
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the method chosen to calculate bandwdth. Could be cross validation or Plug-in. Default is set to CV.
#' @param lambdas to be set for "B-Splines". Can be a vector or a single numeric value. Tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param data the abnormal/external data we want to compare the curves with
#' @return Plots centile curves with the different methods and displays them on the same figure as the noise data to compare
#' @export
#'

compareCurv<-function(x,y,bandwidth.method="CV",lambdas,data){
  message('Make sure to load locpol and quantregGrowth using library()')
  plot.new()
  par(mfrow=c(3,2))
  #1
  localCst0(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data)
  #2
  localLin0(x,y,bandwidth.method,cents=c(0.03,0.25,0.5,0.75,0.97),data)
  #3
  cubicSpline0(x,y,cents=c(0.03,0.25,0.5,0.75,0.97),data)
  #4
  BSplines0(x,y,lambdas,data,cents=c(0.03,0.25,0.5,0.75,0.97))
}

