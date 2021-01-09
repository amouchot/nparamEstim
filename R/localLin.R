#' Local linear estimator
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the bandwidth method choice: CV or plug-in. Default is CV.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots the centile curves and returns a list object containing bandwidth value and estimated centiles values. Kernel used is gaussian.
#' @import graphics
#' @import locpol
#' @export
#'
#' @examples
#' #create a data frame
#' example<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' #calculate the centiles and plot the curves
#' localLin(x,y)
#'
localLin<-function(x,y,bandwidth.method="CV",cents=c(0.03,0.25,0.5,0.75,0.97)){
  x_seq=seq(min(x),max(x),0.01)
  bandwidth<-window(x,y,bandwidth.method)
  lcw<-locpol::locLinWeightsC(x,x_seq,bandwidth,gaussK)$locWeig
  m= lcw %*% y
  v= lcw %*% (y*y)-m^2
  s=sqrt(v)
  centiles<- data.frame(sapply(cents, function(x) m+s*qnorm(x)))
  results<-list(bandwidth=bandwidth,centiles=centiles)
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)")
  title(main='Local linear polynomial')
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  for (value in centiles){
    j=j+1
    lines(x_seq,value,type="l",lty=lty[j],lwd=2,col=colors[j])
  }
  return(results)
}
