#'Regression Splines regression to draw centile curves
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots the centile curves
#' @import graphics
#' @import fields
#' @export
#'
#' @examples
#' #create a data frame
#' example<-data.frame(sample(30:42,100,rep=TRUE),sample(800:5000,100,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' #calculate the centiles and plot the curves
#' RegSpline(x,y)
#'
#'
RegSpline<-function(x,y,cents=c(0.03,0.25,0.5,0.75,0.97)){
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",lwd=1)
  i=0
  for(cent in cents){
    i=i+1
    fit.robust<-fields::qsreg(x,y,alpha =cent)
    lines(fit.robust$predicted$x,fit.robust$predicted$y[,25],type="l",lty=lty[i],lwd=3,
          col=colors[i])
  }
  points(x,y)
}
