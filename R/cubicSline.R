#' Cubic spline quantile regression
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param cents  numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#' @import graphics
#' @import stats
#' @import fields
#' @export
#' @return Plots the curves at centiles selected
#'
#' @examples
#' #create a sample data frame
#' weights=c(500,600,1000,1150,1200,1260,1240,1300,1370,1500,2000,2100,2150,2500,
#' 2800,2900,3050,3200,2980,3000,3300,3100,3200,3600,3500,3700,3900,3900,4000,
#' 4200,3000,4500,4300,4900,4350,3700,4000,5000,4300)
#' age<-c(30,30,30,31,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,36,
#' 37,37,37,38,38,38,39,39,39,40,40,40,41,41,41,42,42,42)
#' sample<-data.frame(weights,age)
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' cubicSpline(x,y)
#'
#'
cubicSpline<-function(x,y,cents=c(0.03,0.25,0.5,0.75,0.97)){
  col=c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot(x,y,main = "Cubic Splines")
  i=0
  leg = c()
  for(cent in cents){
    i=i+1
    fit.robust<-fields::qsreg(x,y,alpha =cent)
    lines(fit.robust$predicted$x,fit.robust$predicted$y[,25],type="l",lty=lty[i],lwd=3,col=col[i])
    leg = c(leg, toString(cent))
  }
  legend("bottomright", legend = leg, col = col, lty = lty)
}
