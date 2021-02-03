#Cubic B-splines quantile regression

#' Cubic B-splines with first order Laplacian differentiation
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param lambdas tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param cents  numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @import graphics
#' @import quantregGrowth
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

#' bsplines1(x,y,lambdas=seq(1,100))
#'
#'
bsplines1<-function(x,y,lambdas,cents=c(0.03,0.25,0.5,0.75,0.97)){
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  for (cent in cents){
    j=j+1
    fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=1),tau=cent)
    plot(fit, res=TRUE, lty=lty[j],lwd=2,col=colors[j])
    title(main='Cubic B-Splines d=1')
    par(new=TRUE)
  }
}

#' Cubic B-Splines with second order Laplacian diffentiation
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param lambdas tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots the curves at centiles selected
#' @import graphics
#' @import quantregGrowth
#' @export
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

#' bsplines2(x,y,lambda=70)
#'
#'
bsplines2<-function(x,y,lambdas,cents=c(0.03,0.25,0.5,0.75,0.97)){
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  for (cent in cents){
    j=j+1
    fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=2),tau=cent)
    plot(fit, res=TRUE, lty=lty[j],lwd=2,col=colors[j])
    title(main='Cubic B-Splines d=2')
    par(new=TRUE)
  }
}

#' B-Splines with third order Laplacian diffentiation
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param lambdas tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots the curves at centiles selected
#' @import graphics
#' @import quantregGrowth
#' @export
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

#' bsplines3(x,y,lambdas=seq(1,100))
#'
#'
bsplines3<-function(x,y,lambdas,cents=c(0.03,0.25,0.5,0.75,0.97)){
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  leg = c()
  for (cent in cents){
    j=j+1
    fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=3),tau=cent)
    plot(fit, res=TRUE, lty=lty[j],lwd=2,col=colors[j])
    title(main='Cubic B-Splines d=3')
    par(new=TRUE)
    leg = c(leg, toString(cent))
  }
  legend("bottomright", legend = leg, col = colors, lty = lty)
}


#' Cubic B-Splines with all three order Laplacian diffentiation
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param lambdas tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots all the curves at centiles selected using fonctions splines1, splines2 and splines3 from the same package
#' @import graphics
#' @import quantregGrowth
#' @export
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
#' bsplines(x,y,lambdas=seq(1,50))
#'
#'
bsplines<-function(x,y,lambdas,cents=c(0.03,0.25,0.5,0.75,0.97)){
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot.new()
  text(0.5,0.5, "This method does not support the dataset")
  title(main='Cubic B-Splines d=3')
  j=0

  for (cent in cents){
    j=j+1
    fit<-try(quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=3),tau=cent), silent = TRUE)#, outFile = "This method does not support the dataset")
    par(new=TRUE)
    try(plot(fit, res=TRUE, lty=lty[j],lwd=2,col=colors[j]),silent = TRUE)# outFile = "This method does not support the dataset")
  }
  # plot.new()
  # title(main='Cubic B-splines d=2')
  # j=0
  # for (cent in cents){
  #   j=j+1
  #   fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=2),tau=cent)
  #   par(new=TRUE)
  #   plot(fit, res=TRUE, lty=lty[j],lwd=2,col=colors[j],xlab,ylab)
  # }
  # plot.new()
  # title(main='Cubic B-splines d=1')
  # j=0
  # for (cent in cents){
  #   j=j+1
  #   fit<-quantregGrowth::gcrq(y~ps(x, lambda=lambdas, d=1),tau=cent)
  #   par(new=TRUE)
  #   plot(fit, res=TRUE, lty=lty[j],lwd=2,col=colors[j],xlab,ylab)
  # }
}
