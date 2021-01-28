#'Local constant estimator
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
#' #create an example data frame
#' example<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' #calculate the centile and plot the curves
#' localCst(x,y)
#'
localCst<-function(x,y,bandwidth.method="CV", kernel = "gauss", cents=c(0.03,0.25,0.5,0.75,0.97), disp_window = TRUE){
  x_seq=seq(min(x),max(x),0.01)
  bandwidth<-window(x,y,bandwidth.method)
  kern = gaussK
  if(kernel == "epan"){kern = EpaK}
  if(kernel == "trig"){kern = TrianK}
  lcw<-locpol::locCteWeightsC(x,x_seq,bandwidth,kern)$locWeig
  m= lcw %*% y
  v= lcw %*% (y*y)-m^2
  s=sqrt(v)
  centiles<- data.frame(sapply(cents, function(x) m+s*qnorm(x)))
  results<-list(bandwidth=bandwidth,centiles=centiles)
  plot(x,y,lwd=1)
  title(main='Local constant polynomial')
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  j=0
  for (value in centiles){
    j=j+1
    lines(x_seq,value,type="l",lty=lty[j],lwd=2,col=colors[j])
  }
  leg = c()
  for (i in cents){leg = c(leg, toString(i))}
  if(disp_window){
    usr = par('usr')
    Y = usr[3] + 0.05*(usr[4]-usr[3])
    m = (usr[1] + usr[2])/2
    X1 = m - bandwidth/2
    X2 = m + bandwidth/2
    segments(X1, Y, X2, Y, lwd = 3)
    Y1 = Y - 0.5*(Y-usr[3])
    Y2 = Y + 0.5*(Y-usr[3])
    segments(X1, Y1, X1, Y2, lwd =3)
    segments(X2, Y1, X2, Y2, lwd =3)
  }
  legend('bottomright',
         legend = c(paste("window size : ", toString(round(bandwidth, 3))),
                    leg),
         col = c("black", colors),
         lty = c(1,lty)
         )
  return(results)
}
