#' Kernel regression with a pre-fixed bandwidth
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the bandwidth method choice: CV or plug-in. Default is CV.
#' @param kernel kernel fucntion to choose: gaussian, trig, circular. See KernelFunctions.R. Default is
#' set to gaussian
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots the centile curves and returns a list object containing bandwidth value, kernel and estimated centiles values.
#'
#' @import graphics
#' @import stats
#' @export
#' @examples
#' #' create a data frame
#' example<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' #calculate the centile and plot the curves
#' KernelReg(x,y)
KernelReg<-function(x,y,bandwidth.method="CV",kernel=gaussian.kernel,cents=c(0.03,0.25,0.5,0.75,0.97)){
  x_seq=seq(min(x),max(x),0.01)
  bandwidth<-window(x,y,method=bandwidth.method)
  centiles<-data.frame()
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",lwd=1)
  j=0
  for(cent in cents){
    cent
    j=j+1
    i=0
    quant=rep(0,length(x_seq))
    for (value in x_seq){
      i=i+1
      W_i=((1/bandwidth)*kernel((x-value)/bandwidth))/sum((1/bandwidth)*kernel((x-value)/bandwidth ))
      fct = function(a) sum(W_i*(abs(y-a)+(2*cent-1)*(y-a)))
      interval=c(0,6000)
      quant[i]=stats::optimize(fct, interval,lower = min(interval), upper = max(interval),
                               maximum = FALSE ,tol = .Machine$double.eps^0.25)$minimum
    }
    lines(x_seq,quant,type="l",lty=lty[j],lwd=3,col=colors[j])
    centiles<-cbind(quant)
  }
  results<-list(bandwidth=bandwidth,kernel=kernel,centiles=centiles)
  return(results)
}
