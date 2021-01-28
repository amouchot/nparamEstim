#'Local Polynomial method for estimating centile curves
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param bandwidth.method the bandwidth method choice: CV or plug-in. Default is CV.
#' @param kernel.choice The kernel choice: Epanechnikov, gaussian, circular... See kernel.select function. Default is set to Epanechnikov.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots the centile curves and returns a list object containing bandwidth value, kernel and estimated centiles values.
#' @import graphics
#' @import stats
#' @export
#'
#' @examples
#' #create a data frame
#' example<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(example)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-example$`Gestational Age in weeks`
#' y<-example$`Weight in gramms`
#' #calculate the centile and plot the curves
#' LocalPol(x,y)

LocalPol<-function(x,y,bandwidth.method="CV",kernel.choice="epan",cents=c(0.03,0.25,0.5,0.75,0.97)){
  x_seq=seq(min(x),max(x),0.01)
  kernel=kernel.select(kernel.choice)
  bandwidth<-window(x,y,bandwidth.method)
  centiles<-data.frame()
  colors<-c("green","blue","red","blue","green")
  lty=c(2,2,3,2,2)
  plot(x,y,xlab = "Gestational Age (weeks)",ylab = "Weight (gramms)",lwd=1)
  title(main='Local Polynomial')
  j=0
  for(cent in cents){
    cent
    j=j+1
    i=0
    quant=rep(0,length(x_seq))
    for (value in x_seq){
      i=i+1
      W_i=((1/bandwidth)*kernel((x-value)/bandwidth))/sum((1/bandwidth)*kernel((x-value)/bandwidth ))
      fct = function(b) {
        b_0=b[1]
        b_1=b[2]
        sum(W_i*(abs(y-b_0-b_1*(x-value))+(2*cent-1)*(y-b_0-b_1*(x-value))))}
      a=optim(c(2000,2000),fct)$par
      quant[i]=a[1]
      centiles<-cbind(quant)}
    lines(x_seq,quant,type="l",lty=lty[j],lwd=2,col=colors[j])
  }
  results<-list(bandwidth=bandwidth,kernel=kernel,centiles=centiles)
  return(results)
}
