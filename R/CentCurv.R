#' Centile curves based on different methods
#'
#' @param x the explanatory variable - numeric
#' @param y the response variable - numeric
#' @param window.select the bandwidth method choice: CV or plug-in. Default is CV.
#' @param lambdas set to 0. To be set if method chosen is "B-Splines". Tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector
#' @param method str - The method choosen for displaying the curve. Could be: Local constant, Local linear, Cubic splines or B-splines.
#' @param cents A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )
#'
#' @return Plots centile curves according to the method chosen
#' @import graphics
#' @export
#'
#' @examples
#' #create a sample data frame
#' sample<-data.frame(sample(30:42,50,rep=TRUE),sample(800:5000,50,rep=TRUE))
#' colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
#' x<-sample$`Gestational Age in weeks`
#' y<-sample$`Weight in gramms`
#' CentCurv(x,y,method='Cubic splines')
#' CentCurv(x,y,method='B-Splines',lambdas=1)
#'
#'
CentCurv<-function(x,y,window.select="CV",method,lambdas=0, kernel = "gauss", cents=c(0.03,0.25,0.5,0.75,0.97), disp_window = FALSE){
  if (method=="Local constant"){localCst(x,y,window.select, kernel, cents=cents, disp_window = disp_window)}
  else{if (method=="Local linear"){localLin(x,y, window.select, kernel, cents=cents, disp_window=disp_window)}
    else{if(method=="Cubic splines"){cubicSpline(x,y,cents=cents)}
      else{if(method=="B-Splines"){bsplines3(x,y,lambdas,cents=cents)}
        else{stop("The method selected is not supported by the function")}}}}
}
