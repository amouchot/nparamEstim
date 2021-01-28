#Kernel functions
trig.kernel=function(x){
  ifelse(x >= -1 & x <= 1,(1-abs(x)),0)
}

#gaussian kernel
gaussian.kernel=function(x){
  1/(sqrt(2*pi))*exp(-(x-0)^2/(2))
}

#cubic kernel
cubic.kernel=function(x){
  ifelse(x >= -1 & x <= 1,(35/32)*(1-x^2)^3,0)}

#circular kernel
circular.kernel=function(x){
  ifelse(x >= -1 & x <= 1,(pi/4)*cos((pi/2)*x),0)}

#epan
epan.kernel=function(x){
  3/4*(1-x^2)*(abs(x)<=1)
}

#' Kernel selection
#'
#' @param kernel a string - the kernel chosen. Can be trig, gauss, circ, cubic or epan.
#'
#' @return the function of the Kernel selected
#' @export
#'
#' @examples
#' kernel.select("cubic")
kernel.select<-function(kernel){
  if(kernel=="trig"){result<-trig.kernel}
  else{
    if(kernel=="gauss"){result<-gaussian.kernel}
    else{
      if(kernel=="cubic"){result<-cubic.kernel}
      else{if(kernel=="circ"){result<-circular.kernel}
        else{ if(kernel=="epan"){result<-epan.kernel}
          else{message("The Kernel selected is not supported by the function")}}}}}
  return(result)
}
