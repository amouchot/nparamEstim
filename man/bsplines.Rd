% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bSplines.R
\name{bsplines}
\alias{bsplines}
\title{Cubic B-Splines with all three order Laplacian diffentiation}
\usage{
bsplines(x, y, lambdas, cents = c(0.03, 0.25, 0.5, 0.75, 0.97))
}
\arguments{
\item{x}{the explanatory variable - numeric}

\item{y}{the response variable - numeric}

\item{lambdas}{tunes the tradeoff between the goodness of fit and the regularity of the spline - numeric value or numeric vector}

\item{cents}{A numeric vector that represents the centiles calculated. Default is set to cents=c(0.03,0.25,0.5,0.75,0.97) )}
}
\value{
Plots all the curves at centiles selected using fonctions splines1, splines2 and splines3 from the same package
}
\description{
Cubic B-Splines with all three order Laplacian diffentiation
}
\examples{
#create a sample data frame
weights=c(500,600,1000,1150,1200,1260,1240,1300,1370,1500,2000,2100,2150,2500,
2800,2900,3050,3200,2980,3000,3300,3100,3200,3600,3500,3700,3900,3900,4000,
4200,3000,4500,4300,4900,4350,3700,4000,5000,4300)
age<-c(30,30,30,31,31,31,32,32,32,33,33,33,34,34,34,35,35,35,36,36,36,
37,37,37,38,38,38,39,39,39,40,40,40,41,41,41,42,42,42)
sample<-data.frame(weights,age)
colnames(sample)<-c("Gestational Age in weeks","Weight in gramms")
x<-sample$`Gestational Age in weeks`
y<-sample$`Weight in gramms`
bsplines(x,y,lambdas=seq(1,50))


}
