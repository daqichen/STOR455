# define a function to plot CI and PI bounds on a scatterplot 
# for a simple linear model
# written by R. Lock, modified by J. Chapman and I. Ramler
# Arguments:
#   x, y: x- and y-variables
#   level: confidence level (as a proportion) - defaults to 0.95
#   xname,yname - names (labels) for plots

CIPIPlot = function(x, y, level=0.95, xname = "x", yname="y"){
  #fit the model
  mymodel = lm(y ~ x)
  
  #get values needed for PI and CI formulas
  n = length(x)
  tstar = qt( 1 - (1-level)/2, n-2)
  xbar = mean(x)
  ssx = sum( (x-xbar)^2 )
  b0 = mymodel$coeff[1]    #intercept
  b1 = mymodel$coeff[2]    #slope
  Se = summary(mymodel)$sigma       #standard dev of error
  
  # determine appropriate bounds for plot so entire bands show
  # the most extreme points will come from the prediction intervals,
  # since those are wider
  y1 = b0+b1*min(x) - tstar * Se * sqrt( 1 + 1/n + (min(x)-xbar)^2/ssx )
  y2 = b0+b1*max(x) + tstar * Se * sqrt( 1 + 1/n + (max(x)-xbar)^2/ssx )
  
  
  #scatterplot with line
  plot(y ~ x,main = "Confidence and Prediction Intervals", pch=16, ylim = sort(c(y1,y2)),
       xlab = xname, ylab = yname)
  abline(mymodel)
  
  
  #add curves for the CI bounds
  curve(b0+b1*x+tstar*Se*sqrt(1/n+(x-xbar)^2/ssx),add=T,lty=2, col="red",lwd=1)
  curve(b0+b1*x-tstar*Se*sqrt(1/n+(x-xbar)^2/ssx),add=T,lty=2, col="red",lwd=1)
  #add curves for the PI bounds
  curve(b0+b1*x+tstar*Se*sqrt(1+1/n+(x-xbar)^2/ssx),add=T,lty=2, col="blue",lwd=1)
  curve(b0+b1*x-tstar*Se*sqrt(1+1/n+(x-xbar)^2/ssx),add=T,lty=2, col="blue",lwd=1)
}




