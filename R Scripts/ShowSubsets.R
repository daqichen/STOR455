
#Function to show results for regsubsets
# input should be the result from regsubsets

ShowSubsets=function(regout){
  z=summary(regout)
  q=as.data.frame(z$outmat)
  q$Rsq=round(z$rsq*100,2)
  q$adjRsq=round(z$adjr2*100,2)
  q$Cp=round(z$cp,2)
  return(q)
}
