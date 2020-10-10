#run the code below to acivate a VIF function 
# for multiple regression models

vif<- function(model, ...) {  
  V <- summary(model)$cov.unscaled
  Vi <- crossprod(model.matrix(model))
	nam <- names(coef(model))
  if(k <- match("(Intercept)", nam, nomatch = F)) {
		v1 <- diag(V)[-k]
		v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
		nam <- nam[-k]
	} else {
		v1 <- diag(V)
		v2 <- diag(Vi)
		warning("No intercept term detected.  Results may
surprise.")
	}
	structure(v1*v2, names = nam)
}