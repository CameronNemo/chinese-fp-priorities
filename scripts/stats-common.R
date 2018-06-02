library(lmtest)
library(sandwich)
library(apsrtable)
library(memisc)

robust_summary <- function(obj, alpha = 0.05, ...) {
  ## get original summary
  s <- getSummary(obj, alpha = alpha, ...)
  
  ## replace Wald tests of coefficients
  s$coef[,1:4] <- coeftest(obj, vcov = vcovHC(obj))
  
  ## replace confidence intervals
  crit <- qt(alpha/2, obj$df.residual)
  s$coef[,5] <- s$coef[,1] + crit * s$coef[,2]
  s$coef[,6] <- s$coef[,1] - crit * s$coef[,2]
  
  return(s)
}