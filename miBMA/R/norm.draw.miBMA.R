###'@rdname norm.draw.miBMA
###'@export

sym <- function(x) {(x + t(x)) / 2}

.norm.draw.miBMA <- function (y, ry, x, rank.adjust = TRUE, ...){
  p <- estimiBMA(x[ry, , drop = FALSE], y[ry], ...)
  sigma.star <- sqrt(sum((p$r)^2)/rchisq(1, p$df)) #error
  beta.star <- p$c + (t(chol(sym(p$v))) %*% rnorm(ncol(x))) * sigma.star #he is adding some sort of error to coefficients
  parm <- list(p$c, beta.star, sigma.star, p$ls.meth)
  names(parm) <- c("coef", "beta", "sigma", "estimation")
  if(any(is.na(parm$coef)) & rank.adjust){
    parm$coef[is.na(parm$coef)] <- 0
    parm$beta[is.na(parm$beta)] <- 0
  }
  return(parm)
}
