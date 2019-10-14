estimiBMA <- function(x, y, ls.meth = "qr", ridge = 1e-05, ...){
  df <- max(length(y) - ncol(x), 1)
  if (ls.meth == "qr"){
    qr <- my.bicreg(x = x[,-1], y = y, maxCol = 33, nbest = 250,  user.int = F) #take the first row of "1" out, because bicreg automatically calculates the intercept
    c <- t(qr$coef)
    f <- as.vector(qr$fit) #qr$fitted.values
    r <- t(qr$residuals) #t(qr$residuals)
    v <- try(solve(as.matrix(crossprod(qr.R(qr(x))))), silent = TRUE)
    if(inherits(v, "try-error")){
      xtx <- as.matrix(crossprod(qr.R(qr(cbind(1,x)))))
      pen <- diag(xtx) * ridge #calculate ridge penalty
      v <- solve(xtx + diag(pen)) #add ridge penalty to allow inverse of v
      mess <- "* A ridge penalty had to be used to calculate the inverse crossproduct of the predictor matrix. Please remove duplicate variables or unique respondent names/numbers from the imputation model. It may be advisable to check the fraction of missing information (fmi) to evaluate the validity of the imputation model"
      updateLog(out = mess, frame = 6)
      if (get("printFlag", parent.frame(search.parents("printFlag"))))
        cat("*") #indicator of added ridge penalty in the printed iteration history
    }
    return(list(c=t(c), r=t(r), v=v, df=df, ls.meth=ls.meth))
  }
}
