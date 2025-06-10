#' Performs the non-negative matrix factorisation for given phytoplankton 
#' pigments and pigment ratios, to attain an estimate of phytoplankton 
#' class abundances.
#'
#' @param Fn  Pigment to Chl a matrix
#' @param S   Sample data matrix – a matrix of pigment samples
#' @param cm  Weights for each column
#'
#' @return A list containing 
#' \enumerate{
#'  \item The F matrix (pigment: Chl *a*) ratios
#'  \item The root mean square error (RMSE)
#'  \item The C matrix (class abundances for each group)
#'  }
#' @export
#'
#' @examples
#' MC <- Matrix_checks(Sm,Fm)
#' Snew <- MC$Snew
#' Fnew <- MC$Fnew
#' cm <- Bounded_weights(Snew, weight.upper.bound = 30)
#' NNLS_MF(Fnew, Snew, cm)
#'
NNLS_MF <- function(Fn, S, cm = NULL) {
  if (is.null(cm)) {
    cm <- as.vector(rep(1, ncol(S)))
  }

  Fn_wt_err <- t(Weight_error(Fn, cm))
  S_wt_err  <- t(Weight_error(S, cm))
  
  b       <- crossprod(Fn_wt_err, S_wt_err)
  Fn_prod <- crossprod(Fn_wt_err)
  C_new2  <- 
    RcppML::nnls(
      Fn_prod,
      b,
      cd_maxit = 1000, 
      cd_tol = 1e-8
  )
  C_new2 <- t(C_new2)
  Cn2    <- Normalise_S(C_new2) # Row sums to one
  
  colnames(Cn2) <- rownames(Fn)
  error         <- sqrt(mean((S - C_new2 %*% Fn)^2)) # RMSE
  
  return(list("F matrix" = Fn, "RMSE" = error, "C matrix" = Cn2))
}
