#' Select the new F matrix element with lowest error in the steepest
#' descent algorithm. 
#' 
#' Randomizes one non-zero pigment ratio and runs NNLS and detects if NNLS error 
#' is lower than previous. Then, records TRUE/FALSE.
#' 
#' @keywords internal
#'
#' @param Fmat A matrix of pigment ratios (F matrix)
#' @param i    Element number to randomize in Fmat
#' @param S    A matrix of samples (rows) and pigments (colums)
#' @param cm   A vector of weights
#' @param min.scaler  A numeric for minimum scale values
#' @param max.scaler  A numeric for maximum scale values
#'
#' @return A list with results from `NNLS_MF()` and logical if reduces error
#'
#' @examples
Replace_Rand <- function(Fmat, i, S, cm, min.scaler, max.scaler) {

  # randomise non-zero elements
  Fmat_1   <- Fmat[[1]] # extract Fmat as "F matrix" = Fn, "RMSE" = error, "C matrix" = Cn2
  new_rand <- Randomise_elements(Fmat_1[i], min.scaler, max.scaler) # randomize one element
  F_new    <- replace(Fmat_1, i, new_rand)
  F_new    <- as.matrix(F_new)
  F_new    <- NNLS_MF(F_new, S, cm)

  # Which elements decrease the error? Store the location of the elements that decrease it
  # v   <- which(F_new[[2]] < Fmat[[2]]) # true or false
  v   <- F_new[[2]] < Fmat[[2]] # compare RMSE from previous, if better TRUE
  res <- c(F_new, v)

  return(res)
}
