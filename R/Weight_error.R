#' Apply weights to F and S matrices
#' 
#' @keywords internal
#'
#' @param S  xx
#' @param cm  xx
#'
#' @return A matrix
#'
#' @examples
Weight_error <- function(S, cm) {
  S <- S %*% diag(cm)
  return(S)
}
