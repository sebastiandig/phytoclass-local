#' A function that reduces every for every element that didn't reduce in index function
#'
#' @keywords internal
#'
#' @param Fmat   xx
#' @param place  xx
#' @param S    xx
#' @param cm     xx 
#'
#' @return
#'
#' @examples
Minimise_elements_2 <- function(Fmat, place, S, cm, .num = c(1, 2, 3), .fac_rr = c(1, 2, 3)) { 
  f     <- Conduit(Fmat, place, S, cm, num = .num) # Calls index function
  F.new <- f[[1]] # F matrix
  n     <- f[[2]] # elements that reduce error
  
  if (is.null(n)) {
    n <- place
  }
  
  place1 <- NULL
  if (.num != 1) {
    place1 <- place
  }
  
  # Fac_F new matrix
  g <- Fac_F_RR(F.new, vary = place, place = place1, S, cm, fac_rr = .fac_rr) 
  
  if (g[[1]][[2]] < F.new[[2]]) {
    F.new <- g[[1]]
  }
  
  return(F.new)
}
