#' Performs the steepest descent algorithm for a set number of iterations
#' 
#' @keywords internal
#' 
#' @param Fmat xx
#' @param place xx
#' @param S   xx
#' @param cm   xx
#' @param num.loops   xx
#'
#' @return
#'
#' @examples
Steepest_Descent <- function(Fmat, place, S, cm, num.loops) {
  loop <- 1
  F.new <- NNLS_MF(Fmat, S, cm)
  F.initial <- F.new
  for (i in 1:num.loops) { # should always be small. It would be nice to allow the
    F.new <- Minimise_elements_2(F.initial[[1]], place, S, cm, .num = 1, .fac_rr = 1)
    loop <- loop + 1
    loop_2 <- 1
 
    while (F.new[[2]] > F.initial[[2]]) {
      loop_2 <- loop_2 + 1

      # determines which loop and changes minimize based on loop iteration
      # 0 - 5 : 1
      # 6 - 10: 2
      # 11+   : 3
      fac_rr_x <- findInterval(i, c(0, 5, 10), left.open = TRUE)
      F.new <- Minimise_elements_2(
        F.initial[[1]], place, S, cm,
        .num = fac_rr_x,
        .fac_rr = fac_rr_x
      )

      if (loop_2 > 100) {
        # it will continue for 100 iterations, and then stop
        break
      } 
    }
    F.initial <- F.new
  }
  return(F.new)
}
