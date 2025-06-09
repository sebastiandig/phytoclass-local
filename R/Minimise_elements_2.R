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
  # f     <- Conduit_3(Fmat, place, S, cm) # Calls index function
  f     <- Conduit(Fmat, place, S, cm, num = .num) # Calls index function
  F.new <- f[[1]] # F matrix
  n     <- f[[2]] # elements that reduce error
  
  if (is.null(n)) {
    n <- place
  }
  
  F.old     <- f[[3]] # old F matrix
  F.initial <- F.new # Fac_F new matrix
  
  place1 <- NULL
  if (.num != 1) {
    place1 <- place
  }
  
  # Fac_F new matrix
  g <- Fac_F_RR(F.new, vary = place, place = place1, S, cm, fac_rr = .fac_rr) 
  
  # # Fac_F new matrix
  # if (.fac_rr == 1) {
  #   # g <- Fac_F_RR1(F.new, place, S, cm)
  #   # g <- Fac_F_RR1(Fmat = F.new, vary = place, S = S, cm = cm)
  #   g <- Fac_F_RR(F.new, place, S, cm, fac_rr = .fac_rr)
  #   
  # } else if (.fac_rr == 2) {
  #   # g <- Fac_F_RR2(F.new, vary = place, place, S, cm)
  #   # g <- Fac_F_RR2(Fmat = F.new, vary = place, place = place, S = S, cm = cm)
  #   g <- Fac_F_RR(F.new, vary = place, place = place1, S, cm, fac_rr = .fac_rr) 
  #   
  # } else if (.fac_rr == 3) {
  #   # g <- Fac_F_RR3(F.new, vary = place, place, S, cm)
  #   # g <- Fac_F_RR3(Fmat = F.new, vary = place, place = place, S = S, cm = cm)
  #   g <- Fac_F_RR(F.new, vary = place, place = place1, S, cm, fac_rr = .fac_rr)
  #   
  # }
  
  if (g[[1]][[2]] < F.initial[[2]]) {
    F.new <- g[[1]]
  }
  
  n   <- g[[2]]
  res <- list(F.new, n)
  
  return(F.new)
}


# ============================================================================ #


# # for
# # - Conduit_1
# # - Fac_F_RR1
# # A function that reduces every for every element that didn't reduce in index function
# Minimise_elements1 <- function(Fmat, place, S, cm) {
#   # f     <- Conduit_1(Fmat, place, S, cm) # Calls index function
#   f     <- Conduit(Fmat, place, S, cm, num = 1) # Calls index function
#   F.new <- f[[1]] # F matrix
#   n     <- f[[2]] # elements that reduce error
#   
#   if (is.null(n)) {
#     n <- place
#   }
#   
#   F.old     <- f[[3]]    # old F matrix
#   F.initial <- F.new # Fac_F new matrix
#   
#   # Fac_F new matrix
#   # g <- Fac_F_RR1(F.new, place, S, cm)
#   g <- Fac_F_RR(F.new, place, S, cm, fac_rr = 1)
#   if (g[[1]][[2]] < F.initial[[2]]) {
#     F.new <- g[[1]]
#   }
#   
#   n   <- g[[2]]
#   res <- list(F.new, n)
#   
#   return(F.new)
# }
# 
# 
# 
# # for
# # - Conduit_2
# # - Fac_F_RR2
# Minimise_elements2 <- function(Fmat, place, S, cm) { # A function that reduces every for every element that didn't reduce in index function
#   # f     <- Conduit_2(Fmat, place, S, cm) # Calls index function
#   f     <- Conduit(Fmat, place, S, cm, num = 2) # Calls index function
#   F.new <- f[[1]] # F matrix
#   n     <- f[[2]] # elements that reduce error
#   
#   if (is.null(n)) {
#     n <- place
#   }
#   
#   F.old     <- f[[3]] # old F matrix
#   F.initial <- F.new # Fac_F new matrix
#   # Fac_F new matrix
#   # g <- Fac_F_RR2(F.new, vary = place, place, S, cm)
#   g <- Fac_F_RR(F.new, vary = place, place, S, cm, fac_rr = 2) # diff = vary = place
#   
#   if (g[[1]][[2]] < F.initial[[2]]) {
#     F.new <- g[[1]]
#   }
#   
#   n   <- g[[2]]
#   res <- list(F.new, n)
#   
#   return(F.new)
# }
# 
# 
# # for
# # - Conduit_3
# # - Fac_F_RR3
# # A function that reduces every for every element that didn't reduce in index function
# Minimise_elements <- function(Fmat, place, S, cm, num = 3) { 
#   # f     <- Conduit_3(Fmat, place, S, cm) # Calls index function
#   f     <- Conduit(Fmat, place, S, cm, num = 3) # Calls index function
#   F.new <- f[[1]] # F matrix
#   n     <- f[[2]] # elements that reduce error
#   
#   if (is.null(n)) {
#     n <- place
#   }
#   
#   F.old     <- f[[3]] # old F matrix
#   F.initial <- F.new # Fac_F new matrix
#   
#   # Fac_F new matrix
#   # g <- Fac_F_RR3(F.new, vary = place, place, S, cm)
#   g <- Fac_F_RR(F.new, vary = place, place = place, S, cm, fac_rr = 3)
#   
#   if (g[[1]][[2]] < F.initial[[2]]) {
#     F.new <- g[[1]]
#   }
#   
#   n   <- g[[2]]
#   res <- list(F.new, n)
#   
#   return(F.new)
# }