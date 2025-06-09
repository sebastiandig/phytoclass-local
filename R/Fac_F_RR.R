#' Part of the steepest descent algorithm and work to reduce error given 
#' the S and F matrices.
#' 
#' @keywords internal 
#'
#' @param Fmat XX
#' @param vary Are the index of the non-zero integers to vary
#' @param S XX
#' @param cm XX
#'
#' @return
#'
#' @examples
# Inputs are F and which elements to vary, should be all elements
Fac_F_RR <- function(Fmat, vary, S, cm, fac_rr = c(1, 2, 3), place = NULL) {
  
  # which min and max scale values to use
  rand_var <-
    switch(
      fac_rr,
      list(min.scaler = 0.99, max.scaler = 1.01),
      list(min.scaler = 0.98, max.scaler = 1.02),
      list(min.scaler = 0.97, max.scaler = 1.03)
    )

  # randomises every element in 'vary' then retest RMSE to prev RMSE
  F.new <- lapply(
    vary, 
    function(i) {
      Replace_Rand(
        Fmat, i, S, cm,
        min.scaler = rand_var$min.scaler,
        max.scaler = rand_var$max.scaler
      )
    }
  )

  # if RMSE is better, will extract all the elements that reduce RMSE
  # list of TRUE/FALSE for each varied pigment
  conts <- vapply(F.new, function(i) {i[[4]]}, logical(1)) 
  conts <- which(conts) # index which pigments decreases error

  # Procedure for if no elements reduce error

  # when at least one element reduces error
  if (length(conts) > 0) {
    F.news <- vector(length = length(conts)) # initialize new ratios
    for (i in 1:length(conts)) {
      pig_ind     <- vary[conts][i]
      F_ind       <- conts[i]
      F.news[[i]] <- F.new[[F_ind]][[1]][pig_ind]
    }

    F.new <- replace(Fmat[[1]], vary[conts], F.news) # replace new ratios
    F.new <- NNLS_MF(F.new, S, cm)
    
  } else {
    # when no elements reduce the error
    if (fac_rr == 1) {
      F.new <- NNLS_MF(Fmat[[1]], S, cm)
      conts <- vary
    } else {
      C <- Fac_F_RR(Fmat, place, S, cm, fac_rr = fac_rr - 1)
      F.new <- C[[1]]
      conts <- C[[2]]
    }
  }
  
  res <- list(F.new, conts)
  return(res)
}


# ============================================================================ #


# Fac_F_RR1 <- function(Fmat, vary, S, cm) {
#   # ----
#   F.locs <- vector()
# 
#   F.new <- lapply(vary, function(i) { # randomises every element in 'vary'
#     Replace_Rand(Fmat, i, S, cm, min.scaler = 0.99, max.scaler = 1.01)
#   })
# 
#   # Shows which elements reduce error
#   cont <- lapply(1:length(F.new), function(i) {
#     c <- which(length(F.new[[i]]) == 4)
#   })
#   
#   conts <- which(cont == 1)
# 
#   # ----
# 
#   # Procedure for if no elements reduce error
#   if (!is.null(length(conts))) {
#             # cont  <- as.list(vary[conts])
#             # conts <- as.list(conts)
#             # Locs  <- cont
#             # 
#             # F.news <- sapply(conts, function(i) {
#             #   sapply(cont, function(j) {
#             #     F.locs[[length(F.locs) + 1]] <- F.new[[i]][[1]][[j]]
#             #   })
#             # })
#         
#             if (length(F.news) > 0) {
#               
#                               # if (length(F.news) > 1) {
#                               #                             F.news <- diag(F.news)
#                               # } else {
#                               #                             F.news <- F.news[[1]]
#                               # }
#                               
#                               # cont  <- unlist(cont)
#                               # F.new <- replace(Fmat[[1]], cont, F.news)
#                               # F.new <- NNLS_MF(F.new, S, cm)
#               
#             } else {
#                           F.new <- NNLS_MF(Fmat[[1]], S, cm)
#                           cont <- vary
#             }
#             
#             
#   } else {
#             F.new <- NNLS_MF(Fmat[[1]], S, cm)
#             cont <- vary
#   }
# 
# 
#   res <- list(F.new, cont)
#   return(res)
# }
# 
# Fac_F_RR2 <- function(Fmat, vary, place, S, cm) {
#   # ----
#   F.locs <- vector()
#   F.new  <- lapply(vary, function(i) {
#     Replace_Rand(Fmat, i, S, cm, min.scaler = 0.98, max.scaler = 1.02)
#   })
#   
#   cont <- lapply(1:length(F.new), function(i) {
#     c  <- which(length(F.new[[i]]) == 4)
#   })
#   
#   conts <- which(cont == 1)
#   
#   
#   # ----
#   if (!is.null(length(conts))) {
#     
#             # cont   <- as.list(vary[conts])
#             # conts  <- as.list(conts)
#             # Locs   <- cont
#             # F.news <- sapply(conts, function(i) {
#             #   sapply(cont, function(j) {
#             #     F.locs[[length(F.locs) + 1]] <- F.new[[i]][[1]][[j]]
#             #   })
#             # })
#             
#             if (length(F.news) > 0) {
#               
#                                     # if (length(F.news) > 1) {
#                                     #                           F.news <- diag(F.news)
#                                     #   
#                                     # } else {
#                                     #                           F.news <- F.news[[1]]
#                                     # }
#                                     
#                                     # cont  <- unlist(cont)
#                                     # F.new <- replace(Fmat[[1]], cont, F.news)
#                                     # F.new <- NNLS_MF(F.new, S, cm)
#               
#             } else {
#                                     # diff
#                                     C     <- Fac_F_RR1(Fmat, place, S, cm)
#                                     F.new <- C[[1]]
#                                     cont  <- C[[2]]
#             }
#     
#   } else {
#             # diff
#             C     <- Fac_F_RR1(Fmat, place, S, cm)
#             F.new <- C[[1]]
#             cont  <- C[[2]]
#   }
#   
#   res <- list(F.new, cont)
#   return(res)
# }
# 
# Fac_F_RR3 <- function(Fmat, vary, place, S, cm) {
#   # ----
#   F.locs <- vector()
#   F.new  <- lapply(vary, function(i) {
#     Replace_Rand(Fmat, i, S, cm, min.scaler = 0.97, max.scaler = 1.03)
#   })
#   
#   cont <- lapply(1:length(F.new), function(i) {
#     c <- which(length(F.new[[i]]) == 4)
#   })
#   
#   conts <- which(cont == 1)
#   
#   
#   # ----
#   
#   if (!is.null(length(conts))) {
#     # cont   <- as.list(vary[conts])
#     # conts  <- as.list(conts)
#     # Locs   <- cont
#     # F.news <- sapply(conts, function(i) {
#     #   sapply(cont, function(j) {
#     #     F.locs[[length(F.locs) + 1]] <- F.new[[i]][[1]][[j]]
#     #   })
#     # })
#     
#     if (length(F.news) > 0) {
#       
#       # if (length(F.news) > 1) {
#       #                            F.news <- diag(F.news)
#       #   
#       # } else {
#       #                            F.news <- F.news[[1]]
#       # }
#       
#       # cont <- unlist(cont)
#       # F.new <- replace(Fmat[[1]], cont, F.news)
#       # F.new <- NNLS_MF(F.new, S, cm)
#       
#     } else {
#       # diff
#       C     <- Fac_F_RR2(Fmat, vary, place, S, cm)
#       F.new <- C[[1]]
#       cont  <- C[[2]]
#     }
#     
#   } else {
#     # diff
#     C     <- Fac_F_RR1(Fmat, place)
#     F.new <- C[[1]]
#     cont  <- C[[2]]
#   }
#   res <- list(F.new, cont)
#   return(res)
# }