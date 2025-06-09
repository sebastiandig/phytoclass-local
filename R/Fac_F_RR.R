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
Fac_F_RR <- function(Fmat, vary, S, cm, fac_rr = c(1,2,3), place = NULL) {
  F.locs <- vector()

  rand_var <-
    switch(
      fac_rr,
      list(min.scaler = 0.99, max.scaler = 1.01),
      list(min.scaler = 0.98, max.scaler = 1.02),
      list(min.scaler = 0.97, max.scaler = 1.03)
      )
  
  # randomises every element in 'vary' then retest RMSE to prev RMSE
 
  F.new <- lapply(
    vary, function(i) {
      Replace_Rand(
        Fmat, i, S, cm, 
        min.scaler = rand_var$min.scaler, 
        max.scaler = rand_var$max.scaler
      )
    })
  
  # Shows which elements reduce error
  # cont1 <- lapply(
  #   1:length(F.new), 
  #   function(i) {
  #     c <- which(length(F.new[[i]]) == 4)
  #   })
  
  # if RMSE is better, will extract all the elements that reduce RMSE 
  conts <- sapply(F.new, function(i) {i[4]}) # vector of TRUE/FALSE for each varied pigment
  
  conts <- which(conts == 1) # index which pigments decreases error
  
  # Procedure for if no elements reduce error
# browser()
  # when at least one element reduces error
  # if (!is.null(length(conts))) {
  if (length(conts) > 0) {
    # cont1 <- as.list(vary[conts]) # select the ratios run that decrease error
    # conts <- as.list(conts)       # are the positions in F matrix that were reduced
    
    # loops through each run in conts which are the many matrices in F.new
    # then loops through all the pigments that did reduce in cont1
    # F.news <- sapply(conts, function(i) {
    #   sapply(cont1, function(j) {
    #     F.locs[[length(F.locs) + 1]] <- F.new[[i]][[1]][[j]]
    #   })
    # })
    F.news <- vector(length = length(conts))
    for (i in 1:length(conts)) {
      pig_ind     <- vary[conts][i]
      F_ind       <- conts[i]
      F.news[[i]] <- F.new[[F_ind]][[1]][pig_ind]
    }
    # if (length(F.news) > 0) {
      # if (length(F.news) > 1) {
      #   F.news <- diag(F.news)
      # } else {
      #   F.news <- F.news[[1]]
      # }
      # cont1 <- unlist(cont1)
      # F.new <- replace(Fmat[[1]], cont1, F.news)
  
    
    F.new <- replace(Fmat[[1]], vary[conts], F.news)
    F.new <- NNLS_MF(F.new, S, cm)
      
    # } else {
    # 
    #    if (fac_rr == 1) {
    #     
    #     F.new <- NNLS_MF(Fmat[[1]], S, cm)
    #     cont1 <- vary
    #     
    #    } else {
    #      C      <- Fac_F_RR(Fmat, place, S, cm, fac_rr = fac_rr - 1)  # ------------
    #      F.new  <- C[[1]]
    #      cont1  <- C[[2]]
    #    }
    #   
    # }
  } else {
    
    # when no elements reduce the error
    if (fac_rr == 1) {
      F.new <- NNLS_MF(Fmat[[1]], S, cm)
      # cont1  <- vary
      conts  <- vary
    } else {
      C     <- Fac_F_RR(Fmat, place, S, cm, fac_rr = 1) # -----------------
      F.new <- C[[1]]
      # cont1  <- C[[2]]
      conts  <- C[[2]]
    } 
    
  }
  
  
  # res <- list(F.new, cont1)
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