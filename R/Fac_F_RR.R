#' Part of the steepest descent algorithm and work to reduce error given 
#' the S and F matrices.
#' 
#' @keywords internal 
#'
#' @param Fmat A list containing `F matrix`, `RMSE` and `C matrix`
#' @param vary Are the index of the non-zero integers to vary
#' @param S A matrix of samples (rows) and pigments (columns)
#' @param cm A vector of bounded weights for each pigment
#' @param fac_rr A numeric to call which scaler values to use
#' @param place A vector of all the indexs of non-zero pigment ratios
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
  
  # extract the pigments positions that, when adjusted, gives a better RMSE
  # ===
  # loops through each list of adjusted pigments and determines which adjustment
  # leads to a better RMSE based on the 4th list from `Replace_Rand` 
  cont  <- vapply(F.new, function(i) {i[[4]]}, logical(1))
  # extracts the index which has a better RMSE (i.e. TRUE)
  conts <- which(cont == 1) 
  
  # if the length is null, will either run through NNLS or recursively go to 
  # level one of Fac_F_RR
  # NOTE: this doesn't seem to be possible because when length(NULL) = 0 and is
  #       never null
  if (is.null(length(conts))) {
    if (fac_rr == 1) {
      F.new <- NNLS_MF(Fmat[[1]], S, cm)
      cont  <- vary
    } else {
      # TODO: remove because doesnt have enough inputs to work
      c1    <- Fac_F_RR(Fmat, place, fac_rr = 1)
      F.new <- c1[[1]]
      cont  <- c1[[2]]
    }
  }
  
  # when at least one pigment adjustment has a better RMSE, will replace all
  # pigment ratios in the Fmat with the better one and re-run NNLS
  if (length(conts) > 0) {
    F.news <- vector(length = length(conts)) # initialize new ratios
    for (i in 1:length(conts)) {
      pig_ind     <- vary[conts][i]
      F_ind       <- conts[i]
      F.news[[i]] <- F.new[[F_ind]][[1]][pig_ind]
    }
    F.new <- replace(Fmat[[1]], vary[conts], F.news)
    F.new <- NNLS_MF(F.new, S, cm)
  } else {
    # if no pigments return a better RMSE, depending on the number of iterations
    # in `Steepest_Descent`, will try a smaller range of scaler values
    if (fac_rr == 1) {
      # fac_rr = 1: smallest range, not re-running and extracting output only
      # from NNLS
      F.new <- NNLS_MF(Fmat[[1]], S, cm)
      cont  <- vary
    } else if (fac_rr == 2) {
      # fac_rr = 2: medium range going down to smallest range
      c1    <- Fac_F_RR(Fmat, place, S, cm, fac_rr = 1)
      F.new <- c1[[1]]
      cont  <- c1[[2]]
    } else if (fac_rr == 3) {
      # fac_rr = 3: largest range going down to smaller range
      c1    <- Fac_F_RR(Fmat, vary, place = place, S, cm, fac_rr = 2)
      F.new <- c1[[1]]
      cont  <- c1[[2]]  
    }
  }
  
  res <- list(F.new, cont)
  return(res)
}