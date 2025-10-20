################################################################################
# The code in this script is a wrapper for code originally in the MRTAnalysis
# R package
################################################################################

convert_to_emee_fit_object <- function(current_fit, # output of the function emee_categorical_trt_with_Delta, e.g., current_fit = fit1 if fit1 is the name of the output of emee_categorical_trt_with_Delta
                                       EMEE_final, # name of dataset, e.g., EMEE_final = data_for_analysis
                                       id_string){ # variable name for participant ID's in a string format and must be a column within EMEE_final, e.g., id_string = "participant_id"
  output <- list(current_fit)
  names(output) <- c("fit")
  output$call <- match.call()
  output$df <- length(unique(EMEE_final[[id_string]])) - output$fit$dims$p - output$fit$dims$q
  ord <- c("call", "fit", "df")
  output <- output[ord]
  class(output) <- "emee_fit"
  return(output)
}

summary.emee_fit <- function(
    object,
    lincomb = NULL,
    conf_level = 0.95,
    show_control_fit = FALSE,
    ...) {
  p <- length(object$fit$beta_hat)
  q <- length(object$fit$alpha_hat)
  
  if (!is.null(lincomb)) {
    stopifnot(is.numeric(lincomb))
    if (is.vector(lincomb)) {
      stopifnot(length(lincomb) == p)
      lincomb <- matrix(lincomb, nrow = 1)
    } else if (is.matrix(lincomb)) {
      stopifnot(ncol(lincomb) == p)
    } else {
      stop(paste0(
        "lincomb needs to be a vector of length p or a matrix with p columns. ",
        "(p is the number of moderators including intercept.)"
      ))
    }
  }
  
  # output table for beta
  est <- object$fit$beta_hat
  se <- object$fit$beta_se_adjusted
  
  if (!is.null(lincomb)) {
    est_lincomb <- as.vector(lincomb %*% est)
    varcov_beta <- object$fit$varcov_adjusted[(q + 1):(q + p), (q + 1):(q + p)]
    se_lincomb <- sqrt(diag(lincomb %*% varcov_beta %*% t(lincomb)))
    
    est <- c(est, est_lincomb)
    se <- c(se, se_lincomb)
  }
  
  crit <- qt(1 - (1 - conf_level) / 2, df = object$df)
  lcl <- est - se * crit
  ucl <- est + se * crit
  t_value <- est / se
  p_value <- 2 * pt(-abs(t_value), df = object$df)
  
  out_beta <- cbind(est, lcl, ucl, se, t_value, object$df, p_value)
  colnames(out_beta) <- c(
    "Estimate",
    paste0(round(conf_level * 100), "% ", c("LCL", "UCL")),
    "StdErr", "t_value", "df", "p-value"
  )
  
  rowname_out_beta <- names(object$fit$beta_hat)
  
  # construct variable names for linear combination terms
  if (!is.null(lincomb)) {
    for (irow in 1:nrow(lincomb)) {
      this_name <- ""
      for (icol in 1:ncol(lincomb)) {
        lincomb_entry <- lincomb[irow, icol]
        if (lincomb_entry == 1) {
          if (this_name != "") {
            this_name <- paste0(this_name, " + ", rowname_out_beta[icol])
          } else {
            this_name <- paste0(rowname_out_beta[icol])
          }
        } else if (lincomb_entry > 0) {
          if (this_name != "") {
            this_name <- paste0(
              this_name, " + ", lincomb_entry, "*",
              rowname_out_beta[icol]
            )
          } else {
            this_name <- paste0(lincomb_entry, "*", rowname_out_beta[icol])
          }
        } else if (lincomb_entry == -1) {
          if (this_name != "") {
            this_name <- paste0(this_name, " - ", rowname_out_beta[icol])
          } else {
            this_name <- paste0("-", rowname_out_beta[icol])
          }
        } else if (lincomb_entry < 0) {
          if (this_name != "") {
            this_name <- paste0(
              this_name, " - ", abs(lincomb_entry), "*",
              rowname_out_beta[icol]
            )
          } else {
            this_name <- paste0(
              "- ", abs(lincomb_entry), "*",
              rowname_out_beta[icol]
            )
          }
        }
      }
      rowname_out_beta <- c(rowname_out_beta, this_name)
    }
  }
  rownames(out_beta) <- rowname_out_beta
  
  res <- list(
    call = object$call,
    causal_excursion_effect = out_beta
  )
  
  if (show_control_fit) {
    # output table for beta
    est <- object$fit$alpha_hat
    se <- object$fit$alpha_se_adjusted
    crit <- qt(1 - (1 - conf_level) / 2, df = object$df)
    lcl <- est - se * crit
    ucl <- est + se * crit
    t_value <- est / se
    p_value <- 2 * pt(-abs(t_value), df = object$df)
    
    out_alpha <- cbind(est, lcl, ucl, se, t_value, object$df, p_value)
    colnames(out_alpha) <- c(
      "Estimate",
      paste0(round(conf_level * 100), "% ", c("LCL", "UCL")),
      "StdErr", "t_value", "df", "p-value"
    )
    
    rownames(out_alpha) <- names(object$fit$alpha_hat)
    
    res <- c(res, list(control_variables = out_alpha))
    message("Interpreting the fitted coefficients for control variables is not recommended.")
  }
  
  res
}

