#' Contrast analyses for dependent samples
#'
#' This function allows to directly compare two contrasts for dependent samples by
#' standardizing the contrast weights.
#' Please note that sample sizes must be equal between within-subject groups,
#' so no missings are allowed
#'
#' @param nGroup Number of dependent / within-subject groups
#' @param lambda1 A vector of contrast weights for Hypothesis 1
#' @param lambda2 A vector of contrast weights for Hypothesis 2
#' @param dat A matrix or dataframe with nGroup columns; each row contains values for
#'  one respondent;
#'  each column contains values of the dependent variable in the respective within-
#'  subject group
#' @param testvalue value under the Null hypothesis; if not specified, it is fixed to 0.
#'
#' @return A list with following entries:
#' \describe{
#'   \item{\code{Results}}{Results of the contrast analysis}
#'   \item{\code{Contrast Weights}}{Standardized contrast weights of the two original
#'    contrasts and difference between standardized contrast weights}
#' }
#'
#' @note A test favoring Hypothesis 1 is performed, hence a positive t-value indicate
#'  that the contrast weights contained in lambda1 fit the data better than the contrast
#'  weights contained in lambda2, and vice versa for a negative t-value.
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' set.seed(1)
#' nGroup <- 4
#' N <- 50
#' lambda1 <- c(3,-1,-1,-1)
#' lambda2 <- c(0,2,-1,-1)
#' dat <- data.frame(
#'   group1 = sample(1:10, size = N, replace = TRUE),
#'   group2 = sample(1:15, size = N, replace = TRUE),
#'   group3 = sample(1:15, size = N, replace = TRUE),
#'   group4 = sample(1:20, size = N, replace = TRUE)
#' )
#' contrast_compare_dependent(nGroup, lambda1, lambda2, dat)
#'
#' @export
contrast_compare_dependent <- function(nGroup,
                                       lambda1,
                                       lambda2,
                                       dat,
                                       testvalue = 0) {


  # Checks on the input -----------------------------------------------------
  names(dat) <- paste0("group", 1:nGroup)
  if (nGroup != ncol(dat)
      | nGroup != length(lambda1)
      | nGroup != length(lambda2)) {
    stop("Please check the data format: each column must contain ",
         "the dependent variable in the within-subject group.",
         "nGroup must be the total number of within-subject groups. ",
         "lambda1 and lambda2 must each contain one set of contrast weights")
  }
  if (sum(lambda1) != 0 | sum(lambda2) != 0) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


  # Standardize lambda weight -----------------------------------------------
  lambda1Std <- lambda1 / sqrt(mean(lambda1^2))
  lambda2Std <- lambda2 / sqrt(mean(lambda2^2))
  lambdaDiff <- t(as.matrix(lambda1Std - lambda2Std))

  results <- contrast_dependent(nGroup = nGroup,
                                lambda = lambdaDiff,
                                dat = dat,
                                testvalue = testvalue)

  weights <- data.frame(
    lambda1Std = lambda1Std,
    lambda2Std = lambda2Std,
    lambdaDiff = as.vector(lambdaDiff)
  )

  output <- list(results = results,
                 contrastWeights = weights)
  return(output)
}

