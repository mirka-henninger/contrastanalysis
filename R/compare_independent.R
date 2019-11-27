#' Contrast analyses for independent samples
#'
#' This function allows to directly compare two contrasts for independent samples by
#' standardizing the contrast weights
#'
#' @param nGroup Number of independent / between-subject groups
#' @param lambda1 A vector of contrast weights for Hypothesis 1
#' @param lambda2 A vector of contrast weights for Hypothesis 2
#' @param dat A matrix or dataframe with two columns; each row contains values for
#'  one respondents; the first column contains the group indicator, the second column
#'  contains the dependent variable
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
#' lambda1 <- c(-1,-1,1,1)
#' lambda2 <- c(-3,-1,1,3)
#' dat <- data.frame(
#'   x = rep(c(1:4),each = 50),
#'   y = c(rnorm(50,-1,1),rnorm(50),rnorm(50),rnorm(50,1,1))
#' )
#' compare_independent(nGroup, lambda1, lambda2, dat)
#'
#' @export
compare_independent <- function(nGroup,
                                lambda1,
                                lambda2,
                                dat) {


  # Checks on the input -----------------------------------------------------
  names(dat) <- c("groups", "values")
  if (nGroup != length(unique(dat$groups))
      | nGroup != length(lambda1)
      | nGroup != length(lambda2)) {
    stop("Please check the data format: \n",
         " * the first column must contain the group indicator \n",
         " * the second the dependent variable \n",
         " * nGroup must be the total number of between-subject groups \n",
         " * lambda1 and lambda2 must each contain one set of contrast weights")
  }
  if (sum(lambda1) != 0 | sum(lambda2) != 0) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


  # Standardize lambda weight -----------------------------------------------
  lambda1Std <- lambda1 / sqrt(mean(lambda1^2))
  lambda2Std <- lambda2 / sqrt(mean(lambda2^2))
  lambdaDiff <- t(as.matrix(lambda1Std - lambda2Std))

  results <- contrast_independent(nGroup = nGroup,
                                  lambda = lambdaDiff,
                                  dat = dat)

  weights <- data.frame(
    lambda1Std = lambda1Std,
    lambda2Std = lambda2Std,
    lambdaDiff = as.vector(lambdaDiff)
  )

  output <- list(results = results,
                 contrastWeights = weights)
  return(output)
}

