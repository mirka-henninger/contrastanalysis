#' Contrast analyses for independent samples
#'
#' This function allows to directly compare two contrasts for independent samples by
#' standardizing the contrast weights
#'
#' @param n_group Number of independent / between-subject groups
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
#' # load iris dataset
#' data("iris")
#' iris <- iris[,c("Species", "Petal.Length")]
#'
#' # define lambda weights
#' lambda1 <- c(-1, 0, 1)  # H1: Iris versicolor is distinct to Iris setosa and Iris virginica
#' lambda2 <- c(-2, 1, 1)  # H2: Iris versicolor is more similar to Iris virginica
#'
#' # perform contrast analysis
#' compare_independent(n_group=3, lambda1, lambda2, iris)
#'
#' @export
compare_independent <- function(n_group,
                                lambda1,
                                lambda2,
                                dat) {


  # Checks on the input -----------------------------------------------------
  names(dat) <- c("groups", "values")
  if (n_group != length(unique(dat$groups))
      | n_group != length(lambda1)
      | n_group != length(lambda2)) {
    stop("Please check the data format: \n",
         " * the first column must contain the group indicator \n",
         " * the second the dependent variable \n",
         " * n_group must be the total number of between-subject groups \n",
         " * lambda1 and lambda2 must each contain one set of contrast weights")
  }
  if (sum(lambda1) != 0 | sum(lambda2) != 0) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


  # Standardize lambda weight -----------------------------------------------
  lambda1_std <- lambda1 / sqrt(mean(lambda1^2))
  lambda2_std <- lambda2 / sqrt(mean(lambda2^2))
  lambda_diff <- t(as.matrix(lambda1_std - lambda2_std))

  results <- contrast_independent(n_group = n_group,
                                  lambda = lambda_diff,
                                  dat = dat)

  weights <- data.frame(
    lambda1_std = lambda1_std,
    lambda2_std = lambda2_std,
    lambda_diff = as.vector(lambda_diff)
  )

  output <- list(results = results,
                 contrast_weights = weights)
  return(output)
}

