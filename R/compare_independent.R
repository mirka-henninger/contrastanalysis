#' Contrast analyses for independent samples
#'
#' This function allows to directly compare two contrasts for independent samples by
#' standardizing the contrast weights
#'
#' @param n_group Number of independent / between-subject groups
#' @param lambda_preferred A vector of contrast weights for Hypothesis 1
#' @param lambda_competing A vector of contrast weights for Hypothesis 2
#' @param data A matrix or dataframe with two columns; each row contains values for
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
#' @note A test favoring Hypothesis 1 is performed. This means that when the t-value is positive,
#'  the contrast weights contained in lambda_preferred reflect the data better than the contrast weights
#'  contained in lambda_competing. When the t-value is negative, the contrast weights contained in
#'  lambda_competing reflect the data better, and Hypothesis 2 is favored over Hypothesis 1.
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # load iris dataset
#' data("iris")
#' iris <- iris[,c("Species", "Petal.Length")]
#'
#' # define lambda weights
#' lambda_preferred <- c(-1, 0, 1)  # H1: Iris versicolor is distinct to Iris setosa and Iris virginica
#' lambda_competing <- c(-2, 1, 1)  # H2: Iris versicolor is more similar to Iris virginica
#'
#' # perform contrast analysis
#' compare_independent(n_group=3, lambda_preferred, lambda_competing, iris)
#'
#' @export
compare_independent <- function(n_group,
                                lambda_preferred,
                                lambda_competing,
                                data) {


  # Checks on the input -----------------------------------------------------
  names(data) <- c("groups", "values")
  if (n_group != length(unique(data$groups))
      | n_group != length(lambda_preferred)
      | n_group != length(lambda_competing)) {
    stop("Please check the data format: \n",
         " * the first column must contain the group indicator \n",
         " * the second the dependent variable \n",
         " * n_group must be the total number of between-subject groups \n",
         " * lambda_preferred and lambda_competing must each contain one set of contrast weights")
  }
  if (sum(lambda_preferred) != 0 | sum(lambda_competing) != 0) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


  # Standardize lambda weight -----------------------------------------------
  lambda_preferred_std <- lambda_preferred / sqrt(mean(lambda_preferred^2))
  lambda_competing_std <- lambda_competing / sqrt(mean(lambda_competing^2))
  lambda_diff <- as.vector(t(as.matrix(lambda_preferred_std - lambda_competing_std)))

  results <- contrast_independent(n_group = n_group,
                                  lambda = lambda_diff,
                                  data = data)

  weights <- data.frame(
    lambda_preferred_std = lambda_preferred_std,
    lambda_competing_std = lambda_competing_std,
    lambda_diff = as.vector(lambda_diff)
  )

  output <- list(results = results,
                 contrast_weights = weights)
  return(output)
}

