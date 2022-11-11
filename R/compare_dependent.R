#' Contrast analyses for dependent samples
#'
#' This function allows to directly compare two contrasts for dependent samples
#' by standardizing the contrast weights.
#' Please note that sample sizes must be equal between within-subject groups,
#' so no missings are allowed
#'
#' @param n_group Number of dependent / within-subject groups
#' @param lambda_preferred A vector of contrast weights for Hypothesis 1
#' @param lambda_competing A vector of contrast weights for Hypothesis 2
#' @param data A matrix or dataframe with n_group columns; each row contains values
#' for one respondent;
#' each column contains values of the dependent variable in the respective
#' within-subject group
#' @param testvalue value under the Null hypothesis; if not specified, it is
#' fixed to 0.
#'
#' @return A list with following entries:
#' \describe{
#'   \item{\code{Results}}{Results of the contrast analysis}
#'   \item{\code{Contrast Weights}}{Standardized contrast weights of the
#'   two original contrasts and difference between standardized contrast weights}
#' }
#'
#' @note A test favoring Hypothesis 1 is performed, hence a positive t-value
#' indicate that the contrast weights contained in lambda_preferred fit the data better
#' than the contrast weights contained in lambda_competing, and vice versa for a negative
#' t-value.
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # load presidents dataset
#' presidents <- data.frame(Qtr1=presidents[seq(1, length(presidents), 4)],
#'                Qtr2=presidents[seq(2, length(presidents), 4)],
#'                Qtr3=presidents[seq(3, length(presidents), 4)],
#'                Qtr4=presidents[seq(4, length(presidents), 4)])
#' presidents <- na.omit(presidents)
#'
#' # define lambda weights
#' lambda_preferred <- c(1, 0, 0, -1)   # H1: decrease in ratings with stagnation over warmer months
#' lambda_competing <- c(3, 1, -1, -3)  # H2: linear decrease in approval ratings
#'
#' # perform contrast analysis
#' compare_dependent(n_group=4, lambda_preferred, lambda_competing, presidents)
#'
#' @export
compare_dependent <- function(n_group,
                              lambda_preferred,
                              lambda_competing,
                              data,
                              testvalue = 0) {


  # Checks on the input -----------------------------------------------------
  names(data) <- paste0("group", 1:n_group)
  if (n_group != ncol(data)
      | n_group != length(lambda_preferred)
      | n_group != length(lambda_competing)) {
    stop("Please check the data format: \n",
         " * each column must contain the dependent variable in the within-subject group \n",
         " * n_group must be the total number of within-subject groups \n",
         " * lambda_preferred and lambda_competing must each contain one set of contrast weights")
  }
  if (sum(lambda_preferred) != 0 | sum(lambda_competing) != 0) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


  # Standardize lambda weight -----------------------------------------------
  lambda_preferred_std <- lambda_preferred / sqrt(mean(lambda_preferred^2))
  lambda_competing_std <- lambda_competing / sqrt(mean(lambda_competing^2))
  lambda_diff <- t(as.matrix(lambda_preferred_std - lambda_competing_std))

  results <- contrast_dependent(n_group = n_group,
                                lambda = lambda_diff,
                                data = data,
                                testvalue = testvalue)

  weights <- data.frame(
    lambda_preferred_std = lambda_preferred_std,
    lambda_competing_std = lambda_competing_std,
    lambda_diff = as.vector(lambda_diff)
  )

  output <- list(results = results,
                 contrast_weights = weights)
  return(output)
}

