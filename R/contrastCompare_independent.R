#' Contrast analyses for independent samples
#'
#' This function allows to directly compare two contrasts for independent samples by
#' standardizing the contrast weights
#'
#' @param nGroup Number of independent / between-subject groups
#' @param lambda1 a vector of contrast weights for Hypothesis 1
#' @param lambda2 a vector of contrast weights for Hypothesis 2
#' @param dat a matrix or dataframe with two columns; each row contains values for
#' one respondents; the first column contains the group indicator, the second column
#' contains the dependent variable
#'
#' @return a list with following entries:
#' \describe{
#'   \item{\code{Results}}{Results of the contrast analysis}
#'   \item{\code{Contrast Weights}}{Standardized contrast weights of the two original contrasts
#'   and difference between standardized contrast weights}
#' }
#'
#' @note A test favoring Hypothesis 1 is performed, hence a positive t-value indicate that the
#' contrast weights contained in lambda1 fit the data better than the contrast weights
#' contained in lambda2, and vice versa for a negative t-value.
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' library("dplyr")
#' set.seed(1)
#' nGroup <- 4
#' lambda1 <- c(-1,-1,1,1)
#' lambda2 <- c(-3,-1,1,3)
#' dat <- data.frame(
#'   x = rep(c(1:4),each = 50),
#'   y = c(rnorm(50,-1,1),rnorm(50),rnorm(50),rnorm(50,1,1))
#' )
#' contrastCompare_independent(nGroup, lambda1, lambda2, dat)
#'
#' @export
contrastCompare_independent <- function(nGroup, lambda1, lambda2, dat){

  # checks on the input -----------------------------------------------------
  names(dat) <- c("groups", "values")
  if(nGroup != length(unique(dat$groups))
     | nGroup != length(lambda1)
     | nGroup != length(lambda2)) {
    stop("Please check the data format: the first column must contain ",
         "the group indicator, the second the dependent variable. ",
         "nGroup must be the total number of between-subject groups. ",
         "lambda1 and lambda2 must each contain one set of contrast weights")
  }
  if(sum(lambda1) != 0 | sum(lambda2) != 0){
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


  # standardize lambda weight -----------------------------------------------
  lambda1Std <- lambda1 / sqrt(mean(lambda1^2))
  lambda2Std <- lambda2 / sqrt(mean(lambda2^2))
  lambdaDiff <- lambda1Std - lambda2Std %>% as.matrix() %>% t()

  results <- contrast_independent(nGroup = nGroup,
                                  lambda = lambdaDiff,
                                  dat = dat)

  weights <- data.frame(
    lambda1Std = lambda1Std,
    lambda2Std = lambda2Std,
    lambdaDiff = lambdaDiff %>% as.vector()
  )

  output <- list(results = results,
                 contrastWeights = weights)
  return(output)
}

