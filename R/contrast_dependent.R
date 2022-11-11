#' Contrast analyses for dependent samples
#'
#' This function allows to perform contrast analyses for dependent samples.
#' Please note that sample sizes must be equal between within-subject groups,
#' so no missings are allowed
#'
#' @param n_group Number of dependent / within-subject groups
#' @param lambda A matrix of contrast weights with contrasts in rows and
#' groups in columns; or a vector for a single contrast with contrast weights
#' @param data A matrix or dataframe with n_group columns; each row contains
#' values for one respondent;
#' each column contains values of the dependent variable in the respective
#' within-subject group
#' @param testvalue Value under the null hypothesis; if not specified, it is
#' fixed to 0.
#'
#' @return a dataframe with following entries for each of the contrasts:
#' \describe{
#'   \item{\code{SumsofSquares}}{Sums of Squares}
#'   \item{\code{df}}{Degrees of freedom for each contrast}
#'   \item{\code{F}}{F-values}
#'   \item{\code{contrast estimate}}{Contrast estimates}
#'   \item{\code{t}}{t-values}
#'   \item{\code{p}}{Two-tailed p-values}
#'   \item{\code{g}}{Effect size g, a standardized distance measure}
#' }
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # load presidents dataset
#' presidents <- data.frame(
#'                Qtr1=presidents[seq(1, length(presidents), 4)],
#'                Qtr2=presidents[seq(2, length(presidents), 4)],
#'                Qtr3=presidents[seq(3, length(presidents), 4)],
#'                Qtr4=presidents[seq(4, length(presidents), 4)])
#' presidents <- na.omit(presidents)
#' n_group <- ncol(presidents)
#'
#' # define lambda weights
#' lambda1 <- c(1, 0, 0, -1) # H1: decrease in approval ratings with stagnation over warmer months
#' lambda2 <- c(3, 1, -1, -3) # H2: linear decrease in  approval ratings
#'
#' # perform contrast analysis
#' contrast_dependent(n_group, lambda1, presidents)
#' contrast_dependent(n_group, lambda2, presidents)
#'
#' @export
contrast_dependent <- function(n_group,
                               lambda,
                               data,
                               testvalue = 0) {


  # Checks on the input ------------------------------------------------
  names(data) <- paste0("group", 1:n_group)
  if(is.matrix(lambda)){
    message("Please note that each contrast is tested separately, the contrast are not tested jointly!\n",
            "* When the contrasts are orthogonal and the samples size is equal in all groups, the results from contrasts tested separately is equal to the results from contrasts\n",
            "tested together.\n",
            "* However, the results may differ when non-orthogonal contrasts are tested separately.\n",
            "* You may want to switch to an alternative package to conduct a joint test of all contrasts")
  }
  if(is.vector(lambda)){
    lambda <- t(as.matrix(lambda))
  }
  if (n_group != ncol(data) | n_group != ncol(lambda)) {
    stop("Please check the data format: \n",
         " * each column must contain the dependent variable in the
         within-subject group \n",
         " * n_group must be the total number of within-subject groups \n",
         " * lambda must contain the contrast weights in rows and group
         indicator in columns")
  }
  if (all(!(abs(rowSums(lambda)) < .Machine$double.eps^0.5))) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }

  # Compute L and sigma^2 pooled --------------------------------------------
  n <- nrow(data)
  df <- n - 1
  lambda <- t(lambda)
  data <- as.matrix(data)
  L <- data %*% lambda
  sigma2 <- apply(L, 2, var) # or alternatively: colSums((L - rep(colMeans(L), each = nrow(L)))^2) / (n-1)


  # Define contrast estimate ------------------------------------------------
  numerator <- colMeans(L) - testvalue
  denominator <- sqrt(sigma2 / n)


  # F and t values ----------------------------------------------------------
  t_contrast <- numerator / denominator
  F_contrast <- t_contrast^2
  p_val <- 2 * pt( - abs(t_contrast), df)


  # Effect sizes ------------------------------------------------------------
  g <- colMeans(L) / sqrt(sigma2)


  # Format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("df" = df,
                       "contrast_estimate" = round(numerator, rounding),
                       "t_value" = round(t_contrast, rounding),
                       "F_value" = round(F_contrast, rounding),
                       "p_value" = round(p_val, rounding),
                       "g" = round(g, rounding))
  row.names(output) <- paste0("Contrast ", 1:ncol(lambda))

  return(output)
}

