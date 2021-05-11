#' Contrast analyses for dependent samples
#'
#' This function allows to perform contrast analyses for dependent samples.
#' Please note that sample sizes must be equal between within-subject groups,
#' so no missings are allowed
#'
#' @param nGroup Number of dependent / within-subject groups
#' @param lambda A matrix of contrast weights with contrasts in rows and
#' groups in columns
#' @param dat A matrix or dataframe with nGroup columns; each row contains
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
#' presidents <- data.frame(Qtr1=presidents[seq(1, length(presidents), 4)],
#'                Qtr2=presidents[seq(2, length(presidents), 4)],
#'                Qtr3=presidents[seq(3, length(presidents), 4)],
#'                Qtr4=presidents[seq(4, length(presidents), 4)])
#' presidents <- na.omit(presidents)
#' nGroup <- ncol(presidents)
#'
#' # define lambda weights
#' lambda <- matrix(c(
#'     1, 0, 0, -1, # H1: decrease in approval ratings with stagnation over warmer months
#'     3, 1, -1, -3), # H2: linear decrease in  approval ratings
#'     ncol = nGroup,
#'     byrow=TRUE)
#'
#' # perform contrast analysis
#' contrast_dependent(nGroup, lambda,presidents)
#'
#' @export
contrast_dependent <- function(nGroup,
                               lambda,
                               dat,
                               testvalue = 0) {


  # Checks on the input ------------------------------------------------
  names(dat) <- paste0("group", 1:nGroup)
  if (nGroup != ncol(dat) | nGroup != ncol(lambda)) {
    stop("Please check the data format: \n",
         " * each column must contain the dependent variable in the
         within-subject group \n",
         " * nGroup must be the total number of within-subject groups \n",
         " * lambda must contain the contrast weights in rows and group
         indicator in columns")
  }
  if (all(!(rowSums(lambda) - 0) < .Machine$double.eps)) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }
  n <- nrow(dat)

  # Compute L and sigma^2 pooled --------------------------------------------
  lambda <- t(lambda)
  dat <- as.matrix(dat)
  L <- dat %*% lambda
  sigmaPooled <- colSums((L - rep(colMeans(L), each = nrow(L)))^2) / (n - 1)


  # Define contrast estimate ------------------------------------------------
  numerator <- colMeans(L) - testvalue
  denominator <- sqrt(sigmaPooled / n)
  dfContrast <- 1


  # F and t values ----------------------------------------------------------
  tcontrast <- numerator / denominator
  Fcontrast <- tcontrast^2
  pval <- 2 * pt( - abs(tcontrast), (n - 1))


  # Effect sizes ------------------------------------------------------------
  g <- colMeans(L) / sqrt(sigmaPooled)


  # Format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("F" = round(Fcontrast, rounding),
                       "df" = dfContrast,
                       "contrast estimate" = round(numerator, rounding),
                       "t" = round(tcontrast, rounding),
                       "p" = round(pval, rounding),
                       "g" = round(g, rounding))
  row.names(output) <- paste0("Contrast ", 1:ncol(lambda))

  return(output)
}
