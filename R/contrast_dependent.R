#' Contrast analyses for dependent samples
#'
#' This function allows to perform contrast analyses for dependent samples.
#' Please note that sample sizes must be equal between within-subject groups,
#' so no missings are allowed
#'
#' @param nGroup Number of dependent / within-subject groups
#' @param lambda a matrix of contrast weights with contrasts in rows and groups in
#' columns
#' @param dat a matrix or dataframe with nGroup columns; each row contains values for
#' one respondent;
#' each column contains values of the dependent variable in the respective within-
#' subject group
#' @param testvalue value under the null hypothesis; if not specified, it is fixed to 0.
#'
#' @return a dataframe with following entries for each of the contrasts:
#' \describe{
#'   \item{\code{SumsofSquares}}{Sums of Squares}
#'   \item{\code{F}}{F-values}
#'   \item{\code{estimate}}{Contrast estimates}
#'   \item{\code{t}}{t-values}
#'   \item{\code{p}}{two-tailed p-values}
#'   \item{\code{g}}{effect size g, a standardized distance measure}
#' }
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' nGroup <- 4
#' N <- 50
#' lambda <- matrix(c(3,-1,-1,-1,
#'                    0,2,-1,-1,
#'                    0,0,-1,1),
#'                  ncol = nGroup,
#'                  byrow=TRUE)
#' dat <- data.frame(
#'   group1 = sample(1:10, size = N, replace = TRUE),
#'   group2 = sample(1:15, size = N, replace = TRUE),
#'   group3 = sample(1:15, size = N, replace = TRUE),
#'   group4 = sample(1:20, size = N, replace = TRUE)
#' )
#' contrast_dependent(nGroup, lambda,dat)
#'
#' @export
contrast_dependent <- function(nGroup, lambda, dat, testvalue = 0){

  # some checks on the input ------------------------------------------------
  names(dat) <- paste0("group", 1:nGroup)
  if(nGroup != ncol(dat) | nGroup != ncol(lambda)) {
    stop("Please check the data format: each column must contain ",
         "the dependent variable in the within-subject group.",
         "nGroup must be the total number of within-subject groups. ",
         "lambda must contain the contrast weights in rows and the ",
         "group indicator in columns")
  }
  if(all(!dplyr::near(rowSums(lambda), 0))){
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }
  n <- nrow(dat)

  # Compute L and sigma^2 pooled --------------------------------------------
  lambda <- t(lambda)
  dat <- as.matrix(dat)
  L <- dat %*% lambda
  sigmaPooled <- colSums((L-rep(colMeans(L),each = nrow(L)))^2)/(n-1)


  # define contrast estimate ------------------------------------------------
  numerator <- colMeans(L) - testvalue
  denominator <- sqrt(sigmaPooled/n)


  # F and t values ----------------------------------------------------------
  tcontrast <- numerator / denominator
  Fcontrast <- tcontrast^2
  pval <- 2*pt(-abs(tcontrast),(n-1))


  # effect sizes ------------------------------------------------------------
  g <- colMeans(L)/sqrt(sigmaPooled)

  # format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("F" = round(Fcontrast,rounding),
                       "estimate" = round(numerator,rounding),
                       "t" = round(tcontrast,rounding),
                       "p" = round(pval,rounding),
                       "g" = round(g,rounding))
  row.names(output) <- paste0("Contrast ", 1:ncol(lambda))

  return(output)
}
