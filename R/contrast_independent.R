#' Contrast analyses for independent samples
#'
#' This function allows to perform contrast analyses for independent samples
#'
#' @param nGroup Number of independent / between-subject groups
#' @param lambda A matrix of contrast weights with contrasts in rows and groups in
#'  columns
#' @param dat A matrix or dataframe with two columns; each row contains values for
#'  one respondent;
#'  the first column contains the group indicator, the second column contains the
#'  dependent variable
#'
#' @return A dataframe with following entries for each of the contrasts:
#' \describe{
#'   \item{\code{SumsofSquares}}{Sums of Squares}
#'   \item{\code{F}}{F-values}
#'   \item{\code{estimate}}{Contrast estimates}
#'   \item{\code{t}}{t-values}
#'   \item{\code{p}}{Two-tailed p-values}
#'   \item{\code{rEffectSize}}{Correlation between the dependent variable and the
#'    contrast weights}
#'   \item{\code{rAlerting}}{Correlation between group means and contrast weights}
#'   \item{\code{r2Aalerting}}{Squared \code{rAlerting}; can be interpreted similar to
#'    a determination coefficient as a measured of explained variance by the contrast
#'   \code{(SScontrast/SSbetween)}}
#'   \item{\code{rContrast}}{\code{sqrt(t^2/(t^2 + df))}; useful for power analyses}
#' }
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # load iris dataset
#' data("iris")
#' iris <- iris[,c("Species", "Petal.Length")]
#' nGroup <- length(levels(iris$Species))
#'
#' # define lambda weights
#' lambda <- matrix(c(
#'                 -1,0,1, # H1: An increase from setosa over versicolor to virginica
#'                 -2,1,1), # H2: Setosa has smaller Petal Length than versicolor and virginica
#'                 ncol = nGroup,
#'                 byrow=TRUE)
#' # perform contrast analysis
#' contrast_independent(nGroup, lambda, iris)
#' # t > 2 indicates that Contrast 1 and Contrast 2 fit the data well
#' # Both tests are significant with p < .05
#' # It seems that there is an increase in Petal Length from setosa over versicolor to virginica,
#' # but at the same time the results suggest that setosa has smaller Petal Length than versicolor
#' # and virginica.
#'
#'
#' @export
contrast_independent <- function(nGroup,
                                 lambda,
                                 dat) {


  # Checks on the input ------------------------------------------------
  names(dat) <- c("groups", "values")
  if (nGroup != length(unique(dat$groups)) | nGroup != ncol(lambda)) {
    stop("Please check the data format: \n",
         " * the first column must contain the group indicator \n",
         " * the second the dependent variable \n",
         " * nGroup must be the total number of between-subject groups \n",
         " * lambda must contain the contrast weights in rows and group indicator in columns")
  }
  if (all(!(rowSums(lambda) - 0) < .Machine$double.eps)) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }

  # Get group size, means and variances -------------------------------------
  groupVals <- data.frame(groups = unique(dat$groups),
                          groupSize = tapply(dat$values,dat$groups,length),
                          groupMeans = tapply(dat$values,dat$groups,mean),
                          groupVariances = tapply(dat$values,dat$groups,var)

  )

  # Define contrast estimate ------------------------------------------------
  numerator <- lambda %*% groupVals$groupMeans
  denominator <- (lambda^2) %*% (groupVals$groupSize^(-1))
  # dfContrast <- 1



  # SS contrast and SS within -----------------------------------------------
  SScontrast <- (numerator^2) / denominator
  MSwithin <- sum(groupVals$groupVariances) / nGroup
  # SSwithin <- (nrow(dat) - nGroup) * MSwithin


  # F and t values ----------------------------------------------------------
  Fcontrast <- SScontrast / MSwithin
  tcontrast <- numerator / sqrt(MSwithin * denominator)
  pval <- 2 * pt( - abs(tcontrast), min(groupVals$groupSize))



  # Effect sizes ------------------------------------------------------------
  # r effectsize
  groupNames <- unique(dat$groups)
  colnames(lambda) <- groupNames
  dat <- cbind(dat,
               contrast = optimbase::transpose(
                 lambda[, match(dat$groups,
                                colnames(lambda))]))
  r_effectsize <- cor(dat[, -1])[1, -1]
  # r alerting
  r_alerting <- c(cor(groupVals$groupMeans, t(lambda)))
  # r contrast
  r_contrast <- sqrt(tcontrast^2 / (tcontrast^2 + nrow(dat) - nGroup))


  # Format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("SumsofSquares" = round(SScontrast, rounding),
                       "F" = round(Fcontrast, rounding),
                       "estimate" = round(numerator, rounding),
                       "t" = round(tcontrast, rounding),
                       "p" = round(pval, rounding),
                       "rEffectSize" = round(r_effectsize, rounding),
                       "rAlerting" = round(r_alerting, rounding),
                       "r2Alerting" = round(r_alerting^2, rounding),
                       "rContrast" = round(r_contrast, rounding))
  row.names(output) <- paste0("Contrast ", 1:nrow(lambda))

  return(output)
}
