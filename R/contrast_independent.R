#' Contrast analyses for independent samples
#'
#' This function allows to perform contrast analyses for independent samples
#'
#' @param nGroup Number of independent / between-subject groups
#' @param lambda a matrix of contrast weights with contrasts in rows and groups in
#' columns
#' @param dat a matrix or dataframe with two columns; each row contains values for
#' one respondents;
#' the first column contains the group indicator, the second column contains the
#' dependent variable
#'
#' @return a dataframe with following entries for each of the contrasts:
#' \describe{
#'   \item{\code{SumsofSquares}}{Sums of Squares}
#'   \item{\code{F}}{F-values}
#'   \item{\code{estimate}}{Contrast estimates}
#'   \item{\code{t}}{t-values}
#'   \item{\code{p}}{two-tailed p-values}
#'   \item{\code{rEffectSize}}{Correlation between the dependent variable and the
#'   contrast weights}
#'   \item{\code{rAlerting}}{Correlation between group means and contrast weights}
#'   \item{\code{r2Aalerting}}{Squared \code{rAlerting}; can be interpreted similar to
#'   a determination coefficient as a measured of explained variance by the contrast
#'   \code{(SScontrast/SSbetween)}}
#'   \item{\code{rContrast}}{\code{sqrt(t^2/(t^2 + df))}; useful for power analyses}
#' }
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # set.seed(1)
#' nGroup <- 4
#' lambda <- matrix(c(-1,-1,1,
#'                    1,-1,1,-1,
#'                    1,1,-1,-1,1),
#'                  ncol = 4,
#'                  byrow=TRUE)
#' dat <- data.frame(
#'   x = rep(c(1:4),each = 50),
#'   y = c(rnorm(50,-1,1),rnorm(50),rnorm(50),rnorm(50,1,1))
#' )
#' contrast_independent(nGroup, lambda, dat)
#' # results are the same as the standard linear model or anova:
#'
#' dat$c1 <- ifelse(dat$x==1 | dat$x==2, -1,1)
#' dat$c2 <- ifelse(dat$x==1 | dat$x==3, -1,1)
#' dat$c3 <- ifelse(dat$x==2 | dat$x==3, -1,1)
#' lmMod <- lm(y~c1+c2+c3,data=dat)
#' summary(lmMod)
#' anova(lmMod)
#'
#' @export
contrast_independent <- function(nGroup, lambda, dat){

  # some checks on the input ------------------------------------------------
  names(dat) <- c("groups", "values")
  if(nGroup != length(unique(dat$groups)) | nGroup != ncol(lambda)) {
    stop("Please check the data format: the first column must contain ",
         "the group indicator, the second the dependent variable. ",
         "nGroup must be the total number of between-subject groups. ",
         "lambda must contain the contrast weights in rows and the ",
         "group indicator in columns")
  }
  if(any(rowSums(lambda) != 0)){
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }

  # get group size, means and variances -------------------------------------
  groupVals <- dat %>%
    dplyr::group_by(.data$groups) %>%
    dplyr::summarize(groupSize = length(.data$values),
                     groupMeans = mean(.data$values),
                     groupVariances = var(.data$values))


  # define contrast estimate ------------------------------------------------
  numerator <- lambda %*% groupVals$groupMeans
  denominator <- (lambda^2) %*% (groupVals$groupSize^(-1))
  # dfContrast <- 1



  # SS contrast and SS within -----------------------------------------------
  SScontrast <- (numerator^2) / denominator
  MSwithin <- sum(groupVals$groupVariances)/nGroup
  # SSwithin <- (nrow(dat) - nGroup) * MSwithin


  # F and t values ----------------------------------------------------------
  Fcontrast <- SScontrast / MSwithin
  tcontrast <- numerator / sqrt(MSwithin * denominator)
  pval <- 2*pnorm(-abs(tcontrast))



  # effect sizes ------------------------------------------------------------
  # r effectsize
  groupNames <- unique(dat$groups)
  colnames(lambda) <- groupNames
  dat <- cbind(dat,
               contrast = optimbase::transpose(lambda[,match(dat$groups,colnames(lambda))]))
  r_effectsize <- cor(dat[,-1])[1,-1]
  # r alerting
  r_alerting <- c(cor(groupVals$groupMeans,t(lambda)))
  # r contrast
  r_contrast <- sqrt(tcontrast^2/(tcontrast^2+nrow(dat)-nGroup))


  # format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("SumsofSquares" = round(SScontrast,rounding),
                       "F" = round(Fcontrast,rounding),
                       "estimate" = round(numerator,rounding),
                       "t" = round(tcontrast,rounding),
                       "p" = round(pval,rounding),
                       "rEffectSize" = round(r_effectsize,rounding),
                       "rAlerting" = round(r_alerting,rounding),
                       "r2Alerting" = round(r_alerting^2,rounding),
                       "rContrast" = round(r_contrast,rounding))
  row.names(output) <- paste0("Contrast ", row.names(output))

  return(output)
}
