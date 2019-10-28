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
#' @return a dataframe with following entries: sums-of-squares, F-value, contrast estimate,
#' t value, ' two-tailed p value, and effect sizes r effect, r alerting, and r contrast
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # set.seed(1)
#' nGroup <- 4
#' lambda1 <- c(-1,-1,1,1)
#' lambda2 <- c(-3,-1,1,3)
#' dat <- data.frame(
#'   x = rep(c(1:4),each = 50),
#'   y = c(rnorm(50,-1,1),rnorm(50),rnorm(50),rnorm(50,1,1))
#' )
contrastCompare_independent <- function(nGroup, lambda1, lambda2, dat){

# checks on the input -----------------------------------------------------
  names(dat) <- c("groups", "values")
  if(nGroup != length(unique(dat$groups))
     | nGroup != length(lambda1)
     | nGroup != length(lambda2)) {
    stop("Please check the data format: the first column must contain ",
         "the group indicator, the second the dependent variable. ",
         "nGroup must be the total number of between-subject groups. ",
         "lambdaG1 and lambdaG2 must each contain one set of contrast weights")
  }
  if(sum(lambda1) != 0 | sum(lambda2) != 0){
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }


# standardize lambda weight -----------------------------------------------
  lambda1Std <- lambda1 / sqrt(mean(lambda1^2))
  lambda2Std <- lambda2 / sqrt(mean(lambda2^2))
  lambdaDiff <- lambda2Std - lambda1Std


# get group size, means and variances -------------------------------------
  groupVals <- dat %>% group_by(groups) %>% summarize(groupSize = length(values),
                                                      groupMeans = mean(values),
                                                      groupVariances = var(values))


# define contrast estimate ------------------------------------------------
  numerator <- lambdaDiff %*% groupVals$groupMeans
  denominator <- (lambdaDiff^2) %*% (groupVals$groupSize^(-1))
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
  names(lambdaDiff) <- groupNames
  dat <- cbind(dat,lambdaDiff = lambdaDiff[match(dat$groups,names(lambdaDiff))])
  r_effectsize <- cor(dat[,-1])[1,-1]
  # r alerting
  r_alerting <- c(cor(groupVals$groupMeans,lambdaDiff))
  # r contrast
  r_contrast <- sqrt(tcontrast^2/(tcontrast^2+nrow(dat)-nGroup))


# format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("SumsofSquares" = SScontrast %>% round(.,rounding),
                       "F" = Fcontrast %>% round(.,rounding),
                       "estimate" = numerator %>% round(.,rounding),
                       "t" = tcontrast %>% round(.,rounding),
                       "p" = pval %>% round(.,rounding),
                       "rEffectSize" = r_effectsize %>% round(.,rounding),
                       "rAlerting" = r_alerting %>% round(.,rounding),
                       "r2Alerting" = r_alerting^2 %>% round(.,rounding),
                       "rContrast" = r_contrast %>% round(.,rounding))
  row.names(output) <- paste0("Contrast ", row.names(output))

  return(output)
}

contrastCompare_independent(4,lambda1,lambda2,dat)
