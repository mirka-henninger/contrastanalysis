#' Contrast analyses for independent samples
#'
#' This function allows to perform contrast analyses for independent samples
#'
#' @param nGroup: Number of independent / between-subject groups
#' @param lambda: a matrix of contrast weights with contrasts in rows and groups in columns
#' @param dat: a matrix or dataframe with two columns; each row contains values for one respondents;
#' the first column contains the group indicator, the second column contains the dependent variable
contrast_independent <- function(nGroup, lambda, dat){
  names(dat) <- c("groups", "values")
  # checks
  if(nGroup != length(unique(dat$groups)) & nGroup != ncol(lambda)) {
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

  # get group size, means and variances
  groupVals <- dat %>% group_by(groups) %>% summarize(groupSize = length(values),
                                                      groupMeans = mean(values),
                                                      groupVariances = var(values))

  numerator <- lambda %*% groupVals$groupMeans
  denominator <- (lambda^2) %*% (groupVals$groupSize^(-1))
  # dfContrast <- 1

  # SS contrast
  SScontrast <- (numerator^2) / denominator
  SSwithin <- sum(groupVals$groupVariances)/nGroup
  Fcontrast <- SScontrast / SSwithin
  tcontrast <- numerator / sqrt(SSwithin * denominator)
  output <- data.frame("SumsofSquares" = SScontrast,
                       "F" = Fcontrast,
                       "estimate" = numerator,
                       "t" = tcontrast,
                       "p" = pnorm(-abs(tcontrast))) %>% round(.,4)
  row.names(output) <- paste0("Contrast ", row.names(output))
  return(output)
}
