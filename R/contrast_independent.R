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
    stop("Please check the data format: the first column must contain",
         "the group indicator, the second the dependent variable.",
         "nGroup must be the total number of between-subject groups.",
         "lambda must contain the contrasts in the rows and the groups in columns")
  }
  if(apply(lambda,1,rowSums) != 0){
    stop("Your contrast weights do not sum to 0. Please check the weights!")
  }

}
