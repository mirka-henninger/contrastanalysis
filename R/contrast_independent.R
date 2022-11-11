#' Contrast analyses for independent samples
#'
#' This function allows to perform contrast analyses for independent samples
#'
#' @param n_group Number of independent / between-subject groups
#' @param lambda A matrix of contrast weights with contrasts in rows and
#' groups in columns; or a vector for a single contrast with contrast weights
#' @param data A matrix or dataframe with two columns; each row contains
#' values for one respondent;
#' the first column contains the group indicator, the second column contains
#' the dependent variable
#'
#' @return A dataframe with following entries for each of the contrasts:
#' \describe{
#'   \item{\code{SumsofSquares}}{Sums of Squares}
#'   \item{\code{df}}{Degrees of freedom for each contrast}
#'   \item{\code{F}}{F-values}
#'   \item{\code{contrast estimate}}{Contrast estimates}
#'   \item{\code{t}}{t-values}
#'   \item{\code{p}}{Two-tailed p-values}
#'   \item{\code{rEffectSize}}{Correlation between the dependent variable and
#'   the contrast weights}
#'   \item{\code{rAlerting}}{Correlation between group means and contrast weights}
#'   \item{\code{r2Aalerting}}{Squared \code{rAlerting}; can be interpreted
#'   similar to a determination coefficient as a measured of explained variance
#'   by the contrast
#'   \code{(SS_contrast/SSbetween)}}
#'   \item{\code{rContrast}}{\code{sqrt(t^2/(t^2 + df))}; useful for power analyses}
#' }
#'
#' @source Rosenthal et al. (2000); Sedlmeier & Renkewitz (2013)
#'
#' @examples
#' # load iris dataset
#' data("iris")
#' iris <- iris[,c("Species", "Petal.Length")]
#' n_group <- length(levels(iris$Species))
#'
#' # define lambda weights
#' lambda1 <- c(-1,0,1) # H1: An increase from setosa over versicolor to virginica
#' lambda2 <- c(-1,0.5,0.5) # H2: Setosa has smaller Petal Length than versicolor and virginica
#'
#' # perform contrast analysis
#' contrast_independent(n_group, lambda1, iris)
#' contrast_independent(n_group, lambda2, iris)
#'
#'
#' @export
contrast_independent <- function(n_group,
                                 lambda,
                                 data) {


  # Checks on the input ------------------------------------------------
  names(data) <- c("groups", "values")
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
  if (n_group != length(unique(data$groups)) | n_group != ncol(lambda)) {
    stop("Please check the data format: \n",
         " * the first column must contain the group indicator \n",
         " * the second the dependent variable \n",
         " * n_group must be the total number of between-subject groups \n",
         " * lambda must contain the contrast weights in rows and group indicator
         in columns")
  }
  if (all(!(abs(rowSums(lambda)) < .Machine$double.eps^0.5))) {
    stop("Your contrast weights do not sum to 0 for all contrasts. ",
         "Please check the weights again!")
  }

  # Get group size, means and variances -------------------------------------
  group_vals <- data.frame(groups = unique(data$groups),
                           group_size = tapply(data$values,data$groups,length),
                           group_means = tapply(data$values,data$groups,mean),
                           group_variances = tapply(data$values,data$groups,var)

  )

  # Define contrast estimate ------------------------------------------------
  numerator <- lambda %*% group_vals$group_means
  denominator <- (lambda^2) %*% (group_vals$group_size^(-1))
  df <- nrow(data) - n_group



  # SS contrast and SS within -----------------------------------------------
  SS_contrast <- (numerator^2) / denominator
  MS_within <- sum(group_vals$group_variances) / n_group
  # SSwithin <- (nrow(data) - n_group) * MS_within


  # F and t values ----------------------------------------------------------
  F_contrast <- SS_contrast / MS_within
  tcontrast <- numerator / sqrt(MS_within * denominator)
  p_val <- 2 * pt( - abs(tcontrast), df)



  # Effect sizes ------------------------------------------------------------
  # r effectsize
  group_names <- unique(data$groups)
  colnames(lambda) <- group_names
  data <- cbind(data,
               contrast = optimbase::transpose(
                 lambda[, match(data$groups,
                                colnames(lambda))]))
  # r effect size
  r_effectsize <- cor(data[, -1])[1, -1]
  # r alerting
  r_alerting <- c(cor(group_vals$group_means, t(lambda)))
  # r contrast
  r_contrast <- sqrt(tcontrast^2 / (tcontrast^2 + nrow(data) - n_group))

  # Format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("SS" = round(SS_contrast, rounding),
                       "df" = df,
                       "contrast_estimate" = round(numerator, rounding),
                       "t_value" = round(tcontrast, rounding),
                       "F_value" = round(F_contrast, rounding),
                       "p_value" = round(p_val, rounding),
                       "r_effect_size" = round(r_effectsize, rounding),
                       "r_alerting" = round(r_alerting, rounding),
                       "r2_alerting" = round(r_alerting^2, rounding),
                       "r_contrast" = round(r_contrast, rounding))
  row.names(output) <- paste0("Contrast ", 1:nrow(lambda))

  return(output)
}
