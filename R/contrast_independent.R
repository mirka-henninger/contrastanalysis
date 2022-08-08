#' Contrast analyses for independent samples
#'
#' This function allows to perform contrast analyses for independent samples
#'
#' @param n_group Number of independent / between-subject groups
#' @param lambda A matrix of contrast weights with contrasts in rows and
#' groups in columns
#' @param dat A matrix or dataframe with two columns; each row contains
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
#' lambda <- matrix(c(
#'                 -1,0,1, # H1: An increase from setosa over versicolor to virginica
#'                 -2,1,1), # H2: Setosa has smaller Petal Length than versicolor and virginica
#'                 ncol = n_group,
#'                 byrow=TRUE)
#' # perform contrast analysis
#' contrast_independent(n_group, lambda, iris)
#'
#'
#' @export
contrast_independent <- function(n_group,
                                 lambda,
                                 dat) {


  # Checks on the input ------------------------------------------------
  names(dat) <- c("groups", "values")
  if (n_group != length(unique(dat$groups)) | n_group != ncol(lambda)) {
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
  group_vals <- data.frame(groups = unique(dat$groups),
                           group_size = tapply(dat$values,dat$groups,length),
                           group_means = tapply(dat$values,dat$groups,mean),
                           group_variances = tapply(dat$values,dat$groups,var)

  )

  # Define contrast estimate ------------------------------------------------
  numerator <- lambda %*% group_vals$group_means
  denominator <- (lambda^2) %*% (group_vals$group_size^(-1))
  df_contrast <- 1



  # SS contrast and SS within -----------------------------------------------
  SS_contrast <- (numerator^2) / denominator
  MS_within <- sum(group_vals$group_variances) / n_group
  # SSwithin <- (nrow(dat) - n_group) * MS_within


  # F and t values ----------------------------------------------------------
  F_contrast <- SS_contrast / MS_within
  tcontrast <- numerator / sqrt(MS_within * denominator)
  p_val <- 2 * pt( - abs(tcontrast), min(group_vals$group_size))



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
  r_alerting <- c(cor(group_vals$group_means, t(lambda)))
  # r contrast
  r_contrast <- sqrt(tcontrast^2 / (tcontrast^2 + nrow(dat) - n_group))


  # Format output -----------------------------------------------------------
  rounding <- 4
  output <- data.frame("SS" = round(SS_contrast, rounding),
                       "df" = df_contrast,
                       "F_value" = round(F_contrast, rounding),
                       "contrast_estimate" = round(numerator, rounding),
                       "t_value" = round(tcontrast, rounding),
                       "p_value" = round(p_val, rounding),
                       "r_effect_size" = round(r_effectsize, rounding),
                       "r_alerting" = round(r_alerting, rounding),
                       "r2_alerting" = round(r_alerting^2, rounding),
                       "r_contrast" = round(r_contrast, rounding))
  row.names(output) <- paste0("Contrast ", 1:nrow(lambda))

  return(output)
}
