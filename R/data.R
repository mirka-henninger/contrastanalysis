#' Testing Effect data
#'
#' This dataset originates from a study conducted as part of a research seminar
#' in the Psychology B.Sc. program of the University of Cologne.
#' The study participants learned a list of 20 non-associated word pairs.
#' Each half of the word pair was associated with one of two sources (imaginating
#' the word pair in the sky or underwater). The final memory test (cued recall) was
#' conducted two days later. Cued recall means that one word of the word pair was
#' presented, and the participant had to recall the other word.
#' The participants were randomly assigned into one of three between-participant
#' conditions: restudy, source test, item test.
#'
#' @format a data frame with 60 rows and 3 variables:
#' \describe{
#'   \item{subject}{the participant's id}
#'   \item{condition}{the between-partipant condition}
#'   \item{recalled}{the number of words recalled in the cued-recall test}
#' }
#'
"testing_effect"
#'
#' Working memory data
#'
#' This dataset originates from a study conducted at the University of Zurich
#' in the area of visual working memory. This is a vast simplification of
#' the original study for illustrative purposes.
#' The experiment consisted of 324 trials per participant. They were presented
#' with eather two, four, or eight colored squares (set size manipulation) and
#' had to reproduce the color shortly afterwards. Then they were asked to
#' judge their confidence on the given response.
#'
#' For more details see: https://psyarxiv.com/xmf24/
#'
#' For the entire dataset and analysis scripts see: https://osf.io/5mgzt/
#'
#' @format a data frame with 6660 rows and 4 variables:
#' \describe{
#'   \item{confidence_setsize_2}{mean confidence rating per participants at set size = 2}
#'   \item{confidence_setsize_3}{mean confidence rating per participants at set size = 4}
#'   \item{confidence_setsize_8}{mean confidence rating per participants at set size = 8}
#' }
#'
"working_memory"
