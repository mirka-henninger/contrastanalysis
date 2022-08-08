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
