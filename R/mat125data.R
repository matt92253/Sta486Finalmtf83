#' Math 125 data
#'
#' NAU pre-calculus (mat 125) data set.
#' Contains blinded student test scores from Fall 2016 to Fall 2021.
#' Used to perform analysis on multiple attempt test scores and the effects of Covid-19.
#'
#'
#' @format A data frame with 95760 observations with 12 columns.
#' \describe{
#'    \item{Id}{Student id. Type: Int }
#'    \item{section_id}{Course section id. Type: Character}
#'    \item{module_final}{Test taken: M1 - module 1, M2 - module 2, M3 - module 3, M4 - module 4, F1 - final. Type: Factor}
#'    \item{test_attempt}{What attempt number was taken on a test: T1 - attempt 1, T2 - attempt 2. Type: Character}
#'    \item{score}{Score that was recieved on a test. Type: Numeric}
#'    \item{professor_Id}{Professor id. Type: Character}
#'    \item{learning_aid}{Binary indicator if a test taken had learning aids, 0 for no learning aids and 1 for learning aids. Type: Numeric}
#'    \item{honors_code}{Binary indicator if a test taken was for honors code, 0 for no honors code and 1 for honors code. Type: Numeric}
#'    \item{practice_test}{Binary indicator if a test taken was a practice test, 0 for not a practice test and 1 for practice test. Type: Numeric}
#'    \item{pre_test}{Binary indicator if a test taken was a pre test, 0 for not a pre test and 1 for pre test. Type: Numeric}
#'    \item{season}{The semester that the test was taken in, Spring or Fall. Type: Factor}
#'    \item{year}{The last two digits of the year that a test was taken in. Type: Character}
#' }
"mat125data"
