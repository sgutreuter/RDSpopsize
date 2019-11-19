#' Simulated data from a respondent-driven survey of female sex workers
#'
#' A total of 1000 imaginary keychains of a unique type were distributed to
#' imaginary female sex  workers in an imaginary urban area.  One week later a
#' respondent-driven survey recruited 512 imaginary respondents.  Respondents
#' were asked whether they received one of the imaginary keychains.  Nothing
#' here is real.
#'
#' @format  A data frame with the following two variables:
#' \describe{
#' \item{\code{I.object}}{a logical (TRUE/FALSE) or binary (0,1) vector
#' indicating whether respondents received the particular keychain prior to the
#' start of the respondent-driven survey}
#' \item{\code{Weight}}{a numeric vector of respondent-driven survey weights}
#' }
#'
#' @aliases FSW01
#' @examples
#' data(FSW)
#' FSW[1:10, ]
#'
"FSW"
