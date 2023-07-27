#' Tools for working with scientfific notations.
#'
#' These functions perform conversions between floating-point numbers, engineering and scientific notations.
#' @return \code{get_si()}, \code{exponent_to_si()}: A character vector with SI prefix(es)
#' @return \code{si_to_exponent()}, \code{get_exp_eng}, \code{get_factor_eng}: A numeric vector
#' @seealso \link[sitools:f2si]{sitools::f2si()}
#' @name ScientificNotation
#' @examples{
#' get_exp_eng(1000)
#' # And also:
#' exponent_to_si(3)
#'
#' si_to_exponent("nA")
#' si_to_exponent("p")
#'
#'
#' exponent_to_si(3)
#' # But:
#' exponent_to_si(2)
#'
#' get_exp_eng(1000)
#' # And also:
#' exponent_to_si(10000)
#'
#' get_factor_eng(1234)
#' get_factor_eng(12345)
#' }
#' @param val A floating-point number
#' @export get_si
get_si <- function(val) {
  exponent_to_si(get_exp_eng(val))
}

#' @describeIn ScientificNotation Converts an SI prefix or full string into exponent
#' @param letter SI prefix or full SI string
#' @export si_to_exponent
si_to_exponent <- function(letter) {
  exponent <- seq(-24L, 24L, 3L)
  letter <- substr(letter, 1, 1)
  if (letter == "\u03BC") {
    letter = "u"
  }

  sipref <-
    c("y",
      "z",
      "a",
      "f",
      "p",
      "n",
      "u",
      "m",
      "",
      "k",
      "M",
      "G",
      "T",
      "P",
      "E",
      "Z",
      "Y")

  return(10 ^ exponent[sipref == letter])
}

#' @importFrom sitools f2si
#' @describeIn ScientificNotation Converts an exponent into corresponding SI unit
#' @param exp The exponent of a number in scientific notation
#' @export exponent_to_si
exponent_to_si <- function(exp) {
  gsub("[^a-zA-Z]", "", f2si(10 ^ exp))
}

#' @describeIn ScientificNotation Gets the exponent of a number in engineering notation
#' @param val A floating-point number
#' @importFrom stats median
#' @importFrom stringr str_sub
#' @importFrom stringr str_locate
#' @export get_exp_eng
get_exp_eng <- function(val) {
  decimals <- seq(-21, 21, 3)
  decimals[which.min(abs(decimals - median(as.numeric(
    str_sub(format(val, scientific = T),
                     (
                       str_locate(format(val, scientific = T),
                                           "e[\\+\\-]")
                     )
                     [, 2])
  ))))]
}

#' @describeIn ScientificNotation Gets the factor before the base for a number
#' @export get_factor_eng
get_factor_eng <- function(val) {
  val / 10 ^ (get_exp_eng(val))
}
