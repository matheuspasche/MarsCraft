#' Title
#' @param x numeric or complex or logical vectors.
#' @param ... ...
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @export
#'
mean <- function (x, na.rm = T, ...) {
  base::mean(x, na.rm = na.rm, ...)
}


#' Title
#'
#' @param ... numeric or complex or logical vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @export
#'
sum <- function (..., na.rm = T) {
  base::sum(..., na.rm = na.rm)
}


#' Title
#'
#' @param x numeric or complex or logical vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @export
#'
mean <- function (x, na.rm = T,...) {
  base::mean(x, na.rm = na.rm,...)
}


#' Title
#'
#' @param x numeric or complex or logical vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @export
#'
median <- function (x, na.rm = T,...) {
  base::median(x, na.rm = na.rm,...)
}
