#' markr: University of Glasgow INP/Psych Marking.
#'
#' Provides functions for electronic marking with moodle.
#'
#' @docType package
#' @name markr
#' @importFrom dplyr %>%
#' 
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))