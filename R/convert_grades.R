#'Convert Grades to Marks
#'
#'\code{convert_grades} convert letter grades to numeric marks
#'
#'@param grades vector of letter or number grades
#'@param to convert to `letters` or `numbers` (default NA converts to other type)
#'@param scale conversion table containing `letters` and `numbers`
#'
#'@return vector
#'@examples
#'convert_grades(marking_example$marks$Grade)
#'@export

convert_grades <- function(grades, to = NA, scale = glasgow22()) {
  stopifnot(length(grades) > 0)
  stopifnot("numbers" %in% colnames(scale))
  stopifnot("letters" %in% colnames(scale))
  
  # add blanks and NAs to scale
  scale <- scale %>%
    dplyr::add_row(letters = "", numbers = "") %>%
    dplyr::add_row(letters = NA, numbers = NA)
  
  present_grades <- grades[grades!="" & !is.na(grades)]
  
  if (mean(grades %in% scale$numbers) == 1) {
    if (to == "numbers") {
      # leave as is
      converted <- grades
    } else {
    # convert to letters
      converted <- scale$letters[match(grades, scale$numbers)]
    }
  } else if (mean(toupper(grades) %in% toupper(scale$letters)) == 1) {
    if (to == "letters") {
      # leave as is (uppercase)
      converted <- toupper(grades)
    } else {
      # convert to numbers
      converted <- as.integer( scale$numbers[match(toupper(grades), toupper(scale$letters))] )
    }
  } else {
    stop("Some grades are not in the scale")
  }
  
  converted
}

#'Glasgow 22-Point Marking Scale
#'
#'\code{glasgow22} The University of Glasgow 22-point marking scale conversion table
#'
#'@return tibble
#'@examples
#'glasgow22()
#'@export

glasgow22 <- function() {
  tibble::tibble(
    letters =c("CR", "CW", "MV", "H", "G2", "G1", "F3", "F2", "F1", 
            "E3", "E2", "E1", "D3", "D2", "D1", 
            "C3", "C2", "C1", "B3", "B2", "B1", 
            "A5", "A4", "A3", "A2", "A1"), 
    numbers = c(NA, NA, NA, 0:22)
  )
}
