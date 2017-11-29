#'Convert Grades to Marks
#'
#'\code{convert_grades} convert letter grades to numeric marks
#'
#'@param grades vector of letter or number grades
#'@param scale conversion table containing `letters` and `numbers`
#'
#'@return vector
#'@examples
#'convert_grades(marking_example$marks$Grade)
#'@export

convert_grades <- function(grades, scale = glasgow22()) {
  stopifnot(length(grades) > 0)
  stopifnot("numbers" %in% colnames(scale))
  stopifnot("letters" %in% colnames(scale))
  
  if (mean(grades %in% scale$numbers) == 1) {
    scale$letters[match(grades, scale$numbers)]
  } else if (mean(grades %in% scale$letters) == 1) {
    scale$numbers[match(grades, scale$letters)]
  } else {
    stop("Some grades are not in the scale")
  }
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
    letters =c("H", "G2", "G1", "F3", "F2", "F1", 
            "E3", "E2", "E1", "D3", "D2", "D1", 
            "C3", "C2", "C1", "B3", "B2", "B1", 
            "A5", "A4", "A3", "A2", "A1"), 
    numbers = 0:22
  )
}
