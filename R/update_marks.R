#' Update Marks
#'
#' \code{update_marks} returns a table with first and second marks
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param filename path to updated marking file (required columns: moodle_id, question, updated_grade) 
#' 
#' @return list
#' @examples
#' update_marks(marking_example, "final_marks.csv")
#' @export

update_marks <- function(marking, filename) {
  stopifnot(file.exists(filename))
  
  # load updated marks
  message(paste("Loading", filename))
  suppressMessages(
    if (grepl("\\.xls(x)?$", filename)) {
      updated_marks <- readxl::read_excel(filename)
    } else if (grepl("\\.csv$", filename)) {
      updated_marks <- readr::read_csv(filename)
    } else if (grepl("\\.txt$", filename)) {
      updated_marks <- readr::read_tsv(filename)
    }
  )
  
  updated_marks <- updated_marks %>%
    dplyr::mutate(updated_grade = convert_grades(updated_grade, to = "letters")) %>%
    dplyr::select(moodle_id, question, updated_grade, dplyr::contains("comment"))
  
  um <- marking$marks %>%
    dplyr::left_join(updated_marks, by = c("moodle_id", "question")) %>%
    dplyr::mutate(
      Grade = ifelse(is.na(updated_grade), grade1, updated_grade),
      mark = convert_grades(Grade, to = "numbers")
    )
  
  n_updated <- um %>%
    dplyr::filter(updated_grade != grade1) %>%
    nrow()
  
  message(paste(n_updated, "marks updated"))
  
  marking$marks <- um
  
  marking
}