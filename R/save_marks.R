#' Save full marking list
#'
#' \code{save_marks} save the amalgamated marking list
#'
#' @param marking list containing a dataframe (marks)
#' @param filename path to save 
#' 
#' @return none
#' @examples
#' save_marks(marking, "full_marks.xls")
#' @export

save_marks <- function(marking, filename = NA) {
  if (is.na(filename)) {
    filename <- paste0(marking$marks$assign_id[1], "_all_marks.csv")
  }
  
  readr::write_excel_csv(marking$marks, filename)
}