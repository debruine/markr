#' Create Second Marking Table
#'
#' \code{check_second_marking} returns a table with first and second marks
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param filename path of second marking file
#' 
#' @return none
#' @examples
#' check_second_marking(marking_example)
#' @export

check_second_marking <- function(marking, filename = NA) {
  fb <- marking$marks
  
  if (is.na(filename)) {
    assign_id <- fb$assign_id[1]
    filename <- paste0("second_marking/", assign_id, "_second_marking.csv")
  }
  
  if (!file.exists(filename)) return(paste(filename, "does not exist"))
  
  second_marks <- suppressMessages(readr::read_csv(filename)) 
  
  all_na <- dplyr::pull(second_marks, mark2) %>% is.na() %>% mean()
  if (all_na == 1) return("No second marks have been given")
  
  second_marks %>%
    dplyr::left_join(fb, by = "id") %>%
    dplyr::select(id, mark, mark2) %>%
    dplyr::mutate(discrepency = (mark2 - mark)) %>%
    dplyr::arrange(dplyr::desc(abs(discrepency)), discrepency)
  
}