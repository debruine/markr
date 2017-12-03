#' Create Second Marking Table
#'
#' \code{check_second_marking} returns a table with first and second marks
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param filename path to second marking file or directory
#' @param report render output for a LaTeX report (default FALSE)
#' 
#' @return tibble (or latex for a report)
#' @examples
#' check_second_marking(marking_example)
#' @export

check_second_marking <- function(marking, filename = "second_marking", report = FALSE) {
  stopifnot(file.exists(filename))
  
  if (dir.exists(filename)) {
    markfiles <- list.files(filename, pattern = "\\.(xls|xlsx|csv|txt)$")
    second_marks <- tibble::tibble()
    for (f in markfiles) {
      message(paste("Loading", f))
      suppressMessages(
        if (grepl("\\.xls(x)?$", f)) {
           subm <- readxl::read_excel(paste0(filename, "/", f))
        } else if (grepl("\\.csv$", f)) {
          subm <- readr::read_csv(paste0(filename, "/", f))
        } else if (grepl("\\.txt$", f)) {
          subm <- readr::read_tsv(paste0(filename, "/", f))
        }
      )
      
      subm$id = as.integer(subm$id)
      
      if (nrow(second_marks) == 0) {
        second_marks <- subm
      } else {
        second_marks <- dplyr::bind_rows(second_marks, subm)
      }
    }
  }
  
  all_na <- dplyr::pull(second_marks, mark2) %>% is.na() %>% mean()
  if (all_na == 1) return("No second marks have been given")
  
  smark_table <- second_marks %>%
    dplyr::left_join(marking$marks, by = c("id", "question")) %>%
    dplyr::select(id, question, mark, mark2) %>%
    dplyr::mutate(discrepency = (mark2 - mark)) %>%
    dplyr::arrange(question, dplyr::desc(abs(discrepency)), discrepency)
  
  if (report) {
    st <- smark_table %>%
      dplyr::mutate(q = question) %>%
      dplyr::group_by(q) %>%
      dplyr::mutate() %>%
      tidyr::nest() %>%
      dplyr::mutate(table = purrr::map(data, function(data) {
        diff <- abs(data$discrepency) > 2
        too_diff <- round(mean(diff) * 100)
        
        cat("\\subsection{", data$question[1], "}\n")
        cat(too_diff, "% more than 2 fine marks apart\n", sep = "")
        pander::pandoc.table(data, 
                             justify = c("left", "left", "right", "right", "right")
        )
      }))
  } else {
    smark_table
  }
  
}