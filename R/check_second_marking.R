#' Create Second Marking Table
#'
#' \code{check_second_marking} returns a table with first and second marks
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param filename path to second marking file or directory
#' @param report render output for a LaTeX report (default FALSE)
#' 
#' @return list (or latex for a report)
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
      
      subm$moodle_id = as.integer(subm$moodle_id)
      
      if (nrow(second_marks) == 0) {
        second_marks <- subm
      } else {
        second_marks <- dplyr::bind_rows(second_marks, subm)
      }
    }
  }
  
  all_na <- dplyr::pull(second_marks, grade2) %>% is.na() %>% mean()
  if (all_na == 1) {
    warning("No second marks have been given")
    return(marking)
  }
  
  second_marks <- second_marks %>%
    dplyr::select(moodle_id, question, grade2)

  smark_table <- marking$marks %>%
    dplyr::select(-dplyr::matches("grade2")) %>%
    dplyr::left_join(second_marks, by = c("moodle_id", "question")) %>%
    dplyr::mutate(mark2 = convert_grades(grade2, to = "numbers")) %>%
    dplyr::mutate(discrepency = (mark2 - mark1))
  
  if (report) {
    st <- smark_table %>%
      dplyr::filter(!is.na(mark2)) %>%
      dplyr::select(moodle_id, question, agreed = Grade, mark1, mark2, discrepency) %>%
      dplyr::arrange(question, dplyr::desc(abs(discrepency)), discrepency) %>%
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
                             justify = c("left", "left", "right", "right", "right", "right")
        )
      }))
  } else {
    marking$marks <- smark_table
    
    marking
  }
  
}