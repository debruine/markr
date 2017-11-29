#' Load Marking Data
#'
#' \code{load_marks} loads marking data 
#'
#' @param markfile path to marking file
#' @param dir path to moodle submission directory
#' @param evalfile path to evaluation criteria file (optional)
#' @param assign_id Moodle assignment ID (set to last numbers of markfile if missing)
#' @param id_col column where the participant id number is found (can be named or numeric) (defaults to 1)
#' @param encoding encoding for importing .csv files (defaults to latin1)
#' 
#' @return list
#' @examples
#' load_marks("001_marking.csv", "001_submissions", "001_eval.csv")
#' @export

load_marks <- function(markfile = "marks.csv", 
                      dir = "submissions",
                      evalfile = NA,
                      assign_id = NA,
                      id_col = 1,
                      encoding = "latin1") {
  stopifnot(dir.exists(dir))
  stopifnot(file.exists(markfile))
  
  marking <- list(
    marks = NA,
    dir = dir,
    eval = NA
  )
  
  if (stringr::str_sub(markfile, -4) == ".csv") {
    # encoding needed because of stupid •ÈÀIdentifier in moodle
    m <- readr::read_csv(
      markfile, 
      local = readr::locale(encoding = encoding)
    )
  } else if (stringr::str_sub(markfile, -4) == ".xls") {
    m <- readxl::read_xls(markfile)
  } else if (stringr::str_sub(markfile, -5) == ".xlsx") {
    m <- readxl::read_xlsx(markfile)
  } else {
    stop(paste(markfile, "needs to be a CSV or Excel file"))
  }
  
  if (is.na(assign_id)) {
    # get last number in filename if assign_id is not set manually
    #fn <- stringr::str_extract_all(markfile, "[[:digit:]]+", simplify = TRUE)
    #assign_id <- fn[length(fn)]
    
    # give the markfile name if not specified
    assign_id <- gsub("\\.(csv|xls|xlsx)$", "", markfile)
  }
  
  # extracts participant ID from the ID column (default is "Participant 12345678")
  #pid <- purrr::map(m[id_col], stringr::str_extract_all, "[[:digit:]]+", simplify = TRUE)
  
  marking$marks <- m %>%
    dplyr::mutate(
      mark = convert_grades(Grade),
      assign_id = assign_id
    ) %>%
    tidyr::separate(id_col, c("p", "id"), remove = FALSE, convert = TRUE) %>%
    dplyr::select(-p)

  if (!is.na(evalfile)) {
    if (!file.exists(evalfile)) {
      stop(paste(evalfile, "does not exist"))
    } else if (stringr::str_sub(evalfile, -4) == ".csv") {
      marking$eval <- readr::read_csv(evalfile)
    } else if (stringr::str_sub(evalfile, -4) == ".xls") {
      marking$eval <- readxl::read_xls(evalfile)
    } else if (stringr::str_sub(evalfile, -5) == ".xlsx") {
      marking$eval <- readxl::read_xlsx(evalfile)
    } else {
      stop(paste(evalfile, "needs to be a CSV or Excel file"))
    }
  }
  
  marking
}

                      