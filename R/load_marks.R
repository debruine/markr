#' Load Marking Data
#'
#' \code{load_marks} loads marking data 
#'
#' @param markfile path to marking file or directory of marking files
#' @param dir path to moodle submission directory (optional)
#' @param evalfile path to evaluation criteria file (optional)
#' @param assign_id Moodle assignment ID
#' @param id_col column where the participant id number is found (can be named or numeric) (defaults to 1)
#' @param moodlefile path to file of student_id and moodle_id matches
#' @param encoding encoding for importing .csv files (defaults to latin1)
#' 
#' @return list
#' @examples
#' load_marks("001_marking.csv", "001_submissions", "001_eval.csv")
#' @export

load_marks <- function(markfile = "marks.csv", 
                      dir = NA,
                      evalfile = NA,
                      assign_id = NA,
                      id_col = 1,
                      moodlefile = NA,
                      encoding = "latin1") {
  stopifnot(file.exists(markfile))
  
  if (!is.na(dir)) {
    if (!dir.exists(dir)) {
      warning(paste("The moodle submission directory", dir, "does not exist"))
    }
  }
  
  marking <- list(
    marks = NA,
    dir = dir,
    eval = NA
  )
  
  if (dir.exists(markfile)) {
    # markfile is a directory, get all files in it
    markfiles <- list.files(markfile, pattern = "\\.(xls|xlsx|csv|txt)$")
    m <- tibble::tibble()
    for (f in markfiles) {
      message(paste("Loading", f))
      
      filepath <- paste0(markfile, "/", f)
      
      suppressMessages(
        if (grepl("\\.xls(x)?$", f)) {
          subm <- readxl::read_excel(filepath)
        } else if (grepl("\\.csv$", f)) {
          subm <- readr::read_csv(filepath,
                                  col_types = cols("Student ID" = col_character())
                                  )
        } else if (grepl("\\.txt$", f)) {
          subm <- readr::read_tsv(filepath)
        }
      )
      
      subm <- subm %>%
        dplyr::mutate(file = gsub("\\.(xls|xlsx|csv|txt)$", "", f)) %>%
        tidyr::separate(
          file, # e.g., PSYCH4006 Human Development_Exam_ind_feedback_2018-2019
          into = c("class_id"),
          sep = " ",
          convert = TRUE,
          extra = "drop",
          remove = FALSE
        ) %>%
        tidyr::separate(
          file, # e.g., PSYCH4006 Human Development_Exam_ind_feedback_2018-2019
          into = c("assign_id", "junk1", "junk2", "junk3", "year"),
          sep = "_",
          convert = TRUE,
          extra = "merge",
          fill = "right"
        ) %>%
        select(-junk1, -junk2, -junk3) %>%
        dplyr::mutate(marker = gsub("-", " ", Marker))
      
      if (nrow(m) == 0) {
        m <- subm
      } else {
        m <- dplyr::bind_rows(m, subm)
      }
    }
    
    if (is.na(assign_id)) {
      # use year_classid if not specified
      assign_id <- paste(m$year[1], m$class_id[1], sep = "_")
    }
    
  } else {
    #markfile is a file
    if (stringr::str_sub(markfile, -4) == ".csv") {
      # encoding needed because of stupid identifier in moodle
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
      # give the markfile name if not specified
      assign_id <- gsub("\\.(csv|xls|xlsx)$", "", markfile)
    }
  }
  
  # extracts participant ID from the ID column (default is "Participant 12345678")
  pid <- purrr::map(
    m[id_col], 
    stringr::str_extract_all, 
    "[[:digit:]]+", 
    simplify = TRUE
  ) %>%
    unlist() %>%
    as.integer()
  
  # load or create moodle ID matching file
  if (!is.na(moodlefile)) {
    if (file.exists(moodlefile)) {
      suppressMessages(
        if (grepl("\\.xls(x)?$", moodlefile)) {
          moodle_match <- readxl::read_excel(moodlefile)
        } else if (grepl("\\.csv$", moodlefile)) {
          moodle_match <- readr::read_csv(moodlefile)
        } else if (grepl("\\.txt$", moodlefile)) {
          moodle_match <- readr::read_tsv(moodlefile)
        }
      )
      
      moodle_match <- moodle_match %>%
        dplyr::mutate(
          student_id = as.integer(student_id),
          moodle_id = as.integer(moodle_id)
        )
    } else {
      warning(paste(moodlefile, "does not exist"))
    }
  } else {
    moodle_match <- tibble::tibble(
      student_id = pid,
      moodle_id = pid
    ) %>%
      unique()
  }

  marking$marks <- m %>%
    dplyr::mutate(
      assign_id = assign_id,
      mark = convert_grades(Grade, to = "numbers"),
      grade1 = convert_grades(Grade, to = "letters"),
      mark1 = mark,
      student_id = pid
    ) %>%
    dplyr::left_join(moodle_match, by = "student_id") %>%
    dplyr::select(-student_id)
  
  # load eval file
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
