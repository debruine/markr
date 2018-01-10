#' Generate Second Marking List
#'
#' \code{second_mark} determines what assessments need to be second marked 
#' and bundles files into a folder with the second marking sheet
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param dir directory name to save second marking in
#' @param remove_file filename to remove from second marking directory
#' @param pass_min minimum passing mark; all lower marks are second marked (default 9)
#' @param show_first_mark include first mark in second marking files (default FALSE)
#' 
#' @return none
#' @examples
#' second_mark(marking, "data-raw/example/submissions")
#' @export

second_mark <- function(marking, 
                        dir = "second_marking", 
                        remove_file = "feedback.pdf",
                        pass_min = 9, # minimum mark to pass (9 UG, 12 PG)
                        show_first_mark = FALSE
) {
  assign_id <- marking$marks$assign_id[1]
  
  # get distinct questions
  if ("question" %in% names(marking$marks)) {
    questions <- unique(marking$marks$question) %>% sort()
  } else {
    marking$marks <- dplyr::mutate(marking$marks, question = "Q1")
    questions <- "Q1"
  }
  
  # select second marks for each question
  for (q in questions) {
    # set seed to make this always the same for each assignment
    seed <- paste(assign_id, q) %>% utf8ToInt() %>% sum()
    set.seed(seed)
    
    fb <- dplyr::filter(marking$marks, question == q)
    
    # calculate how many to second mark
    # 10% of marks or 10, whichever is higher (or all if < 10)
    second_n <- max(10, ceiling(nrow(fb)/10))
    if (nrow(fb) <= second_n) {
      message(paste("Selecting all", nrow(fb), q, "to second mark"))
      to_second_mark <- dplyr::pull(fb, moodle_id)
    } else {
      # select all fails
      fails <- fb %>%
        dplyr::filter(mark < pass_min) %>%
        dplyr::pull(moodle_id)
      
      # get ~1/3 low, mid and high marks
      low_high_n <- max(0, floor((second_n - length(fails))/3))
      mid_n <- max(0, second_n - length(fails) - 2*low_high_n)
      
      f <- fb %>%
        dplyr::filter(mark >= pass_min) %>%
        dplyr::arrange(mark, moodle_id)
      
      if (nrow(f) > 0 & low_high_n > 0 & mid_n > 0) {
        f <- f %>% 
          dplyr::mutate(
            n = 1:nrow(.),
            band = ifelse(n <= nrow(.)/3, "low", "med"),
            band = ifelse(n > 2*nrow(.)/3, "high", band)
            # quantiles don't work well if there is a very uneven distribution of marks
            #band = ifelse(mark < stats::quantile(mark, 0.67), "med", "high"),
            #band = ifelse(mark < stats::quantile(mark, 0.33), "low", band)
          )
      
        lows <- f %>% 
          dplyr::filter(band == "low") %>%
          dplyr::pull(moodle_id) %>% 
          base::sample(low_high_n)
        mids <- f %>% 
          dplyr::filter(band == "med") %>%
          dplyr::pull(moodle_id) %>% 
          base::sample(mid_n)
        highs <- f %>% 
          dplyr::filter(band == "high") %>%
          dplyr::pull(moodle_id) %>% 
          base::sample(low_high_n)
      } else {
        message("No one passed")
        lows <- c()
        mids <- c()
        highs <- c()
      }
      
      to_second_mark <- c(fails, lows, mids, highs)
    }
    
    # create directory
    qdir <- paste0(dir, "/", q)
    dir.create(qdir, showWarnings = FALSE, recursive = TRUE)
    
    second_marks <- tibble::tibble(
      moodle_id = to_second_mark,
      question = q,
      grade2 = ""
    ) %>%
      dplyr::arrange(moodle_id) %>%
      dplyr::left_join(fb, by = c("moodle_id", "question")) %>% 
      dplyr::select(ID, moodle_id, question, mark1 = mark, grade1 = Grade, grade2)
      
    message(paste(q, "marks = (", toString(sort(second_marks$mark1)), ")"))
    
    if (!show_first_mark) {
      second_marks <- dplyr::select(second_marks, -mark1, -grade1)
    }
    
    # create marking file
    readr::write_csv(
      second_marks, 
      paste0(dir, "/", assign_id, "_", q, "_second_marking.csv")
    )
    
    # copy folders to 2nd marking folder
    if (is.na(marking$dir)) {
      message("No files were copied into the second marking folder")
    } else if (!dir.exists(marking$dir)) {
      message(paste("The feedback directory", marking$dir, 
                  "doesn't exist, so no files were copied into the second marking folder"))
    } else {
      all_dirs <- list.dirs(marking$dir) 
      for (moodle_id in to_second_mark) {
        pdir_n <- grep(moodle_id, all_dirs)
        if (length(pdir_n) == 1) {
          # get the dir that contains the id
          pdir <- all_dirs[pdir_n]
        } else if (length(pdir_n) == 0) {
          message(paste("No directory found for", moodle_id))
          pdir <- c()
        } else if (length(pdir_n) > 1) {
          message(paste(length(pdir_n), "directories copied for", moodle_id))
          pdir <- all_dirs[pdir_n]
        }
        file.copy(from = pdir, to = qdir,
                  overwrite = TRUE, 
                  recursive = TRUE, 
                  copy.mode = TRUE)
        
        # remove feedback files
        fb_file <- paste0(qdir, "/", remove_file)
        if (remove_file != "" & file.exists(fb_file)) {
          file.remove(fb_file)
        }
      }
    }
  }
}
