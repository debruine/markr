#' Create Feedback Reports
#'
#' \code{make_feedback} creates feedback PDFs from a template .Rmd file
#' and saves them in the moodle directory under the appropriate
#' participant folder
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param template path to the .Rmd template file for feedback
#' @param filename the name to give the rendered file (defaults to feedback.pdf)
#' @param moodle_dir give the directory a moodle-style name, use the pattern 'Participant_*_assignsubmission_onlinetext_' where \* is replaced by the moodle ID (defaults FALSE)
#' @param quiet passed to rmarkdown::render (default TRUE)
#' @param filetype output a pdf or html file (defaults to pdf)
#' 
#' @return none
#' @examples
#' make_feedback(marking_example, "787420_template.Rmd")
#' @export
 
make_feedback <- function(marking,
                          template, # the template file for feedback
                          filename = "feedback.pdf",
                          moodle_dir = FALSE,
                          quiet = TRUE,
                          filetype = "pdf") {
  stopifnot(file.exists(template))
  
  # set feedback directory or create it if none exists
  fb_dir <- if (is.na(marking$dir)) "feedback" else marking$dir
  if (!dir.exists(fb_dir)) {
    dir.create(fb_dir, recursive = TRUE)
  }
  
  for (myid in unique(marking$marks$moodle_id)){
    # get list on each recursion in case new dirs are made
    all_dirs <- list.dirs(fb_dir) 
    pdir_n <- grep(myid, all_dirs)
    
    if (length(pdir_n) == 1) {
      # save in a dir that contains the id
      pdir <- all_dirs[pdir_n]
    } else if (length(pdir_n) == 0) {
      # create a dir that is the id
      if (moodle_dir != FALSE) {
        moodle_dir_ind <- gsub("*", myid, moodle_dir, fixed = TRUE)
        pdir <- paste0(fb_dir, '/', moodle_dir_ind)
      } else {
        pdir <- paste0(fb_dir, "/", myid)
      }
      dir.create(pdir)
    } else if (length(pdir_n) > 1) {
      stop(paste("ID", myid, "could go in multiple directories"))
    }
    
    systime <- system.time(
      rmarkdown::render(
        template,
        output_file = paste0(pdir, '/', filename),
        quiet = quiet,
        encoding = 'UTF-8'
      )
    )
    etime <- round(systime['elapsed'], 2)
    print(paste(myid, "completed (", etime, "s)"))

  }
}