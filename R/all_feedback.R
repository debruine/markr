#' Generate All Feedback (Glasgow-specific)
#'
#' \code{all_feedback} generates feedback for all files in a directory
#'
#' @param markfile path to marking file or directory of marking files
#' @param id_col column where the participant id number is found (can be named or numeric) (defaults to 1)
#' @param moodlefile path to file of student_id and moodle_id matches
#' @param evalfile path to evaluation criteria file (optional)
#' @param template_file path to the .Rmd template file for feedback (default of FALSE uses template called {class_id}_template.Rmd for each class
#' @param moodle_dir give the directory a moodle-style name, use the pattern 'Participant_*_assignsubmission_onlinetext_' where \* is replaced by the moodle ID (defaults FALSE)
#' 
#' @return list
#' @examples
#' load_marks("001_marking.csv", "001_submissions", "001_eval.csv")
#' @export


all_feedback <- function(markfile, 
                         id_col = 1, 
                         moodlefile = NA, 
                         evalfile = NA, 
                         template_file = FALSE, 
                         moodle_dir = FALSE) {
  
  m <- load_marks(markfile = markfile, 
                  id_col = id_col, 
                  moodlefile = moodlefile, 
                  evalfile = evalfile)
  
  # remove MV, CW and CR
  # double up line breaks to make paragraphs
  
  m$marks <- m$marks %>%
    dplyr::filter(!(Grade %in% c("MV", "CR", "CW") )) %>%
    dplyr::mutate(`Generic Feedback` = gsub("\n", "\n\n", `Generic Feedback`),
           question = paste0("Q", Question),
           id = `Student ID`)
  
  #View(m$marks)
  
  # check for missing feedback
  # contact course leads if not 0
  missing_feedback <- m$marks %>% 
    dplyr::filter(is.na(`Individual Feedback 1`) | 
             is.na(`Individual Feedback 2`) | 
             is.na(`Individual Feedback 3`) | 
             is.na(`Generic Feedback`) |
             `Generic Feedback` == "null")
  
  # check for missing moodle IDs
  # contact Helena Patterson if not 0
  no_moodle <- m$marks %>% dplyr::filter(is.na(moodle_id))

  # check for duplicate marks for same student:class:Q
  # contact Mac Becirsphic if not 0
  dupe_marks <- m$marks %>% 
    dplyr::group_by(`Student ID`, class_id, Question) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::filter(n > 1)
  
  if (nrow(missing_feedback) > 0) {
    warning("Missing Feedback: contact course lead")
    return(missing_feedback)
  }
  if (nrow(dupe_marks) > 0) {
    warning("Duplicate Marks: contact Marc Becirspahic")
    return(dupe_marks)
  }
  if (nrow(no_moodle) > 0) {
    warning("Missing Moodle IDs: contact Helena Patterson")
    return(no_moodle)
  }
  
  # make an overview plot
  m$marks %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(aes(mark, fill = question),
                   color = "white",
                   binwidth = 1) +
    ggplot2::xlim(-0.5, 22.5) +
    ggplot2::facet_wrap(~class_id, scales="free_y") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_minimal()
  
  if (!dir.exists("figures")) dir.create("figures")
  ggplot2::ggsave("figures/all_marks.png", width = 10, height = 10)
  
  # get all classes
  classes <- m$marks %>%
    dplyr::pull(class_id) %>%
    dplyr::unique()
  
  for (c_id in classes) {
    m2 <- m
    
    m2$marks <- m2$marks %>% dplyr::filter(class_id == c_id)
    
    if (template_file == FALSE) {
      template_file <- paste0(c_id, "_template.Rmd")
    }
    
    if (file.exists(template_file)) {
      make_feedback(
        m2,
        template = template_file,
        filename = paste0(c_id, "_feedback.html"),
        moodle_dir = moodle_dir
      )
    } else {
      warning(paste(template_file, "doesn't exist"))
    }
  }
}