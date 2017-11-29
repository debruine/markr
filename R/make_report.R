#' Create Assessment Summary Report
#'
#' \code{make_feedback} creates a report PDF from a dataframe of 
#' assignment evaluations
#'
#' @param marking list containing a dataframe (marks)
#' @param template path to the .Rmd template file for reports
#' @param show list of sections to show in the default report template
#' @param output_file path for the rendered file (defaults to feedback.pdf)
#' @param quiet passed to rmarkdown::render (default TRUE)
#' 
#' @return none
#' @examples
#' make_report(marking_example)
#' @export

make_report <- function(marking,
                        template = "report_template.Rmd",
                        show = c("eval", "mark2", "mark_dist", "cat_dist", "cat_mark"),
                        output_file = NA,
                        quiet = TRUE) {
  fb <- marking$marks
  eval <- marking$eval
  assign_id <- fb$assign_id[1]
  
  if (is.na(output_file)) {
    output_file = paste0(assign_id, "_report.pdf")
  }
  rmarkdown::render(
    template,
    output_file = output_file,
    quiet = quiet,
    encoding = 'UTF-8'
  )
}
