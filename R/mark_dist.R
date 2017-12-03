#' Marking Distribution Graph
#'
#' \code{mark_dist} create a graph of the marking distribution
#'
#' @param marking list containing a dataframe (marks)
#' @param show_by_question facet graphs by question (default FALSE)
#' 
#' @return ggplot
#' @examples
#' mark_dist(marking_example)
#' @export

mark_dist <- function(marking, show_by_question = FALSE) {
  md <- marking$marks %>%
    dplyr::filter(!is.na(mark)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(mark, fill = as.factor(mark)), 
      binwidth = 1, 
      color = "white"
    ) +
    ggplot2::scale_fill_manual(
      values = viridis::viridis(23, 0.75), 
      limits = 0:22, 
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      breaks=seq(0,22,2), 
      limits = c(-0.5, 22.5)
    ) +
    ggplot2::theme_minimal() 
  
  if (show_by_question) {
    md + ggplot2::facet_grid(question~.)
  } else {
    md
  }
}