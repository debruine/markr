#' Category vs Mark Graph
#'
#' \code{cat_mark} create a graph of the category sums versus the marks
#'
#' @param marking list containing a dataframe (marks) and eval
#' 
#' @return ggplot
#' @examples
#' cat_mark(marking_example)
#' @export

cat_mark <- function(marking) {
  cats <- dplyr::pull(marking$eval, q)
  
  check_scores <- marking$marks %>%
    dplyr::filter(!is.na(mark)) %>%
    tidyr::gather("Category", "cat_label", cats) %>%
    dplyr::mutate(score = mean(cat_label) - cat_label) %>%
    dplyr::group_by(id, mark) %>%
    dplyr::summarise(mean_score = mean(score)) 
  
  check_scores %>%
    dplyr::group_by(mark, mean_score) %>%
    dplyr::mutate(n = n()) %>%
    ggplot2::ggplot(ggplot2::aes(mark, mean_score, size = n, color = n)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks=0:22) +
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(question~.)
}