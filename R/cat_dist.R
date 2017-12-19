#' Category Distribution Graph
#'
#' \code{cat_dist} create a graph of the category distribution
#'
#' @param marking list containing a dataframe (marks)
#' @param rating_levels list of rating levels (default 1:3)
#' 
#' @return ggplot
#' @examples
#' cat_dist(marking_example, rating_levels = 1:4)
#' @export

cat_dist <- function(marking, rating_levels = 1:3) {
  if (!is.data.frame(marking$eval)) return("Categories are not defined.")
  
  cats <- dplyr::pull(marking$eval, q)
  
  marking$marks %>%
    dplyr::filter(!is.na(mark)) %>%
    tidyr::gather("Category", "Rating", cats) %>%
    dplyr::mutate(
      Rating = factor(Rating, levels = rating_levels),
      Category = factor(Category, levels = cats)   
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(Rating, fill = Category)) +
    ggplot2::facet_grid(Category~.) + 
    viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.75) +
    ggplot2::theme_minimal()
}