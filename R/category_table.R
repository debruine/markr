#' Format Category Table
#'
#' \code{category_table} returns a latex-formatted table
#' from an existing 1-row dataframe (ind_fb) with specified 
#' evaluation categories (eval) and category lables (cats).
#'
#' @param ind_fb 1-row data frame of assignment evaluations
#' @param eval data frame of evaluation labels (q) and descriptions (Category)
#' @param cats list of labels to map onto evaluations
#' 
#' @return A latex-formatted table
#' @examples
#' marking_example$marks %>% 
#'   slice(1) %>% 
#'   category_table(
#'     marking_example$eval, 
#'     c("Excellent", "Good", "Poor")
#'   )
#' @export

category_table <- function(ind_fb, eval, cats) {
  ind_fb %>%
    dplyr::slice(1) %>%
    dplyr::select(eval$q) %>%
    tidyr::gather("q", "grade", eval$q) %>%
    dplyr::mutate(grade = cats[grade]) %>%
    dplyr::add_row(q = cats, grade = cats) %>%
    dplyr::mutate(x = "*") %>%
    tidyr::spread(grade, x) %>%
    dplyr::filter(!(q %in% cats)) %>%
    dplyr::mutate_all(dplyr::funs(replace(., is.na(.), ""))) %>%
    dplyr::left_join(eval, by = "q") %>%
    dplyr::select(Category, cats) %>%
    pander::pandoc.table(
      justify = c("left", rep("center", length(cats))),
      split.cells = c((60-(5*length(cats))), rep(5, length(cats)))
    )
}