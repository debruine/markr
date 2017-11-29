#' Linear Model of Marks
#'
#' \code{predict_mark} generate a linear model of marks from category ratings
#'
#' @param marking list containing a dataframe (marks)
#' 
#' @return lm() summary
#' @examples
#' predict_mark(marking_example)
#' @export

predict_mark <- function(marking) {
  cats <- dplyr::pull(marking$eval, q)
  
  model_scores <- marking$marks %>%
    dplyr::filter(!is.na(mark)) %>%
    tidyr::gather("Category", "cat_label", cats) %>%
    dplyr::mutate(score = mean(cat_label) - cat_label) %>%
    dplyr::select(part_id, mark, Category, score) %>%
    tidyr::spread(Category, score)  
  
  formula <- stats::as.formula(paste("mark ~", paste(cats, collapse="+")))
  model <-  stats::lm(formula, data = model_scores)
  
  summary(model)
}