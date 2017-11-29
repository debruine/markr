#' Generate Second Marking List
#'
#' \code{second_mark} determines what assessments need to be second marked 
#' and bundles files into a folder with the second marking sheet
#'
#' @param marking list containing a dataframe (marks) and moodle participant directory (dir)
#' @param dir directory name to save second marking in
#' @param remove_file filename to remove from second marking directory
#' @param pass_min minimum passing mark; all lower marks are second marked (default 9)
#' 
#' @return none
#' @examples
#' second_mark(marking, "data-raw/example/submissions")
#' @export

second_mark <- function(marking, 
                        dir = "second_marking", 
                        remove_file = "feedback.pdf",
                        pass_min = 9 # minimum mark to pass (9 UG, 12 PG)
) {
  fb <- marking$marks
  fb_dir <- marking$dir
  all_dirs <- list.dirs(fb_dir) 
  assign_id <- fb$assign_id[1]
  
  # set seed to make this always the same for each assignment
  # generate an integer from the assign_id
  seed <- assign_id %>% utf8ToInt() %>% sum()
  set.seed(seed)
  
  # calculate how many to second mark
  # 10% of marks or 10, whichever is higher (or all if < 10)
  second_n <- max(10, ceiling(nrow(fb)/10))
  if (nrow(fb) <= second_n) {
    message(paste("Selecting all", nrow(fb), "assessments to second mark"))
    to_second_mark <- dplyr::pull(fb, id)
  } else {
    # select all fails
    fails <- fb %>%
      dplyr::filter(mark < pass_min) %>%
      dplyr::pull(id)
    
    # get ~1/3 low, mid and high marks
    low_high_n <- floor((second_n - length(fails))/3)
    mid_n <- second_n - length(fails) - 2*low_high_n
    
    f <- fb %>%
      dplyr::filter(mark >= pass_min) %>%
      dplyr::arrange(mark, id) %>%
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
      dplyr::pull(id) %>% 
      base::sample(low_high_n)
    mids <- f %>% 
      dplyr::filter(band == "med") %>%
      dplyr::pull(id) %>% 
      base::sample(mid_n)
    highs <- f %>% 
      dplyr::filter(band == "high") %>%
      dplyr::pull(id) %>% 
      base::sample(low_high_n)
    
    to_second_mark <- c(fails, lows, mids, highs)
  }
  
  # create directory
  dir.create(dir, showWarnings = FALSE)
  
  second_marks <- tibble::tibble(
    id = to_second_mark,
    assign_id = assign_id,
    mark2 = 0
  ) %>%
    dplyr::arrange(id)
  
  # create marking file
  readr::write_csv(second_marks, paste0(dir, "/", assign_id, "_second_marking.csv"))
  
  # copy folders to 2nd marking folder
  if (is.na(fb_dir)) {
    print("No files were copied into the second marking folder")
  } else if (!dir.exists(fb_dir)) {
    message(paste("The feedback directory", fb_dir, 
                "doesn't exist, so no files were copied into the second marking folder"))
  } else {
    for (id in to_second_mark) {
      pdir_n <- grep(id, all_dirs)
      if (length(pdir_n) == 1) {
        # get the dir that contains the id
        pdir <- all_dirs[pdir_n]
      } else if (length(pdir_n) == 0) {
        message(paste("No directory found for", id))
        pdir <- c()
      } else if (length(pdir_n) > 1) {
        message(paste(length(pdir_n), "directories copied for", id))
        pdir <- all_dirs[pdir_n]
      }
      file.copy(from = pdir, to = dir,
                overwrite = TRUE, 
                recursive = TRUE, 
                copy.mode = TRUE)
      
      # remove feedback files
      fb_file <- paste0(pdir, "/", remove_file)
      if (remove_file != "" & file.exists(fb_file)) {
        file.remove(fb_file)
      }
    }
  }
}
