#devtools::install_github("debruine/markr")
library("markr")

# multiple-question exam demo

setwd("~/markr/data-raw/helena")
m <- load_marks("marking1", moodlefile = "moodle.xlsx")
#View(m$marks)

# generate second marking list
second_mark(m, show_first_mark = TRUE, pass_min = 9)

# fill in second marks before continuing

m2 <- check_second_marking(m)
#View(m2$marks)

m3 <- update_marks(m2, "updates.csv")
#View(m3$marks)

# saves by default as year_class_assignment_all_marks.csv
save_marks(m3)

# make a report (check help for other show options)
make_report(
  m3,
  template = "report_template.Rmd",
  show = c("mark2", "mark_dist") 
)

# create feedback sheets and save in fb_dir
make_feedback(
  m3, 
  template = "feedback_template.Rmd",
  filename = "feedback.pdf"
)
