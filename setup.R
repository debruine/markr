# edit package
setwd("~/markr")
#devtools::use_data(marking_example, overwrite = TRUE)
devtools::document()
devtools::load_all()
#devtools::check()


# to build the package for installation
setwd("~/")
devtools::install("markr")





## run this to generate a report and individual feedback
library("markr")

setwd("~/markr/data-raw/helena")
m <- load_marks("marking1", assign_id = "ATEP")
#View(m$marks)

# generate second marking list
second_mark(m, show_first_mark = F)

check_second_marking(m)

# make a report (keeps failing the first time, just try again)
make_report(m) 

# create feedback sheets and save in fb_dir
class_name = "ATEP Demo"
assignment_name = ""
make_feedback(
  m, 
  template = "feedback_template.Rmd",
  filename = "feedback.pdf"
)





# remember to set your working directory!
setwd("~/markr/data-raw/example")
marking_example <- load_marks(
  "787420_marking.csv",
  "submissions",
  "787420_eval.csv",
  "ATEP 1"
)

# generate second marking list
second_mark(m)

# make a report 
make_report(m) 

# create feedback sheets and save in fb_dir
make_feedback(m, 
  template = "787420_template.Rmd",
  filename = "feedback.pdf"
)
