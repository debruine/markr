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

# remember to set your working directory!
setwd("~/markr/data-raw/example")

m2 <- load_marks(
  markfile = "787420_marking.csv",
  dir = "submissions",
  evalfile = "787420_eval.csv",
  assign_id = "787420"
)

# generate second marking list
second_mark(m2)

# make a report 
make_report(m2) 

# create feedback sheets and save in fb_dir
make_feedback(
  m2, 
  template = "787420_template.Rmd",
  filename = "feedback.pdf"
)






# multiple-question exam demo

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





