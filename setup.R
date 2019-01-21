# edit package
setwd("~/rproj/markr")
#devtools::use_data(marking_example, overwrite = TRUE)
devtools::document()
devtools::load_all()
devtools::check()


# to build the package for installation
setwd("~/rproj/")
devtools::install("markr")




#devtools::install_github("debruine/markr")
## run this to generate a report and individual feedback
library("markr")

# multiple-question exam demo

setwd("~/markr/data-raw/helena")
m <- load_marks("marking1", moodlefile = "moodle.xlsx")
#View(m$marks)

# generate second marking list
#second_mark(m, show_first_mark = TRUE, pass_min = 9)

m2 <- check_second_marking(m)
#View(m2$marks)

m3 <- update_marks(m2, "updates.csv")
#View(m3$marks)

save_marks(m3)

# make a report
make_report(m3) 

# create feedback sheets and save in fb_dir
make_feedback(
  m3, 
  template = "feedback_template.Rmd",
  filename = "feedback.pdf"
)







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






