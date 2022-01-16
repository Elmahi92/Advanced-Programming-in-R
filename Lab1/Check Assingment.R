install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")
library(markmyassignment)
lab_path <-"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
rm(list=ls())
mark_my_assignment()

