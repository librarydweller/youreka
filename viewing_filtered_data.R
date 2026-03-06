#viewing filtered data
#core material, nanoparticle size, zeta potential, tumor size, biodistribution
#tumor, heart, liver, spleen, lungs, kidney

library(haven)

clean_data <- read.csv("clean_youreka_set.csv")
View(replace(clean_data, is.na(clean_data), ""))
