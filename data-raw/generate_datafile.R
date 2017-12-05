tree.data <- read.csv("data-raw/TreeData.csv", header = FALSE)
tree.data$branches <- tree.data$V61
tree.data$V61 <- NULL

usethis::use_data(tree.data)
