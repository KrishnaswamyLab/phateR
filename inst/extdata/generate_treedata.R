pyphate <- reticulate::import("phate")

# generate DLA tree
tree <- pyphate$tree$gen_dla(n_dim=100L, n_branch=10L, branch_length=300L, rand_multiplier=2L, seed=37L, sigma=4L)
tree.data <- list(data=tree[[1]],
                  branches=as.factor(tree[[2]]))

usethis::use_data(tree.data, overwrite=TRUE)
