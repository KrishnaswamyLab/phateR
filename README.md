
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phater

This R package provides an implementation of the [PHATE dimensionality
reduction and visualization
method](https://www.biorxiv.org/content/early/2017/12/01/120378).

For a thorough overview of the PHATE visualization method, please see
the [bioRxiv
preprint](https://www.biorxiv.org/content/early/2017/12/01/120378)

For our Python and Matlab implementations, please see
[KrishnaswamyLab/PHATE](https://github.com/KrishnaswamyLab/PHATE).

## Installation

You can install phater from GitHub with:

``` r
if (!suppressWarnings(require(devtools))) install.packages("devtools")
devtools::install_github("KrishnaswamyLab/phater")
```

## Example

This is a basic example running `phate` on a highly branched example
dataset that is included with the package. First, let’s examine it with
PCA.

``` r
library(phater)
data(tree.data)
plot(prcomp(tree.data$data)$x, col=tree.data$branches)
```

<img src="man/figures/README-example-data-1.png" width="100%" />

Now we run PHATE on the data. We’ll just go ahead and try with the
default parameters.

``` r
# runs phate
tree.phate <- phate(tree.data$data)
#> Calculating kernel...
#> Calculated kernel in 2.6 secs.
#> Calculating diffusion operator...
#> Calculated diffusion operator in 9.5 secs.
#> Calculating diffusion potential...
#> Calculated diffusion potential in 13.1 secs.
#> Embedding metric MDS...
#> Calculated MDS in 20.6 secs.
#> Embedded PHATE in 45.8 secs.
summary(tree.phate)
#> PHATE embedding
#> k = 5, alpha = NA, t = 102
#> Data: (3000, 100)
#> Embedding: (3000, 2)
```

Let’s plot the results.

``` r
# plot embedding
palette(rainbow(10))
plot(tree.phate, col = tree.data$branches)
```

<img src="man/figures/README-plot-results-1.png" width="100%" />

Good news\! Our branches look nice. However, all of the interesting
activity seems to be concentrated into one region of the plot - in this
case we should try the square root potential instead. We can also try
increasing `t` to make the structure a little clearer - in this case,
because synthetic data in unusually structured, we can use a very large
value, like 120, but in biological data the automatic `t` selection is
generally very close to ideal. Note here that if we pass our previous
result in with the argument `init`, we won’t have to recompute the
diffusion operator.

``` r
# runs phate with different parameters
tree.phate <- phate(tree.data$data, potential.method='sqrt', t=90, init=tree.phate)
#> Warning in if (class(init) != "phate") {: the condition has length > 1 and
#> only the first element will be used
#> Using precomputed kernel...
#> Using precomputed diffusion operator...
#> Calculating diffusion potential...
#> Calculated diffusion potential in 8.1 secs.
#> Embedding metric MDS...
#> Calculated MDS in 1.4 mins.
#> Embedded PHATE in 1.6 mins.
# plot embedding
palette(rainbow(10))
plot(tree.phate, col = tree.data$branches)
```

<img src="man/figures/README-adjust-parameters-1.png" width="100%" />

We can also pass the PHATE object directly to `ggplot`, if it is
installed.

``` r
if (!suppressWarnings(require(ggplot2))) install.packages("ggplot2")
#> Loading required package: ggplot2
ggplot(tree.phate, aes(x=PHATE1, y=PHATE2, color=tree.data$branches)) +
  geom_point()
```

<img src="man/figures/README-ggplot-1.png" width="100%" />

## Issues

Please let us know of any issues at the [GitHub
repo](https://github.com/KrishnaswamyLab/PHATE/issues)
