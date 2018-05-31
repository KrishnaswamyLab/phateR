phateR v0.2.6
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Latest PyPI version](https://img.shields.io/pypi/v/phate.svg)](https://pypi.org/project/phate/)
[![Latest CRAN version](https://img.shields.io/cran/v/phateR.svg)](https://cran.r-project.org/package=phateR)
[![Travis CI Build](https://api.travis-ci.com/KrishnaswamyLab/phateR.svg?branch=master)](https://travis-ci.com/KrishnaswamyLab/phateR)
[![Read the Docs](https://img.shields.io/readthedocs/phate.svg)](https://phate.readthedocs.io/)
[![bioRxiv Preprint](https://zenodo.org/badge/DOI/10.1101/120378.svg)](https://www.biorxiv.org/content/early/2017/12/01/120378)
[![Twitter](https://img.shields.io/twitter/follow/KrishnaswamyLab.svg?style=social&label=Follow)](https://twitter.com/KrishnaswamyLab)
[![Github Stars](https://img.shields.io/github/stars/KrishnaswamyLab/PHATE.svg?style=social&label=Stars)](https://github.com/KrishnaswamyLab/PHATE/)

This R package provides an implementation of the [PHATE dimensionality
reduction and visualization
method](https://www.biorxiv.org/content/early/2017/12/01/120378).

For a thorough overview of the PHATE visualization method, please see
the [bioRxiv
preprint](https://www.biorxiv.org/content/early/2017/12/01/120378)

For our Python and Matlab implementations, please see
[KrishnaswamyLab/PHATE](https://github.com/KrishnaswamyLab/PHATE).

## Table of Contents

  * [Installation](#installation)
    * [Installation from CRAN and PyPi](#installation-from-cran-and-pypi)
    * [Installation with devtools and <code>reticulate</code>](#installation-with-devtools-and-reticulate)
    * [Installation from source](#installation-from-source)
  * [Tutorial](#tutorial)
  * [Issues](#issues)

## Installation

In order to use PHATE in R, you must also install the Python package.

#### Installation from CRAN and PyPi

Install `phateR` from CRAN by running the following code in R:

``` r
install.packages("phateR")
```

Install `phate` in Python by running the following code from a terminal:

``` bash
pip install --user phate
```

#### Installation with `devtools` and `reticulate`

The development version of PHATE can be installed directly from R with
`devtools`:

``` r
if (!suppressWarnings(require(devtools))) install.packages("devtools")
devtools::install_github("KrishnaswamyLab/phateR")
```

If you have the development version of `reticulate`, you can also
install `phate` in Python by running the following code in R:

``` r
devtools::install_github("rstudio/reticulate")
reticulate::py_install("phate")
```

#### Installation from source

The latest source version of PHATE can be accessed by running the
following in a terminal:

``` bash
git clone --recursive git://github.com/SmitaKrishnaswamy/PHATE.git
cd PHATE/phateR
R CMD INSTALL
cd ../Python
python setup.py install --user
```

If the `phateR` folder is empty, you have may forgotten to use the
`--recursive` option for `git clone`. You can rectify this by
running the following in a terminal:

``` bash
cd PHATE
git submodule init
git submodule update
cd phateR
R CMD INSTALL
cd ../Python
python setup.py install --user
```

## Tutorial

This is a basic example running `phate` on a highly branched example
dataset that is included with the package. First, let’s examine it with
PCA.

``` r
library(phateR)
data(tree.data)
plot(prcomp(tree.data$data)$x, col=tree.data$branches)
```

<img src="man/figures/README-example-data-1.png" width="100%" />

Now we run PHATE on the data. We’ll just go ahead and try with the
default parameters.

``` r
# runs phate
tree.phate <- phate(tree.data$data)
summary(tree.phate)
#> PHATE embedding
#> k = 15, alpha = NA, t = auto
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

Good news\! Our branches separate nicely. However, most of the
interesting activity seems to be concentrated into one region of the
plot - in this case we should try the square root potential instead. We
can also try increasing `t` to make the structure a little clearer - in
this case, because synthetic data in unusually structured, we can use a
very large value, like 200, but in biological data the automatic `t`
selection is generally very close to ideal. Note here that if we pass
our previous result in with the argument `init`, we won’t have to
recompute the diffusion operator.

``` r
# runs phate with different parameters
tree.phate <- phate(tree.data$data, potential.method='sqrt', t=200, init=tree.phate)
# plot embedding
palette(rainbow(10))
plot(tree.phate, col = tree.data$branches)
```

<img src="man/figures/README-adjust-parameters-1.png" width="100%" />

We can also pass the PHATE object directly to `ggplot`, if it is
installed.

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.4.4
ggplot(tree.phate, aes(x=PHATE1, y=PHATE2, color=tree.data$branches)) +
  geom_point()
```

<img src="man/figures/README-ggplot-1.png" width="100%" />

## Issues

Please let us know of any issues at the [GitHub
repo](https://github.com/KrishnaswamyLab/phateR/issues)
