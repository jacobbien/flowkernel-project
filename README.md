
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `flowkernel` project

<!-- badges: start -->
<!-- badges: end -->

This repository contains the `flowkernel` R package, which can be
installed as follows:

``` r
remotes::install_github("jacobbien/flowkernel-project", subdir = "flowkernel")
```

It is implemented using literate programming with
[litr](https://jacobbien.github.io/litr-project/). Thus, the source code
is in the form of a bookdown, available [here](_book/index.html). Note:
Until this repo is made public, the best way to view the bookdown is to
first download the repo and then open `_bookdown/index.html` in a
browser locally.

To modify the code in this R package, modify the `.Rmd` files in this
directory and then (from an R session in this directory) run the
following:

``` r
litr::render("index.Rmd")
```

To install the latest version of `litr`, run

``` r
remotes::install_github("jacobbien/litr-project", subdir = "litr")
```
