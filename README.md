
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
is in the form of a bookdown, available
[here](https://jacobbien.github.io/flowkernel-project/create/).

To modify the code in this R package, modify the `.Rmd` files in the
`create-flowkernel/` directory and then (from an R session in this
directory) run the following:

``` r
litr::render("create-flowkernel/index.Rmd")
fs::dir_copy("create-flowkernel/_book", "docs/create", overwrite = TRUE)
fs::dir_delete("create-flowkernel/_book")
fs::dir_copy("create-flowkernel/flowkernel", "flowkernel", overwrite = TRUE)
fs::dir_delete("create-flowkernel/flowkernel")
fs::dir_delete("create-flowkernel/_main_files/")
```

To install the latest version of `litr`, run

``` r
remotes::install_github("jacobbien/litr-project", subdir = "litr")
```
