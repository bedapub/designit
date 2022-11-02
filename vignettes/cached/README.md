# How to add precomplied vignette children

* name it `_something.Rmd` (starting with an underscore, otherwise `pkgdown`
  renders it even if in `.Rbuildignore`)
* add the following header:
    - output is `html_fragment`
    - `title` is mandatory
    - `knit` is needed for RStudio to knit to the correct place (they cannot be
      in a subdirectory, because then CHECK fails)
  ````
  ---
  title: "Title of vignette child"
  output: html_fragment
  knit: (\(input, ...) rmarkdown::render(input, output_dir = "vignettes"))
  ---
  ````
* knit child (either in RStudio or with `rmarkdown::render()`)
* add link to child to vignette
  ````
  ```{r, child="_something.html"}
  ```
  ````
