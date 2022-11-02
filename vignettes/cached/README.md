# How to add precomplied vignette children

* name it `_something.Rmd` (starting with an underscore, otherwise `pkgdown`
  renders it even if in `.Rbuildignore`)
* add the following header (`title` is needed, and output is `html_fragment`)
    - output is `html_fragment`
    - `title` is mandatory
    - `knit` is needed for RStudio to knit to the correct place
  ````
  ---
  title: "Title of vignette child"
  output: html_fragment
  knit: (\(input, ...) rmarkdown::render(input, output_dir = "vignettes/cached"))
  ---
  ````
* knit child (either in RStudio or `rmarkdown::render()`)
* add link to child to vignette
  ````
  ```{r, child="cached/_something.html"}
  ```
  ````
