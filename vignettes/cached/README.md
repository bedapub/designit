# How to add precomplied vignette children

* name it `_something.Rmd` (starting with an underscore, otherwise `pkgdown`
  renders it even if in `.Rbuildignore`)
* add the following header:
    - output is `html_fragment`
    - `title` is mandatory
    - `knit` is needed for RStudio to knit to the correct place
  ````
  ---
  title: "Title of vignette child"
  output: html_fragment
  knit: (\(input, ...) rmarkdown::render(input, output_dir = here::here("vignettes/cached")))
  ---
  ````
* knit child to HTML fragment (either in RStudio or with `rmarkdown::render()`)
* in the main vignette add link to child
  ````
  ```{r, child="cached/_something.html"}
  ```
  ````
