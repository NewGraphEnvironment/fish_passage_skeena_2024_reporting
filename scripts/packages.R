# install.packages('pak')

pkgs_cran <- c(
  'tidyverse',
  'knitr',
  'bookdown',
  'rmarkdown',
  'pagedown',
  'readwritesqlite',
  'RPostgres',
  'sf'
)

pkgs_gh <- c(
  "newgraphenvironment/fpr",
  # watch for issues in the future with this particular pin to deal with black captions
  # https://github.com/NewGraphEnvironment/mybookdown-template/issues/50
  "haozhu233/kableExtra@a9c509a"
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)


# install or upgrade all the packages with pak
# install or upgrade all the packages with pak
if(params$update_packages){
  lapply(pkgs_all, pak::pkg_install, ask = FALSE)
}

# load all the packages
pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)
