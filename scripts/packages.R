# ensure pak is installed and up to date from CRAN
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
} else {
  # Only run this if an update is needed
  current <- packageVersion("pak")
  latest <- package_version(available.packages()["pak", "Version"])
  if (current < latest) {
    pak::pak("pak")  # uses pak to update itself = no popup
  }
}

pkgs_cran <- c(
  'tidyverse',
  'knitr',
  'bookdown',
  'rmarkdown',
  'pagedown',
  'RPostgres',
  'sf',
  "ggdark",
  "kableExtra"
)

pkgs_gh <- c(
  "newgraphenvironment/fpr",
  "newgraphenvironment/ngr",
  "newgraphenvironment/staticimports",
  "lucy-schick/fishbc@updated_data",
  "poissonconsulting/readwritesqlite", #https://github.com/poissonconsulting/readwritesqlite/issues/47
  "paleolimbot/rbbt"
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
