source("scripts/packages.R")

# Let's just make the HTML tables when we need to


# bcfishpass HTML to link from map ----------------------------------------

fs::file_delete("docs/sum/bcfp")  # Erase the file and start over every time
fs::dir_create("docs/sum/bcfp", recurse = TRUE)


bcfishpass |>
  sf::st_drop_geometry() |>
  # Need to determine which sites have no bcfishpass info and exclude those so purrr::map does not bail on us
  dplyr::filter(stream_crossing_id %in% (pscis_all |> dplyr::pull(pscis_crossing_id))) |>
  dplyr::pull(stream_crossing_id) |>
  purrr::map(fpr::fpr_table_bcfp_html, scroll = FALSE)


# culvert HTML to link from map ----------------------------------------
# Build all the cv tables for the interactive map -

fs::file_delete("docs/sum/cv")  # Erase the file and start over every time
fs::dir_create("docs/sum/cv", recurse = TRUE)

# Be sure to run options(knitr.kable.XXX = '--') in the setup chunk of index.Rmd first
pscis_all |>
  dplyr::distinct(pscis_crossing_id) |>
  # filter(source %ilike% 'phase2|>
  dplyr::pull(pscis_crossing_id) |>
  purrr::map(fpr::fpr_table_cv_html)
