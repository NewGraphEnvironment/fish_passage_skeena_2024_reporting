keys = rbbt::bbt_detect_citations(
  list.files(
    pattern = "*.Rmd")
)

path_bib <- system.file("extdata", "NewGraphEnvironment.bib", package = "xciter")


keys_missing <- xciter::xct_bib_keys_missing(
  path_bib = path_bib,
  citations = rbbt::bbt_detect_citations(
    list.files(
      pattern = "*.Rmd")
  )
)

keys_matched <- xciter::xct_keys_guess_match(
  keys_missing,
  keys_bib = xciter::xct_bib_keys_extract(path_bib),
  # stringdist_method = "osa",
  stringdist_threshold = 25,
  no_match_rows_include = TRUE
) |>
  dplyr::arrange(key_missing)

# we are looking here
file_list <- fs::dir_ls(glob = "*.Rmd")

# they look good so we replace all at once without asking
purrr::map2(
  .x = keys_matched$key_missing,
  .y = keys_matched$key_missing_guess_match,
  ~ ngr::ngr_str_replace_in_files(text_current = .x, text_replace = .y, files = file_list, ask = FALSE)
)

ngr::ngr_str_replace_in_files("tip_header","tip_flag",files = file_list)
