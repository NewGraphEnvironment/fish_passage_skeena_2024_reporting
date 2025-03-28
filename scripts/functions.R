##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}


# https://github.com/NewGraphEnvironment/fpr/issues/115
lfpr_table_cv_summary_memo <- function(dat = pscis_phase2,
                                           site = my_site,
                                           site_photo_id = my_site,
                                           font = 11,
                                           col_filter = pscis_crossing_id) {

  dat_site <- dat |> dplyr::filter({{ col_filter }} == site)
  comments <- dat_site |> dplyr::pull(assessment_comment)

  comment_label <- paste0("Comments: ", comments)

  photo_label <- paste0(
    "Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet."
    # "![](data/photos/", site_photo_id, "/crossing_all.JPG)"
  )
  photos_inserted <- paste0("![](data/photos/", site_photo_id, "/crossing_all.JPG)")


  fpr::fpr_kable(
    fpr::fpr_table_cv_detailed(dat = dat_site),
    caption_text = paste0("Summary of fish passage assessment for PSCIS crossing ", site, "."),
    booktabs = TRUE,
    scroll = FALSE,
    font = font
  ) |>
    kableExtra::add_footnote(
      label = c(comment_label, photo_label, photos_inserted),
      notation = "none"
    )
}

