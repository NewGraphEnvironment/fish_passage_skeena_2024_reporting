# Export pscis data to csv for cut and paste into PSCIS submission spreadsheet

# Currently updated for Peace 2024

path <- "~/Projects/gis/sern_peace_fwcp_2023/data_field/2024/form_pscis_2024.gpkg"
dir_backup = "data/backup/"

# read in cleaned form from Q after review and finalization
# first we back up the gpkg in the repo and update the coordinate columns in the gpkg in QGIS
pscis_export_raw <- fpr::fpr_sp_gpkg_backup(
  dir_backup = dir_backup,
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
  # csv is versioned
  write_to_rdata = TRUE,
  return_object = TRUE)


## Check for duplicates
dups <- pscis_export_raw |>
  group_by(site_id) |>
  filter(n() > 1)

# No duplicate sites for peace 2024


# Not used in Peace 2024
# ## Adding in spell-checked comments
# clean_comments <-  read.csv('data/inputs_extracted/clean_comments.csv')
#
# pscis_export_raw_clean <-  dplyr::left_join(pscis_export_raw |>
#                                               select(-assessment_comment),
#                                             clean_comments,
#                                             join_by(site_id == my_crossing_reference)) |>
#   relocate(assessment_comment, .after = my_priority)


### IF RUN, must update code below with pscis_export_raw_clean



# Not used in Peace 2024
# ## Fix time zone, issue here https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/18
# pscis_export_raw_clean2 <- pscis_export_raw |>
# dplyr::mutate(date_time_start_raw = date_time_start,
#               date_time_start = lubridate::force_tz(date_time_start_raw, tzone = "America/Vancouver"),
#               date_time_start = lubridate::with_tz(date_time_start, tzone = "UTC"))


### IF RUN, must update code below with pscis_export_raw_clean2




## prep for cut and paste to csvs by subsetting columns to those in spreadsheet
pscis_export <- pscis_export_raw %>%
  # Get time to append to comments
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
         date = lubridate::date(date_time_start),
         time = hms::as_hms(date_time_start)) |>
  # append moti ids to comments, differentiate between highway major structure, and add time to end
  dplyr::mutate(assessment_comment = dplyr::case_when(
    moti_chris_culvert_id > 1000000 ~ paste0(assessment_comment, ' MoTi chris_culvert_id: ', moti_chris_culvert_id),
    moti_chris_culvert_id < 1000000 ~ paste0(assessment_comment, ' MoTi chris_hwy_structure_road_id: ', moti_chris_culvert_id),
    TRUE ~ assessment_comment)) |>
  dplyr::mutate(assessment_comment = dplyr::case_when(moti_chris_culvert_id2 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id2), TRUE ~ assessment_comment)) |>
  dplyr::mutate(assessment_comment = dplyr::case_when(moti_chris_culvert_id3 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id3), TRUE ~ assessment_comment)) |>
  # add time to end
  dplyr::mutate(assessment_comment = paste0(assessment_comment, '. ', time)) |>
  # ditch time column
  dplyr::select(-time) |>
  # add in pscis phase
  dplyr::mutate(pscis_phase = case_when(
    assess_type_phase1 == "Yes" ~ "phase 1",
    assess_type_phase2 == "Yes" ~ "phase 2",
    assess_type_reassessment == "Yes" ~ "reassessment")) |>
  # only select columns from the template object, as well as site_id, pscis_phase, and date_time_start
  dplyr::select(
    dplyr::any_of(names(fpr::fpr_xref_template_pscis())),
    site_id,
    date_time_start,
    pscis_phase
  ) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result) %>%
  sf::st_drop_geometry() %>%
  # arrange by phase so easy to copy/paste into correct spreadsheet
  dplyr::arrange(pscis_phase,
                 crossing_type,
                 continuous_embeddedment_yes_no,
                 backwatered_yes_no,
                 crew_members,
                 date_time_start)

# write to the `imports_extracted` directory. This is data we import to the project but they are extracted from other places.
dir.create("data/inputs_extracted")
pscis_export %>%
  readr::write_csv('data/inputs_extracted/pscis_export_submission.csv', na='')


## For Peace 2024 we have phase 1, phase 2, and reassessment data


# Now copy/paste all phase 1, phase 2, and reassessment data from `pscis_export_submission.csv` into separate pscis spreadsheets
# (For example: pscis_phase1.xlsm, pscis_phase2.xlsm, pscis_reassessments.xlsm) which can be downloaded here
# https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/aquatic-habitat-management/fish-passage/fish-passage-technical/assessment-projects.


# After the data has been copy/pasted from `pscis_export_submission.csv` into the appropriate pscis spreadsheet, we can
# calculate the recommended replacement structure, type, and size using `sfpr_structure_size_type`.

# `fpr_import_pscis_all` reads and backups all pscis spreadsheets but you only need to run `sfpr_structure_size_type` for the
# assessment phases you have.
pscis_list <- fpr_import_pscis_all()
pscis_phase1 <- pscis_list %>% pluck('pscis_phase1')
pscis_phase2 <- pscis_list %>% pluck('pscis_phase2')
pscis_reassessments <- pscis_list %>% pluck('pscis_reassessments')

# `sfpr_structure_size_type` burns the structure, type, and size to a csv called `str_type_{your_assessment_phase}.csv`
sfpr_structure_size_type(pscis_phase1)
sfpr_structure_size_type(pscis_phase2)
sfpr_structure_size_type(pscis_reassessments)

# Finally, copy/paste the data from `str_type_pscis1.csv`, `str_type_pscis2.csv`, and `str_type_pscis_reassessments.csv`
# back into the appropriate pscis spreadsheet

