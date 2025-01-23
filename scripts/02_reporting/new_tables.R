## new tables.R

## Params -------------------------------------------------

path_form_fiss_site <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_fiss_site_2024.gpkg')



## Load PSCIS data -------------------------------------------------

# What we should be doing here is just reading in form_pscis_2024.gpkg which should have site elevations, barrier scores,
# and replacement structure, size, type, see issue: https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/56

# import data and build tables we for reporting
pscis_list <- fpr::fpr_import_pscis_all()
pscis_phase1 <- pscis_list |> pluck('pscis_phase1')
pscis_phase2 <- pscis_list |> pluck('pscis_phase2') |>
  arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list |> pluck('pscis_reassessments')
pscis_all_prep <- pscis_list |>
  bind_rows()



# WHY DO WE NEED THIS, BECAUSE:
# It looks like this object is used in multiple other parts of this script.
# Take note of how it is used as we continue purging this script

# this doesn't work till our data loads to pscis
pscis_all <- left_join(
  pscis_all_prep,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) |>
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ as.numeric(stream_crossing_id),
    TRUE ~ pscis_crossing_id
  )) |>
  arrange(pscis_crossing_id)




## Load habitat data -------------------------------------------------

form_fiss_site_raw <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form_fiss_site,
  dir_backup = "data/backup/",
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  return_object = TRUE,
  col_easting = "utm_easting",
  col_northing = "utm_northing")


## Build hab_site object for fpr_my_habitat_info() -------------------------------------------------

hab_site <- form_fiss_site



# ------------ make priority spreadsheet ----------------------------------------------

# # spreadsheet to build for input includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())
# # thing is that we don't really have the fish info
#
# # grab the field form data
# dir_gis <- 'sern_skeena_2023'
#
# ## Import the raw form_fiss_2023.gpkg and update the local_name with the pscis values
# form_fiss_site_raw <- fpr::fpr_sp_gpkg_backup(
#   path_gpkg = paste0("~/Projects/gis/", dir_gis, '/data_field/2023/form_fiss_site_2023.gpkg'),
#   update_utm = TRUE,
#   update_site_id = FALSE,
#   write_back_to_path = FALSE,
#   write_to_csv = FALSE,
#   write_to_rdata = FALSE,
#   return_object = TRUE,
#   col_easting = "utm_easting",
#   col_northing = "utm_northing"
# ) |>
#   # keep sites that end with us or us# only
#   # dplyr::filter(stringr::str_detect(local_name, 'us\\d?$')) |>
#   tidyr::separate(local_name, c("site", "location", "ef"), sep = "_", remove = FALSE) |>
#   sf::st_drop_geometry() |>
#   mutate(site = as.numeric(site)) |>
#   # make a new column for the time as is with different name then mutate to PST
#   # we don't need the new column but will leave here for now so we can visualize and confirm the time is correct
#   mutate(date_time_start_raw = date_time_start,
#          date_time_start = lubridate::force_tz(date_time_start_raw, tzone = "America/Vancouver"),
#          date_time_start = lubridate::with_tz(date_time_start, tzone = "UTC"))
# # turn on line below and add pipe aboveto visualize and confirm the times are correct
# # looks like site 8478 imports raw represented in PDT so is converted incorrectly. not sure why and not related
# # to method of time conversion at all I (al) don't think though.
# # select(local_name, date_time_start, date_time_start_raw)
#
# # we need to swap in the PSCIS IDs for the `site` IDs that are modelled_crossing_ids
# form_fiss_site_raw <- left_join(
#   form_fiss_site_raw,
#   xref_pscis_my_crossing_modelled,
#   by = c('site' = 'external_crossing_reference')
# ) |>
#   mutate(site = case_when(
#     !is.na(stream_crossing_id) ~ stream_crossing_id,
#     T ~ site
#   )) |>
#   tidyr::unite(local_name, site, location, ef, sep = "_", na.rm = TRUE)
#
#
# # Function to replace empty character and numeric values with NA
replace_empty_with_na <- function(x) {
  if(is.character(x) && length(x) == 0) return(NA_character_)
  if(is.numeric(x) && length(x) == 0) return(NA_real_)
  return(x)
}

hab_priority_prep <- form_fiss_site_raw |>
  select(stream_name = gazetted_names,
         local_name,
         date_time_start) |>
  tidyr::separate(local_name, c("site", "location", "ef"), sep = "_", remove = FALSE) |>
  dplyr::rowwise() |>
  # lets make the columns with functions
  mutate(
    crew_members = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = crew_members)),
    # length_surveyed = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name,col_pull = site_length)),
    hab_value = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = habitat_value_rating)),
    # need this column
    priority = NA_character_,
    # priority = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = priority)),
    ## first we grab hand bombed estimate from form so that number stands if it is present
    # us_habitat_m = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = us_habitat_m)),
    # species_known = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = species_known)),
    # gps_waypoint_number = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = gps_waypoint_number)),
    comments = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site_raw, site = local_name, col_filter = local_name, col_pull = comments)),
    upstream_habitat_length_m = list(fpr::fpr_my_bcfishpass(site = site, col_pull = st_rearing_km, round_dig = 4)),
    species_codes = list(fpr::fpr_my_bcfishpass(site = site, col_pull = observedspp_upstr)),
    # if the hand bombed estimate is present we use that
    # upstream_habitat_length_m = case_when(
    #   !is.na(us_habitat_m) ~ us_habitat_m,
    #   T ~ upstream_habitat_length_m
    # ),
    # species_codes = case_when(
    #   !is.na(species_known) ~ species_known,
    #   T ~ species_codes
    # ),

    across(everything(), ~replace_empty_with_na(.))) |>
  dplyr::ungroup() |>
  dplyr::mutate(upstream_habitat_length_m = round(upstream_habitat_length_m)) |>
  dplyr::arrange(local_name, crew_members, date_time_start) |>
  sf::st_drop_geometry()
#
#
# # burn to csv.  This has us doing all updates in Q or programatically. may be viable... we will see
# hab_priority_prep %>%
#   readr::write_csv('data/habitat_confirmations_priorities.csv', na = '')
