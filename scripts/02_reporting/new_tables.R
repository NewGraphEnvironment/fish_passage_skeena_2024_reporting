## new tables.R

## Params -------------------------------------------------

# path to form_pscis_2024
path_form_pscis <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_pscis_2024.gpkg')

# path to NEW `form_fiss_site_2024` made from `0205_fiss_extract_inputs.Rmd`
path_form_fiss_site <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_fiss_site_2024.gpkg')

# Onedrive path to the fish data with the pit tags joined.
path_onedrive_tags_joined <-  fs::path('/Users/lucyschick/Library/CloudStorage/OneDrive-Personal/Projects/2024_data/fish/fish_data_tags_joined.csv')

# specify which project data we want. for this case `2024-073-sern-peace-fish-passage`
project = "2024-072-sern-skeena-fish-passage"

## Load PSCIS data -------------------------------------------------

# What we should be doing here is just reading in form_pscis_2024.gpkg which should have site elevations, barrier scores,
# and replacement structure, size, type, see issue: https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/56

# read in form_pscis
form_pscis <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form_pscis,
  dir_backup = "data/backup/",
  update_utm = TRUE,
  update_site_id = TRUE, ## This now also checks for duplicates
  write_back_to_path = FALSE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE)



# For now, import data and build tables we for reporting
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
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  col_easting = "utm_easting",
  col_northing = "utm_northing")



## Load fish data -------------------------------------------------

fish_data_complete <- readr::read_csv(file = path_onedrive_tags_joined) |>
  janitor::clean_names() |>
  #filter for skeena 2024
  dplyr::filter(project_name == project)


## Build hab_site object for fpr_my_habitat_info() -------------------------------------------------

hab_site <- form_fiss_site_raw



# Bcfishpass modelling table setup for reporting --------------------------

xref_bcfishpass_names <- fpr::fpr_xref_crossings



# Read priority spreadsheet ----------------------------------------------

# spreadsheet that includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())

# read in the object
habitat_confirmations_priorities <- readr::read_csv(
  file = "data/habitat_confirmations_priorities.csv")



## Make objects to display fish data ----------------------------------------------

tab_fish_summary <- fish_data_complete |>
  tidyr::separate(local_name, into = c("site_id", "location", "ef")) |>
  dplyr::mutate(site_id = paste0(site_id, "_", location)) |>
  dplyr::group_by(site_id,
                  ef,
                  sampling_method,
                  species) |>
  dplyr::summarise(count_fish = n()) |>
  dplyr::arrange(site_id, species, ef)




