
source('scripts/packages.R')

# Paths ------------------------------------------------------
# Pit tag data for ALL years is currently being stored on OneDrive .
path_tag <- fs::path('/Users/lucyschick/Library/CloudStorage/OneDrive-Personal/Projects/2024_data/fish/tag_01_05.csv')

# Raw fish data stored in Onedrive
path_fish <-  fs::path('/Users/lucyschick/Library/CloudStorage/OneDrive-Personal/Projects/2024_data/fish/fish_data_raw.xlsx')

# path for form_fiss_site geopackage
path_form_fiss_site <- fs::path('~/Projects/gis/sern_peace_fwcp_2023/data_field/2024/form_fiss_site_2024.gpkg')

# Onedrive path where to store the fish data with the pit tags joined.
path_onedrive_tags_joined <-  fs::path('/Users/lucyschick/Library/CloudStorage/OneDrive-Personal/Projects/2024_data/fish/fish_data_tags_joined.csv')

# Repo path to individual fish data ready to c/p into `step_3_individual_fish_data`
path_repo_fish_data_ind <-  fs::path('data/inputs_raw/fish_data_ind.csv')

# Repo path to individual fish data ready to c/p into `step_2_fish_coll_data`
path_repo_fish_data_coll <-  fs::path('data/inputs_raw/fish_data_coll.csv')

# specify which project data we want. for this case `2024-073-sern-peace-fish-passage`
project = "2024-073-sern-peace-fish-passage"



# Pit Tags ------------------------------------------------------
# combining pit tag data to individual fish data so that we can copy and paste directly into submission template

# import the pit tag csv
# tag_01_05 does not have a column name so for that reason the call to read_csv needs to be different (change col_names to F for that file) and
# the column name will default to X1.
pit_tag <- readr::read_csv(path_tag, col_names = F) |>
  #separate the pit tag out from the rest of the info in the pit tag csv
  # https://stackoverflow.com/questions/66696779/separate-by-pattern-word-in-tidyr-and-dplyr
  tidyr::separate(col=X1, into=c('date', 'tag_id'), sep='\\s*TAG\\s*') |>
  tibble::rowid_to_column() |>
  dplyr::filter(str_like(date, '%2024%'))



# Read and clean the raw fish data
fish <- readxl::read_xlsx(path_fish, sheet = "fish_data") |>
  # remove the dates added by excel, they are wrong. We only want the time segments
  mutate(across(c(site_start_time, site_end_time,segment_start_time, segment_end_time, photo_time_start, photo_time_end),
                ~ format(., "%H:%M:%S")))


#join fish csv with pit tag csv based on tag row ID |>
fish_data_tags <- dplyr::left_join(fish,
                              pit_tag |>
                                dplyr::select(rowid, tag_id),
                              by = c("row_id" = "rowid")) |>
  # arrange columns
  dplyr::mutate(pit_tag_id = tag_id) |>
  dplyr::select(-tag_id) |>
  dplyr::relocate(row_id, .after = pit_tag_id) |>
  # add a period, a space and the row number to the pit tag to go in the comments to make it easy to pull anything out we want later
  dplyr::mutate(comments = case_when(
    !is.na(pit_tag_id) ~ paste0(comments,". Pit Tag ID: ", pit_tag_id, ". Row ID: ", row_id, ". "),
    T ~ comments))



# select a subsample of fish (lets go 15% since the sample size is small) to review manually to be sure the
# pit tags match which fish they go with
# set seed for reproducible sample - try running it again without setting the seed immediately before and see how it differs
set.seed(1234)

qa <- fish_data_tags |>
  filter(!is.na(row_id)) |>
  slice_sample(prop = 0.15) |>
  select(local_name, project_name, row_id, pit_tag_id, length, weight) |>
  arrange(row_id)



# burn the csv to the repo for cut and paste and to OneDrive for backup
fish_data_tags |>
  readr::write_csv(path_onedrive_tags_joined,
                   na = "" )




# Fish Data step 3 ------------------------------------------------------

# Prepare the fish data for copy paste into `step_3_individual_fish_data` of the habitat confirmations spreadsheet.

# read in the data and filter to contain data from the correct project
fish_data_complete <- readr::read_csv(file = path_onedrive_tags_joined) |>
  janitor::clean_names() |>
  #filter for peace 2024
  dplyr::filter(project_name == project)

# cross reference with step 1 of hab con sheet to get reference numbers
ref_names <- left_join(
  fish_data_complete,
  fpr_import_hab_con(backup = F, row_empty_remove = T, col_filter_na = T) |>
    pluck("step_1_ref_and_loc_info") |>
    select(reference_number, alias_local_name),
  by = c('local_name' = 'alias_local_name')
) |>
  relocate(reference_number, .before = 'local_name')


# arrange for easy c/p into `step_3_individual_fish_data`
fish_ind_data <- ref_names |>
  dplyr::select(reference_number, local_name, sampling_method, pass_number, species:weight, comments)


fish_ind_data |>
  # burn cleaned file to repo
  readr::write_csv(file = path_repo_fish_data_ind, na = '')



# Fish Data step 2 ------------------------------------------------------

# Prepare the fish data for copy paste into `step_2_fish_coll_data` of the habitat confirmations spreadsheet.

# read in the form_fiss_site which contains the site length info.
form_fiss_site <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form_fiss_site,
  dir_backup = "data/backup/",
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  return_object = TRUE,
  col_easting = "utm_easting",
  col_northing = "utm_northing") |>
  sf::st_drop_geometry()



fish_coll_data <- dplyr::left_join(
  #join the data from form_fiss_site and the fish data with reference numbers
  ref_names |>
    dplyr::select(reference_number,
                  local_name,
                  sampling_method:enclosure,
                  species:weight),
  form_fiss_site |>
    dplyr::select(local_name,
                  temperature_c,
                  conductivity_m_s_cm,
                  turbidity,
                  site_length,
                  avg_wetted_width_m),
  by = ('local_name')

  ) |>
  # add in make, model, and extra empty columns for easy c/p
  dplyr::mutate(model = case_when(stringr::str_like(local_name, '%ef%') ~ 'halltech HT2000'),
                make = case_when(stringr::str_like(local_name, '%ef%') ~ 'other'),
                method_number = NA,
                pulse = NA,
                age = NA) |>
  # add in life stage
  dplyr::mutate(life_stage = case_when(
    length <= 65 ~ 'fry',
    length > 65 & length <= 110 ~ 'parr',
    length > 110 & length <= 140 ~ 'juvenile',
    length > 140 ~ 'adult',
    T ~ NA_character_
  ),
  # when the species is not a salmonid we don't add a life stage. Check to see what other species are present and add!!!
  life_stage = case_when(
    stringr::str_like(species, '%sculpin%') ~ NA_character_,
    T ~ life_stage),
  comments = case_when(
    stringr::str_like(species, '%sculpin%') ~
      'Not salmonids so no life stage specified.',
    T ~ NA),
  # refactor (needed later in the plot)
  life_stage = fct_relevel(life_stage,
                                  'fry',
                                  'parr',
                                  'juvenile',
                                  'adult')) |>
  #group by life stage
  dplyr::group_by(dplyr::across(-all_of(c('length', 'weight')))) |>
  dplyr::summarise(min_length = min(length),
            max_length = max(length),
            total_num = length(length)) |>
  # reorder for easy c/p into `step_2_fish_coll_data`
  dplyr::select(reference_number,
                local_name,
                temperature_c,
                conductivity_m_s_cm,
                turbidity,
                sampling_method,
                method_number,
                pass_number,
                ef_seconds,
                site_length,
                avg_wetted_width_m,
                enclosure,
                voltage,
                frequency,
                pulse,
                make,
                model,
                species,
                life_stage,
                age,
                total_num,
                min_length,
                max_length,
                comments)



# Burn to csv and c/p into `step_2_fish_coll_data` of the habitat confirmations spreadsheet
fish_coll_data |>
  # burn cleaned file to repo
  readr::write_csv(file = path_repo_fish_data_coll, na = '')



