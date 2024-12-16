#combining pit tag data to individual fish data so that we can copy and paste directly into submission template
# would be good to include the comments and times/camera of photos in the fish csv and then paste them to the right of the pit tag.

source('scripts/packages.R')

# Pit Tags ------
#import the pit tag csv
# Pit tag data is currently being stored on OneDrive until the 2024 repository has been made.
path_tag <- '/Users/lucyschick/Library/CloudStorage/OneDrive-Personal/Projects/2024-072-sern-skeena-fish-passage/data/tag_01_04.csv'

# tag_01_04 does not have a column name so for that reason the call to read_csv needs to be different (change col_names to F for that file) and
# the column name will default to X1.
pit_tag <- readr::read_csv(path_tag, col_names = F) |>
  #separate the pit tag out from the rest of the info in the pit tag csv
  # https://stackoverflow.com/questions/66696779/separate-by-pattern-word-in-tidyr-and-dplyr
  tidyr::separate(col=X1, into=c('date', 'tag_id'), sep='\\s*TAG\\s*') |>
  tibble::rowid_to_column()


#import csv with fish data
path_fish <- '/Users/lucyschick/Library/CloudStorage/OneDrive-Personal/Projects/2024_data/fish_data_raw.xlsx'

# Read and clean the data
fish <- readxl::read_xlsx(path_fish, sheet = "fish_data") |>
  # remove the dates added by excel, they are wrong. We only want the time segments
  mutate(across(c(site_start_time, site_end_time,segment_start_time, segment_end_time, photo_time_start, photo_time_end),
                ~ format(., "%H:%M:%S")))

# Unsure if this will still be nessissary
  # mutate(row_id = tag_row - 1)
  # # readr::write_csv('data/inputs_raw/fish_data.csv', na = '')

#join fish csv with pit tag csv based on tag row number
fish_tags <- dplyr::left_join(fish,
                              pit_tag,
                              by = c("tag_row" = "rowid")) %>%
  # arrange columns
  relocate(tag_id, .after = last_col()) %>%
  # remove the first row because it was from the office. We need to pass the object piped to nrow as a "."
  # as the first object gets passed to the slice function only
  dplyr::slice(1:nrow(.)) %>%
  # add a period, a space and the row number to the pit tag to go in the comments to make it easy to pull anything out we want later
  dplyr::mutate(tag_id = case_when(
    !is.na(tag_id) ~ paste0(tag_id, '. Row ID ', tag_row, '. '),
    T ~ tag_id))

# select a subsample of fish (lets go 15% since the sample size is small) to review manually to be sure the
# pit tags match which fish they go with
# set seed for reproducible sample - try running it again without setting the seed immediately before and see how it differs
set.seed(1234)
sample(nrow(fish_tags), nrow(fish_tags) * 0.15) %>%
  # arrange the row numbers to make it easier to QA
  sort()

# burn the csv ready to cut and paste
fish_tags %>%
  readr::write_csv('data/dff/fish_tags_joined.csv',
                   na = "" )

# Fish Data -----
# import raw fish data csv on onedrive, add common names and reference numbers
path <- 'Projects/2023_data/peace/fish/fish_data.csv'
stub_from <- 'C:/Users/matwi/OneDrive/'

fish_data <- readr::read_csv(file = paste0(stub_from, path)) %>%
  janitor::clean_names() %>%
  # there is an extra underscore in site names after ef that needs to be removed
  mutate(local_name = str_replace_all(local_name, 'ef_', 'ef'))

# cross reference with step 1 of hab con sheet to get ref numbers
ref_names <- left_join(
  fish_data,
  fpr_import_hab_con(backup = F, row_empty_remove = T, col_filter_na = T) %>%
    pluck(1) %>%
    select(reference_number, alias_local_name),
  by = c('local_name' = 'alias_local_name')
) %>%
  relocate(reference_number, .before = 'local_name')

# import fish names and codes
hab_fish_codes <- fishbc::freshwaterfish %>%
  select(species_code = Code, common_name = CommonName) %>%
  # add option when there was no fish caught
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') %>%
  # CT is named differently in hab con sheet
  mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))

# xref and change codes to common names in raw file
fish_names <- left_join(
  ref_names,
  hab_fish_codes,
  by = c('species' = 'species_code')
) %>%
  # re arrange columns to align with step 3 of submission sheet, drop species code column
  select(-species) %>%
  relocate(common_name, .before = 'length_mm') %>%
  # burn cleaned file to repo
  readr::write_csv(file = 'data/inputs_raw/fish_data.csv', na = '')


