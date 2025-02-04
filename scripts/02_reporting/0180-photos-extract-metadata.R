##here we need to pull all the metadata from all the marked photos so we can use it to have our photos show on the leaflet map
## NOTE: this script needs to be re run if photos are deleted or new ones added

# define your project repo name b/c this is not
repo_name <- 'fish_passage_skeena_2024_reporting'

photo_metadata <- exifr::read_exif('data/photos', recursive=T) |>
  janitor::clean_names() |>
  dplyr::select(file_name, source_file, create_date, gps_latitude, gps_longitude) |>
  dplyr::mutate(url  = paste0('https://github.com/NewGraphEnvironment/', repo_name, '/raw/main/',
                       source_file)) |>
  # filter photos used in hab con site memos, but do not include photos used for pscis phase 2 submission portal as we don't want to clutter map
  # portal photos have been labelled '_k_nm' to distinguish them, they are still committed to repo
  dplyr::filter(
    stringr::str_detect(file_name, "_k_") & !stringr::str_detect(file_name, "_nm_")
  ) |>
  dplyr::mutate(create_date = lubridate::as_datetime(create_date, tz="America/Vancouver"))



# Add the data to the sqlite
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_drop_table("photo_metadata", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(photo_metadata, exists = F, delete = TRUE,
          conn = conn, x_name = "photo_metadata")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)

