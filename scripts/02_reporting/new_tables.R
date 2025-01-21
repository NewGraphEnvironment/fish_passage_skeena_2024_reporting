## new tables.R

## Params -------------------------------------------------

path_form_fiss_site <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_fiss_site_2024.gpkg')



## Load PSCIS data -------------------------------------------------

# import data and build tables we for reporting
pscis_list <- fpr::fpr_import_pscis_all()
pscis_phase1 <- pscis_list |> pluck('pscis_phase1')
pscis_phase2 <- pscis_list |> pluck('pscis_phase2') |>
  arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list |> pluck('pscis_reassessments')
pscis_all_prep <- pscis_list |>
  bind_rows()



# WHY DO WE NEED THIS, BECAUSE:
# It looked like this object is used in multiple other parts of this script.
# Take note of how it is used as we continue purging this script

# this doesn't work till our data loads to pscis
pscis_all <- left_join(
  pscis_all_prep,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ as.numeric(stream_crossing_id),
    TRUE ~ pscis_crossing_id
  )) %>%
  arrange(pscis_crossing_id)




## Load habitat data -------------------------------------------------

# build a new object from `form_fiss_site_2024`, with the geom dropped,
# seperate alias_local_name into site, location, and ef.
# and all the averaged values, using ngr_str_df_col_agg https://www.newgraphenvironment.com/ngr/reference/ngr_str_df_col_agg.html

form_fiss_site <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form_fiss_site,
  dir_backup = "data/backup/",
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  return_object = TRUE,
  col_easting = "utm_easting",
  col_northing = "utm_northing")


# Separate the local name and filter to exclude electrofishing sites.
hab_site_raw <- form_fiss_site |>
  sf::st_drop_geometry() |>
  tidyr::separate_wider_delim(local_name,
                              delim = "_",
                              names = c('site', 'location', 'ef'),
                              too_few = "align_start",
                              cols_remove = FALSE) |>
  dplyr::filter(is.na(ef))



# aggregate the numeric columns
# as per the example in ?ngr_str_df_col_agg
col_str_negate = "time|method|avg|average"
col_str_to_agg <- c("channel_width", "wetted_width", "residual_pool", "gradient", "bankfull_depth")
columns_result <- c("avg_channel_width_m", "avg_wetted_width_m", "average_residual_pool_depth_m", "average_gradient_percent", "average_bankfull_depth_m")

hab_site <- purrr::reduce(
  .x = seq_along(col_str_to_agg),
  .f = function(dat_acc, i) {
    ngr::ngr_str_df_col_agg(
      # we call the dataframe that accumulates results dat_acc
      dat = dat_acc,
      col_str_match = col_str_to_agg[i],
      col_result = columns_result[i],
      col_str_negate = col_str_negate,
      decimal_places = 1
    )
  },
  .init = hab_site_raw
)


## bcfishpass modelling table setup for reporting -------------------------------------------------

# WHY DO WE NEED THIS, BECAUSE:
# We need it to cross reference the name of the columns in bcfishpass with the column names we use in the reports

# bcfishpass modelling table setup for reporting
xref_bcfishpass_names <- fpr::fpr_xref_crossings



## Build a spacial file of all the PSCIS crossings? -------------------------------------------------

# WHY DO WE NEED THIS, BECAUSE:
# The `pscis_all_sf` object is used in `0170-load-wshd_stats.R` to add the site elevations to the wshds object,
# which is used in the memos and is also burned to a geopackage, kml, and the sqlite.

## could we just read in the `form_pscis_2024.gpkg` instead?


pscis_all_sf_prep <- pscis_all |>
  dplyr::group_split(utm_zone) |>
  purrr::map(
    ~ sf::st_as_sf(.x, coords = c("easting", "northing"),
                   crs = 26900 + unique(.x$utm_zone), remove = FALSE)
  ) |>
  # convert to match the bcfishpass format
  purrr::map(
    sf::st_transform, crs = 3005) |>
  dplyr::bind_rows()



# looks like the `ps_elevation_google()` api maxes out at 220 queries and we have 223.  As a work around split by source,
# then call the function which is now in staticimports
pscis_all_sf <- pscis_all_sf_prep |>
  dplyr::group_split(source) |>
  purrr::map(sfpr_get_elev) |>
  dplyr::bind_rows()


rm(pscis_all_sf_prep)

