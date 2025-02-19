
# build priority spreadsheet ----------------------------------------------

# spreadsheet to build for input includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())


# Function to replace empty character and numeric values with NA
replace_empty_with_na <- function(x) {
  if(is.character(x) && length(x) == 0) return(NA_character_)
  if(is.numeric(x) && length(x) == 0) return(NA_real_)
  return(x)
}

# specify in index.Rmd YAML which species you want to use for the modelling
# For Skeena we use steelhead
# For Peace we use bull trout

# Convert the species-specific rearing column to a symbol upfront
model_species_rearing_km <- rlang::sym(paste0(params$model_species, "_rearing_km"))

hab_priority_prep <- form_fiss_site |>
  dplyr::select(
    stream_name = gazetted_names,
    local_name,
    date_time_start
  ) |>
  tidyr::separate(local_name, c("site", "location", "ef"), sep = "_", remove = FALSE) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    crew_members = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = crew_members)),
    length_surveyed = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = site_length)),
    hab_value = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = habitat_value_rating)),

    # Priority pulled from form_pscis
    priority = list(fpr::fpr_my_bcfishpass(dat = form_pscis, site = site, col_filter = site_id, col_pull = my_priority)),

    # Comments field
    comments = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = comments)),

    # Unquoting only for the dynamic species-specific column
    upstream_habitat_length_m = list(
      fpr::fpr_my_bcfishpass(site = site, col_pull = !!model_species_rearing_km, round_dig = 4)
    ),
    upstream_habitat_length_m = list(round((upstream_habitat_length_m * 1000), digits = 0)),

    # Static column, no unquoting needed
    species_codes = list(fpr::fpr_my_bcfishpass(site = site, col_pull = observedspp_upstr)),

    # Replace empty values with NA
    dplyr::across(everything(), ~replace_empty_with_na(.))
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(is.na(ef)) |>
  dplyr::mutate(priority = dplyr::case_when(priority == "mod" ~ "moderate", TRUE ~ priority)) |>
  dplyr::mutate(priority = stringr::str_to_title(priority)) |>
  dplyr::mutate(hab_value = stringr::str_to_title(hab_value)) |>
  dplyr::arrange(local_name, crew_members, date_time_start) |>
  sf::st_drop_geometry()


# burn to csv
hab_priority_prep|>
  readr::write_csv("data/habitat_confirmations_priorities.csv", na = '')


# extract rd cost multiplier ----------------------------------------------

# rebuild using bcfishpass object from the tables.R script.
# see older repos if we need to go back to a system that can run these before we have pscis IDs simplifying for now on
rd_class_surface <- bcfishpass |>
  dplyr::select(stream_crossing_id, transport_line_structured_name_1:dam_operating_status) |>
  dplyr::filter(
    stringr::str_detect(
      stream_crossing_id,
      paste0(
        pscis_all |>
          dplyr::filter(!is.na(pscis_crossing_id)) |>
          dplyr::pull(pscis_crossing_id),
        collapse = "|"
      )
    )
  )|>
  dplyr::mutate(my_road_class = ften_file_type_description)|>
  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(transport_line_type_description) ~
                                            transport_line_type_description,
                                          T ~ my_road_class))|>

  dplyr::mutate(my_road_class = case_when(is.na(my_road_class) & !is.na(rail_owner_name) ~
                                            'rail',
                                          T ~ my_road_class))|>
  dplyr::mutate(my_road_surface = case_when(is.na(transport_line_surface_description) & !is.na(ften_file_type_description) ~
                                              'loose',
                                            T ~ transport_line_surface_description))|>
  dplyr::mutate(my_road_surface = case_when(is.na(my_road_surface) & !is.na(rail_owner_name) ~
                                              'rail',
                                            T ~ my_road_surface))|>
  dplyr::mutate(my_road_class = stringr::str_replace_all(my_road_class, 'Forest Service Road', 'fsr'),
                my_road_class = stringr::str_replace_all(my_road_class, 'Road ', ''),
                my_road_class = stringr::str_replace_all(my_road_class, 'Special Use Permit, ', 'Permit-Special-'),
                my_road_class = dplyr::case_when(
                  stringr::str_detect(my_road_class, '%driveway%') ~ 'driveway',
                  T ~ my_road_class),
                my_road_class = stringr::word(my_road_class, 1),
                my_road_class = stringr::str_to_lower(my_road_class))



conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_drop_table("rd_class_surface", conn = conn)
readwritesqlite::rws_write(rd_class_surface, exists = F, delete = T,
                           conn = conn, x_name = "rd_class_surface")
readwritesqlite::rws_disconnect(conn)



# Fish histogram -----------------------------------------------------------------------
bin_1 <- floor(min(fish_data_complete$length, na.rm = TRUE) / 5) * 5
bin_n <- ceiling(max(fish_data_complete$length, na.rm = TRUE) / 5) * 5
bins <- seq(bin_1, bin_n, by = 5)

# Check what species we have and filter out any we don't want, took out the hybrids so we have an even number of facets
fish_data_complete |> dplyr::distinct(species)

plot_fish_hist <- ggplot2::ggplot(
  fish_data_complete |> dplyr::filter(!species %in% c('Fish Unidentified Species',"Cutthroat Trout /Rainbow Trout  hybrid",  'NFC')),
  ggplot2::aes(x = length)
) +
  ggplot2::geom_histogram(breaks = bins, alpha = 0.75,
                          position = "identity", size = 0.75) +
  ggplot2::labs(x = "Fork Length (mm)", y = "Count (#)") +
  ggplot2::facet_wrap(~species) +
  ggplot2::theme_light(base_size = 8) +
  ggplot2::scale_x_continuous(breaks = bins[seq(1, length(bins), by = 2)]) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

plot_fish_hist

ggplot2::ggsave(plot = plot_fish_hist, file = "./fig/fish_histogram.png",
                h = 3.4, w = 5.11, units = "in", dpi = 300)









# OLD CODE ----------------------
#### THE FOLLOWING CODE IS OLD AND IM NOT SURE IF WE STILL NEED IT SO LEAVING FOR NOW


# # xref_hab_site_corrected----------------------
# habitat_confirmations <- fpr_import_hab_con()
#
# hab_loc <- habitat_confirmations|>
#   purrr::pluck("step_1_ref_and_loc_info")|>
#   dplyr::filter(!is.na(site_number))%>%
#   mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))|>
#   tidyr::separate(alias_local_name, into = c('site', 'location', 'fish'), remove = F)|>
#   select(site:fish)|>
#   mutate(site = as.numeric(site))
#
# xref_hab_site_corrected <- left_join(
#   hab_loc,
#   xref_pscis_my_crossing_modelled,
#   by = c('site' = 'external_crossing_reference')
# )|>
#   mutate(stream_crossing_id = as.numeric(stream_crossing_id),
#          stream_crossing_id = case_when(
#            is.na(stream_crossing_id) ~ site,
#            T ~ stream_crossing_id
#          ))|>
#   mutate(site_corrected = paste(stream_crossing_id, location, fish, sep = '_'))|>
#   mutate(site_corrected = stringr::str_replace_all(site_corrected, '_NA', ''))|>
#   tibble::rownames_to_column()|>
#   readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/xref_hab_site_corrected.csv'), na = '')
#
#
# # rws_list_tables(conn)
# # rws_drop_table("xref_hab_site_corrected", conn = conn) ##now drop the table so you can replace it
# # rws_write(hab_site_corrected, exists = F, delete = TRUE,
# #           conn = conn, x_name = "xref_hab_site_corrected")
#
# # xref_phase2_corrected------------------------------------
# # once we have our data loaded this gives us a xref dataframe to pull in pscis ids and join to our  spreadsheet imports
# pscis_all <- bind_rows(pscis_list)
#
# xref_phase2_corrected <- left_join(
#   pscis_all,
#   xref_pscis_my_crossing_modelled,
#   by = c('my_crossing_reference' = 'external_crossing_reference')
# ) |>
#   mutate(pscis_crossing_id = case_when(
#     is.na(pscis_crossing_id) ~ stream_crossing_id,
#     T ~ as.integer(pscis_crossing_id)
#   ))|>
#   dplyr::filter(str_detect(source, 'phase2'))  |>
#   readr::write_csv(file = '/data/inputs_extracted/xref_phase2_corrected.csv', na = '')
#
# # rws_list_tables(conn)
# # rws_drop_table("xref_phase2_corrected", conn = conn) ##now drop the table so you can replace it
# # rws_write(xref_pscis_my_crossing_phase2, exists = F, delete = TRUE,
# #           conn = conn, x_name = "xref_phase2_corrected")
# # rws_list_tables(conn)
# # rws_disconnect(conn)
#
#
#
#
# # Fish species and hab gain estimates for phase 2 sites ------------------------
#
# habitat_con_pri <- read_csv('data/habitat_confirmations_priorities.csv')
#
# hab_priority_fish_hg <- left_join(
#   habitat_con_pri |> select(reference_number, alias_local_name, site, location, ef),
#   bcfishpass |> select(stream_crossing_id, observedspp_upstr, st_rearing_km),
#   by = c('site' = 'stream_crossing_id')
# ) |>
#   mutate(observedspp_upstr = gsub("[{}]", "", observedspp_upstr)) |>
#   mutate(observedspp_upstr = case_when(
#     alias_local_name %like% '_ds' |
#       # ends in a number
#       alias_local_name %like% '\\d$' ~ NA_character_,
#     T ~ observedspp_upstr),
#     st_rearing_km = case_when(
#       alias_local_name %like% 'ds' |
#         # ends in a number
#         alias_local_name %like% '\\d$' ~ NA_real_,
#       T ~ st_rearing_km)) |>
#   rename(species_codes = observedspp_upstr) |>
#   mutate(
#     upstream_habitat_length_m = st_rearing_km * 1000,
#     species_codes = stringr::str_replace_all(species_codes, c('CCT,|SST,|SP,'), ''),
#     species_codes = case_when(
#       site == 198090 ~ NA_character_,
#       T ~ species_codes
#     )
#   ) |>
#   readr::write_csv('data/inputs_extracted/hab_priority_fish_hg.csv', na = '')
#
#
#

# ######----------------density plots--------------------------
# # needs to be modified to have remve the haul number and just use the pop estimate
#
# hab_fish_dens <- hab_fish_indiv |>
#   filter(sampling_method == 'electrofishing') |> ##added this since we now have mt data as well!!
#   mutate(area = round(ef_length_m * ef_width_m),0) |>
#   group_by(local_name, method_number, haul_number_pass_number, ef_length_m, ef_width_m, ef_seconds, area, species_code, life_stage) |>
#   summarise(fish_total = length(life_stage)) |>
#   ungroup() |>
#   mutate(density_100m2 = round(fish_total/area * 100, 1)) |>
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
#   mutate(site_id = paste0(site, location),
#          location = case_when(location == 'us' ~ 'Upstream',
#                               T ~ 'Downstream'),
#          life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult')))
#
# # hab_fish_dens |>
# #   readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/hab_fish_dens.csv'))
#
# ##paths to write to will need to change now
# # ggsave(plot = plot_fish_box, filename = "./fig/plot_fish_box.png",
# #        h=9.66, w=14.5, units="cm", dpi=300)
#
#
# ##clean up the objects
# rm(hab_site_prep,
#    # hab_fish_indiv_prep,
#    # hab_fish_indiv_prep2,
#    hab_fish_collect_prep2,
#    hab_loc2)
#
# # gps get coordinates for waypoints -----------------------------------------------------
#
# gpx <- 'C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/GPS/kylegps_sept22backup_bulkley2021.GPX'
#
#
# wp_kyle <- sf::st_read(gpx,
#                        layer = 'waypoints',
#   quiet = T) |>
#   janitor::clean_names() |>
#   # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
#   mutate(name = as.numeric(name),
#          name = case_when(name > 1000 ~ round(name/10, 0),
#                           T ~ name)) |>
#   dplyr::select(name_old = name, everything())  |>
#   mutate(source = 'KP',
#          name = paste0(name_old, '_', source, '_', lubridate::year(time))) |>
#   sf::st_transform(crs = 26909) |>
#   poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') |>
#   select(name, name_old, source, ele, time, easting, northing)
#
# gpx <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/GPS/bulkley_2021_field_al.gpx"
#
# wp_al <- sf::st_read(gpx,
#                      layer = 'waypoints',
#                      quiet = T) |>
#   janitor::clean_names() |>
#   # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
#   mutate(name = as.numeric(name),
#          name = case_when(name > 1000 ~ round(name/10, 0),
#                           T ~ name)) |>
#   dplyr::select(name_old = name, everything())  |>
#   mutate(source = 'AI',
#          name = paste0(name_old, '_', source, '_', lubridate::year(time))) |>
#   sf::st_transform(crs = 26909) |>
#   poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') |>
#   select(name, name_old, source, ele, time, easting, northing)
#
# wp <- bind_rows(
#   wp_kyle,
#   wp_al
# )
#
# rm(wp_kyle, wp_al)
#
# # join with the priorities spreadsheet to get those coords
# hab_con <- readr::read_csv(file = "./data/habitat_confirmations_priorities.csv")
#
#
# wp_joined <- left_join(
#   hab_con|> separate(crew_members, into = c('source', 'others')),
#   wp|> select(name_old, source, easting, northing),
#   by = c('waypoint' = 'name_old', 'source')
# )
#
# # bring in the locations and insert utms where we don't have them already
# hab_loc_utm <- left_join(
#   fpr_import_hab_con(backup = F, col_filter_na = T)|>
#   purrr::pluck("step_1_ref_and_loc_info"),
#
#   wp_joined|> select(alias_local_name, easting, northing),
#
#   by = 'alias_local_name'
# )|>
#   mutate(
#     utm_easting = case_when(
#     is.na(utm_easting) ~ easting,
#     T ~ utm_easting),
#     utm_northing = case_when(
#       is.na(utm_northing) ~ northing,
#       T ~ utm_northing
#       )
#     )
#
#
