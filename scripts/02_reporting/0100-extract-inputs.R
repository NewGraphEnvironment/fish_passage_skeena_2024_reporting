
# Road Tenure -------------------------------------------------------------
#
# pscis_all_sf <- dat
#
# ##get the road info from the database
# conn <- fpr::fpr_db_conn()
#
# # add a unique id - we could just use the reference number
# pscis_all_sf$misc_point_id <- seq.int(nrow(pscis_all_sf))
#
# # dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "test_hack",";"))
# # load to database
# sf::st_write(obj = pscis_all_sf, dsn = conn, Id(schema= "ali", table = "misc"))
#
#
#
# # sf doesn't automagically create a spatial index or a primary key
# res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
# dbClearResult(res)
# res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
# dbClearResult(res)
#
# dat_info <- dbGetQuery(conn, "SELECT
#   a.misc_point_id,
#   b.*,
#   ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
# FROM
#   ali.misc AS a
# CROSS JOIN LATERAL
#   (SELECT *
#    FROM fish_passage.modelled_crossings_closed_bottom
#    ORDER BY
#      a.geometry <-> geom
#    LIMIT 1) AS b")
#
#
# ##swapped out fish_passage.modelled_crossings_closed_bottom for bcfishpass.barriers_anthropogenic
#
# ##join the modelling data to our pscis submission info
# dat_joined <- left_join(
#   select(pscis_all_sf, misc_point_id, pscis_crossing_id, my_crossing_reference, source), ##traded pscis_crossing_id for my_crossing_reference
#   dat_info,
#   by = "misc_point_id"
# )|>
#   mutate(downstream_route_measure = as.integer(downstream_route_measure))
#
#
# dbDisconnect(conn = conn)
#
#
# ##we also need to know if the culverts are within a municipality so we should check
# ##get the road info from our database
# conn <- fpr::fpr_db_conn()
#
# # load to database
# sf::st_write(obj = pscis_all_sf, dsn = conn, Id(schema= "working", table = "misc"))
#
# dat_info <- dbGetQuery(conn,
#                        "
#
#                                   SELECT a.misc_point_id, b.admin_area_abbreviation, c.map_tile_display_name
#                                   FROM working.misc a
#                                   INNER JOIN
#                                   whse_basemapping.dbm_mof_50k_grid c
#                                   ON ST_Intersects(c.geom, ST_Transform(a.geometry,3005))
#                                   LEFT OUTER JOIN
#                                   whse_legal_admin_boundaries.abms_municipalities_sp b
#                                   ON ST_Intersects(b.geom, ST_Transform(a.geometry,3005))
#                        ")
#
# dbDisconnect(conn = conn)
#
# ##add the municipality info
# dat_joined2 <- left_join(
#   dat_joined,
#   dat_info,
#   by = "misc_point_id"
# )
#
# # ##clean up the workspace
# rm(dat_info, dat_joined, res)
# #
#
# ##this no longer works because we were using the fish_passage.modelled_crossings_closed_bottom and now we don't have the rd info
# ##make a tibble of the client names so you can summarize in the report
# ##we do not need to repeat this step but this is how we make a dat to paste into a kable in rmarkdown then paste tibble as a rstudio addin so we can
# ##populate the client_name_abb...
#
# ##we already did this but can do it again I guess.  you cut and paste the result into kable then back
# ##into here using addin for datapasta
# # tab_rd_tenure_xref <- unique(dat_joined2$client_name)|>
# #   as_tibble()|>
# #   purrr::set_names(nm = 'client_name')|>
# #   mutate(client_name_abb = NA)
#
# tab_rd_tenure_xref <- tibble::tribble(
#                                            ~client_name, ~client_name_abb,
#                                                      NA,               NA,
#                         "DISTRICT MANAGER NADINA (DND)",       "FLNR DND",
#                         "CANADIAN FOREST PRODUCTS LTD.",         "Canfor",
#                                "WEST FRASER MILLS LTD.",    "West Fraser",
#                                        "CHARLES PRIEST", "Charles Priest",
#                          "SOLID GROUND CONTRACTING LTD",   "Solid Ground",
#                                "CHINOOK COMFOR LIMITED", "Chinook Comfor"
#                         )
#
# ##add that to your dat file for later
# dat_joined3 <- left_join(
#   dat_joined2,
#   tab_rd_tenure_xref,
#   by = 'client_name'
# )
#
# ##make a dat to make it easier to see so we can summarize the road info we might want to use
# dat_joined4 <- dat_joined3|>
#   mutate(admin_area_abbreviation = case_when(
#     is.na(admin_area_abbreviation) & (road_class %ilike% 'arterial' | road_class %ilike% 'local') ~ 'MoTi',
#     T ~ admin_area_abbreviation),
#     admin_area_abbreviation = replace_na(admin_area_abbreviation, ''),
#     my_road_tenure =
#       case_when(!is.na(client_name_abb) ~ paste0(client_name_abb, ' ', forest_file_id),
#                 !is.na(road_class) ~ paste0(admin_area_abbreviation, ' ', stringr::str_to_title(road_class)),
#                 !is.na(owner_name) ~ owner_name))|>
#   mutate(my_road_tenure =
#            case_when(distance > 100 ~ 'Unknown',  ##we need to get rid of the info for the ones that are far away
#                      T ~ my_road_tenure))|>
#   rename(geom_modelled_crossing = geom)|>
#   mutate(
#     my_road_tenure =stringr::str_trim(my_road_tenure),
#     aggregated_crossings_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
#                                         my_crossing_reference > 200000000 ~ my_crossing_reference,
#                                         T ~ my_crossing_reference + 1000000000))|>
#   sf::st_drop_geometry()
#
# ##we cannot use base R to add a column named 'source' so we choose a different name
# col_new <- pscis_all_sf$source
# dat_joined4$source_wkb <- col_new
#
#
# ##build tables to populate the pscis spreadsheets
# pscis1_rd_tenure <- left_join(
#   select(pscis_phase1, rowid, my_crossing_reference, road_tenure),
#   dat_joined4|> filter(source_wkb %ilike% 'phase1')|> select(my_crossing_reference, my_road_tenure),
#   by = 'my_crossing_reference'
# )|>
#   # for some reason there is a duplicate. not messing withi it
#   distinct(rowid, .keep_all = T)
#
#
#
#
# ##burn it all to a file we can input to pscis submission spreadsheet
# pscis1_rd_tenure|>
#   readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis1.csv'),
#                    na = '')
#
#
#
# pscis_reassessments_rd_tenure <- left_join(
#   select(pscis_reassessments, rowid, pscis_crossing_id, road_tenure),
#   dat_joined4|> filter(source_wkb %ilike% 'reassess')|> select(pscis_crossing_id, my_road_tenure),
#   by = 'pscis_crossing_id'
# )
#
#
# ## burn --------------------------------------------------------------------
#
# ##burn it all to a file we can input to pscis submission spreadsheet
# pscis_reassessments_rd_tenure|>
#   readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_reassessments.csv'),
#                    na = '')
# ##we need to qa which are our modelled crossings at least for our phase 2 crossings
#
# pscis2_rd_tenure <- left_join(
#   select(pscis_phase2, rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, road_tenure),
#   dat_joined4|> filter(source_wkb %ilike% 'phase2')|> select(aggregated_crossings_id, my_road_tenure),
#   by = 'aggregated_crossings_id'
# )
#
#
# ##burn it all to a file we can input to pscis submission spreadsheet
# pscis2_rd_tenure|>
#   readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis2.csv'),
#                    na = '')
#
#

# build priority spreadsheet ----------------------------------------------

# spreadsheet to build for input includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())


# Function to replace empty character and numeric values with NA
replace_empty_with_na <- function(x) {
  if(is.character(x) && length(x) == 0) return(NA_character_)
  if(is.numeric(x) && length(x) == 0) return(NA_real_)
  return(x)
}

## used st_rearing_km for Skeena 2024
hab_priority_prep <- form_fiss_site |>
  dplyr::select(stream_name = gazetted_names,
                local_name,
                date_time_start) |>
  tidyr::separate(local_name, c("site", "location", "ef"), sep = "_", remove = FALSE) |>
  dplyr::rowwise() |>
  # lets make the columns with functions
  dplyr::mutate(
    crew_members = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = crew_members)),
    length_surveyed = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name,col_pull = site_length)),
    hab_value = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = habitat_value_rating)),
    # `priority` is not currently in the 2024 `form_fiss_site` formso for now we will pull it from form_pscis
    priority = list(fpr::fpr_my_bcfishpass(dat = form_pscis, site = site, col_filter = site_id, col_pull = my_priority)),
    ## first we grab hand bombed estimate from form so that number stands if it is present
    # `us_habitat_m` and `species_known` are not currently in the 2024 `form_fiss_site` form but it will be added to the 2025 form
    # us_habitat_m = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = us_habitat_m)),
    # species_known = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = species_known)),
    comments = list(fpr::fpr_my_bcfishpass(dat = form_fiss_site, site = local_name, col_filter = local_name, col_pull = comments)),
    upstream_habitat_length_m = list(fpr::fpr_my_bcfishpass(site = site, col_pull = st_rearing_km, round_dig = 4)),
    # `upstream_habitat_length_m` is currently in km due to `st_rearing_km` being in kms.
    upstream_habitat_length_m = list(round((upstream_habitat_length_m * 1000), digits = 0)),
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

    dplyr::across(everything(), ~replace_empty_with_na(.))) |>
  dplyr::ungroup() |>
  dplyr::filter(is.na(ef)) |>
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


####----tab cost multipliers for road surface-----
# moving this to a csv to simplify setup. We just alter the csv when we need more categories and want to change the price
# csv located in 'data/inputs_raw/tab_cost_rd_mult.csv'

# rd_cost_mult <- pscis_rd|>
#   select(my_road_class, my_road_surface)|>
#   # mutate(road_surface_mult = NA_real_, road_class_mult = NA_real_)|>
#   mutate(road_class_mult = case_when(my_road_class == 'local' ~ 4,
#                                      my_road_class == 'collector' ~ 4,
#                                      my_road_class == 'arterial' ~ 15,
#                                      my_road_class == 'highway' ~ 15,
#                                      my_road_class == 'rail' ~ 15,
#                                      T ~ 1)) |>
#   mutate(road_surface_mult = case_when(my_road_surface == 'loose' |
#                                          my_road_surface == 'rough' ~
#                                          1,
#                                        T ~ 2))|>
#   # mutate(road_type_mult = road_class_mult * road_surface_mult)|>
#   mutate(cost_m_1000s_bridge = road_surface_mult * road_class_mult * 20,  #changed from 12.5 due to inflation
#          cost_embed_cv = road_surface_mult * road_class_mult * 40)|>
#   # mutate(cost_1000s_for_10m_bridge = 10 * cost_m_1000s_bridge)|>
#   distinct( .keep_all = T)|>
#   tidyr::drop_na()|>
#   arrange(cost_m_1000s_bridge, my_road_class)
#
# rws_write(rd_cost_mult, exists = F, delete = TRUE,
#           conn = conn, x_name = "rd_cost_mult")
# rws_list_tables(conn)
rws_disconnect(conn)


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

# fish summary ------------------------------------------------------------

## Leaving this code here for now but lots of this code has been updated and moved to `fish_data_tidy.R` and now does not
## rely (as much) on the spreadsheet. Double check it is no longer needed and then delete.


# # we need to summarize all our fish sizes
#
# ## fish collection data ----------------------------------------------------
# habitat_confirmations <- fpr::fpr_import_hab_con(row_empty_remove = T)
#
#
# hab_fish_indiv_prep <- habitat_confirmations |>
#   purrr::pluck("step_3_individual_fish_data") |>
#   dplyr::filter(!is.na(site_number)) |>
#   select(-gazetted_names:-site_number)
#
# hab_loc <- habitat_confirmations |>
#   purrr::pluck("step_1_ref_and_loc_info") |>
#   dplyr::filter(!is.na(site_number))|>
#   mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))
#
#
# ##add the species code
# hab_fish_codes <- fishbc::freshwaterfish |>
#   select(species_code = Code, common_name = CommonName) |>
#   tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') |>
#   mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))
#
# hab_fish_indiv_prep2 <- left_join(
#   hab_fish_indiv_prep,
#   hab_loc,
#   by = 'reference_number'
# ) |> mutate(
#   species = case_when(species == 'Fish Unidentified Species' ~ 'Unidentified Species',
#            T ~ species))
#
#
# hab_fish_indiv_prep3 <- left_join(
#   hab_fish_indiv_prep2,
#   select(hab_fish_codes, common_name:species_code),
#   by = c('species' = 'common_name')
# ) |>
#   dplyr::select(reference_number,
#                 alias_local_name,
#                 site_number,
#                 sampling_method,
#                 method_number,
#                 haul_number_pass_number,
#                 species_code,
#                 length_mm,
#                 weight_g) ##added method #
#
#
# ##we need the size of the sites too
#
# ####workflow is a bit weird because we need to input NFC sites and the size of the sites
# ##or else we don't know about them in the summary.
# hab_fish_collect_prep <- habitat_confirmations |>
#   purrr::pluck("step_2_fish_coll_data") |>
#   dplyr::filter(!is.na(site_number)) |>
#   # select(-gazetted_name:-site_number) |>
#   dplyr::distinct(reference_number, method_number, haul_number_pass_number, .keep_all = T) |>
#   # distinct(reference_number, .keep_all = T) |>
#   arrange(reference_number) |>
#   mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) |>
#   mutate(across(c(time_in,time_out), chron::times))
# # hab_fish_collect_prep_mt <- habitat_confirmations |>
# #   purrr::pluck("step_2_fish_coll_data") |>
# #   dplyr::filter(!is.na(site_number)) |>
# #   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
# #   mutate(site_id = paste0(site, location)) |>
# #   distinct(local_name, sampling_method, method_number, .keep_all = T) |> ##changed this to make it work as a feed for the extract-fish.R file
# #   mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) |>
# #   mutate(across(c(time_in,time_out), chron::times))
#
# ##we use this to test things out
# # hab_fish_indiv <- left_join(
# #   select(hab_fish_collect_prep_mt |> filter(reference_number == 36),
# #          reference_number,
# #          local_name,
# #          site_number:model, date_in:time_out ##added date_in:time_out
# #   ),
# #   select(hab_fish_indiv_prep3 |> filter(reference_number == 36),
# #          reference_number,
# #          sampling_method,
# #          method_number, ##added method #
# #          # alias_local_name,
# #          species_code, length_mm),
# #   by = c('reference_number', 'sampling_method', 'method_number') #added method # and haul
# # )
#
# # test to see if there are any missing lengths
# hab_fish_indiv_prep3 |>
#   filter(is.na(length_mm))
#
# # join the indiv fish data to existing site info
# hab_fish_indiv <- full_join(
#   select(hab_fish_indiv_prep3,
#          reference_number,
#          sampling_method,
#          method_number,
#          haul_number_pass_number,
#          species_code,
#          length_mm,
#          weight_g),
#   select(hab_fish_collect_prep,
#          reference_number,
#          local_name,
#          temperature_c:model,
#          date_in:time_out, ##added date_in:time_out because we did minnow traps
#          comments
#   ),
#   by = c(
#     "reference_number",
#     # 'alias_local_name' = 'local_name',
#     "sampling_method",
#     "method_number",
#     "haul_number_pass_number")
# ) |>
#   mutate(species_code = as.character(species_code)) |>
#   mutate(species_code = case_when(
#     is.na(species_code) ~ 'NFC',
#     T ~ species_code)
#   ) |>
#   mutate(species_code = as.factor(species_code)) |>
#   mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
#     length_mm <= 65 ~ 'fry',
#     length_mm > 65 & length_mm <= 110 ~ 'parr',
#     length_mm > 110 & length_mm <= 140 ~ 'juvenile',
#     length_mm > 140 ~ 'adult',
#     T ~ NA_character_
#     )
#   # life_stage = case_when(
#   #   species_code %in% c('L', 'SU', 'LSU') ~ NA_character_,
#   #   T ~ life_stage),
#   # comments = case_when(
#   #   species_code %in% c('L', 'SU', 'LSU') & !is.na(comments) ~
#   #     paste0(comments, 'Not salmonids so no life stage specified.'),
#   #   species_code %in% c('L', 'SU', 'LSU') & is.na(comments) ~
#   #     'Not salmonids so no life stage specified.',
#   #   T ~ comments)
#   )|>
#   mutate(life_stage = fct_relevel(life_stage,
#                                   'fry',
#                                   'parr',
#                                   'juvenile',
#                                   'adult')) |>
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
#   mutate(site_id = paste0(site, '_', location))



# ###------from duncan_fish_plots_20200210
#
# ####----------fish length-----------
# # filter(species_code == "CO")
# # fish_eb <-  hab_fish_indiv |> filter(species_code != "EB")
#
# bin_1 <- floor(min(hab_fish_indiv$length_mm, na.rm = TRUE)/5)*5
# bin_n <- ceiling(max(hab_fish_indiv$length_mm, na.rm = TRUE)/5)*5
# bins <- seq(bin_1,bin_n, by = 5)
#
# plot_fish_hist <- ggplot(hab_fish_indiv |> filter(!species_code %in% c('LSU','SU','NFC')), #!species_code %in% c('LSU','SU','NFC')
#                          aes(x=length_mm
#                              # fill=alias_local_name
#                              # color = alias_local_name
#                          )) +
#   geom_histogram(breaks = bins, alpha=0.75,
#                  position="identity", size = 0.75)+
#   labs(x = "Fork Length (mm)", y = "Count (#)") +
#   facet_wrap(~species_code)+
#   # scale_color_grey() +
#   # scale_fill_grey() +
#   ggdark::dark_theme_bw(base_size = 8)+
#   # theme_bw(base_size = 8)+
#   scale_x_continuous(breaks = bins[seq(1, length(bins), by = 2)])+
#   # scale_color_manual(values=c("grey90", "grey60", "grey30", "grey0"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # geom_histogram(aes(y=..density..), breaks = bins, alpha=1,
# #                position="identity", size = 0.75)
# plot_fish_hist
#
# # ggsave(plot = plot_fish_hist, file="./fig/fish_histogram.png",
# #        h=3.4, w=5.11, units="in", dpi=300)
#
#
# # this will be joined to the abundance estimates and the confidence intervals
# tab_fish_summary <- hab_fish_indiv |>
#   group_by(site_id,
#            ef,
#            sampling_method,
#            # haul_number_pass_number,
#            species_code) |> ##added sampling method!
#   summarise(count_fish = n()) |>
#   arrange(site_id, species_code, ef)


######----------------depletion estimates--------------------------
# we are going to use





######----------------density plots--------------------------
# needs to be modified to have remve the haul number and just use the pop estimate

hab_fish_dens <- hab_fish_indiv |>
  filter(sampling_method == 'electrofishing') |> ##added this since we now have mt data as well!!
  mutate(area = round(ef_length_m * ef_width_m),0) |>
  group_by(local_name, method_number, haul_number_pass_number, ef_length_m, ef_width_m, ef_seconds, area, species_code, life_stage) |>
  summarise(fish_total = length(life_stage)) |>
  ungroup() |>
  mutate(density_100m2 = round(fish_total/area * 100, 1)) |>
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
  mutate(site_id = paste0(site, location),
         location = case_when(location == 'us' ~ 'Upstream',
                              T ~ 'Downstream'),
         life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult')))

# hab_fish_dens |>
#   readr::write_csv(file = paste0(getwd(), '/data/extracted_inputs/hab_fish_dens.csv'))

##paths to write to will need to change now
# ggsave(plot = plot_fish_box, filename = "./fig/plot_fish_box.png",
#        h=9.66, w=14.5, units="cm", dpi=300)


##clean up the objects
rm(hab_site_prep,
   # hab_fish_indiv_prep,
   # hab_fish_indiv_prep2,
   hab_fish_collect_prep2,
   hab_loc2)

# gps get coordinates for waypoints -----------------------------------------------------

gpx <- 'C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/GPS/kylegps_sept22backup_bulkley2021.GPX'


wp_kyle <- sf::st_read(gpx,
                       layer = 'waypoints',
  quiet = T) |>
  janitor::clean_names() |>
  # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
  mutate(name = as.numeric(name),
         name = case_when(name > 1000 ~ round(name/10, 0),
                          T ~ name)) |>
  dplyr::select(name_old = name, everything())  |>
  mutate(source = 'KP',
         name = paste0(name_old, '_', source, '_', lubridate::year(time))) |>
  sf::st_transform(crs = 26909) |>
  poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') |>
  select(name, name_old, source, ele, time, easting, northing)

gpx <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/GPS/bulkley_2021_field_al.gpx"

wp_al <- sf::st_read(gpx,
                     layer = 'waypoints',
                     quiet = T) |>
  janitor::clean_names() |>
  # this is a work around so that we get the original name of the renamed wp if there were duplicate names in basecamp
  mutate(name = as.numeric(name),
         name = case_when(name > 1000 ~ round(name/10, 0),
                          T ~ name)) |>
  dplyr::select(name_old = name, everything())  |>
  mutate(source = 'AI',
         name = paste0(name_old, '_', source, '_', lubridate::year(time))) |>
  sf::st_transform(crs = 26909) |>
  poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') |>
  select(name, name_old, source, ele, time, easting, northing)

wp <- bind_rows(
  wp_kyle,
  wp_al
)

rm(wp_kyle, wp_al)

# join with the priorities spreadsheet to get those coords
hab_con <- readr::read_csv(file = "./data/habitat_confirmations_priorities.csv")


wp_joined <- left_join(
  hab_con|> separate(crew_members, into = c('source', 'others')),
  wp|> select(name_old, source, easting, northing),
  by = c('waypoint' = 'name_old', 'source')
)

# bring in the locations and insert utms where we don't have them already
hab_loc_utm <- left_join(
  fpr_import_hab_con(backup = F, col_filter_na = T)|>
  purrr::pluck("step_1_ref_and_loc_info"),

  wp_joined|> select(alias_local_name, easting, northing),

  by = 'alias_local_name'
)|>
  mutate(
    utm_easting = case_when(
    is.na(utm_easting) ~ easting,
    T ~ utm_easting),
    utm_northing = case_when(
      is.na(utm_northing) ~ northing,
      T ~ utm_northing
      )
    )


