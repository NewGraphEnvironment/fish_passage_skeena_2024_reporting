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
  return_object = TRUE) |>
  sf::st_drop_geometry()



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

form_fiss_site <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path_form_fiss_site,
  dir_backup = "data/backup/",
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  return_object = TRUE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  col_easting = "utm_easting",
  col_northing = "utm_northing") |>
  sf::st_drop_geometry()



## Load fish data -------------------------------------------------

fish_data_complete <- readr::read_csv(file = path_onedrive_tags_joined) |>
  janitor::clean_names() |>
  #filter for skeena 2024
  dplyr::filter(project_name == project)


# Bcfishpass modelling table setup for reporting --------------------------

xref_bcfishpass_names <- fpr::fpr_xref_crossings



## Habitat Summaries -------------------------------------------------

# Build hab_site object for fpr_my_habitat_info()
hab_site <- form_fiss_site


#Build tab_hab_summary object for tables

tab_hab_summary <- form_fiss_site |>
  dplyr::filter(is.na(ef)) |>
  dplyr::select(local_name,
           site,
           location,
           avg_channel_width_m,
           avg_wetted_width_m,
           average_residual_pool_depth_m,
           average_gradient_percent,
           total_cover,
           site_length,
           habitat_value_rating) |>
  dplyr::mutate(location = dplyr::case_when(location == "us" ~ stringr::str_replace_all(location, 'us', 'Upstream'),
    TRUE ~ stringr::str_replace_all(location, 'ds', 'Downstream')
    )) |>
  dplyr::arrange(site, location) |>
  dplyr::select(Site = site,
         Location = location,
         `Length Surveyed (m)` = site_length,
         `Average Channel Width (m)` = avg_channel_width_m,
         `Average Wetted Width (m)` = avg_wetted_width_m,
         `Average Pool Depth (m)` = average_residual_pool_depth_m,
         `Average Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = habitat_value_rating)


# Read priority spreadsheet ----------------------------------------------

# spreadsheet that includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())

# read in the object
habitat_confirmations_priorities <- readr::read_csv(
  file = "data/habitat_confirmations_priorities.csv")



## Fish sampling results condensed ----------------------------------------------
# tab_fish_summary
tab_fish_summary <- fish_data_complete |>
  # exclude visual observations
  dplyr::filter(sampling_method == "electrofishing") |>
  tidyr::separate(local_name, into = c("site_id", "location", "ef")) |>
  dplyr::mutate(site_id = paste0(site_id, "_", location)) |>
  dplyr::group_by(site_id,
                  ef,
                  sampling_method,
                  species) |>
  dplyr::summarise(count_fish = n()) |>
  dplyr::arrange(site_id, species, ef)



# Fish sampling site summary ------------------------------
# `tab_fish_sites_sum` object for `fpr_table_fish_site()`
tab_fish_sites_sum <- dplyr::left_join(fish_data_complete |>
                                         dplyr::group_by(local_name) |>
                                         dplyr::mutate(pass_total = max(pass_number)) |>
                                         dplyr::ungroup() |>
                                         dplyr::select(local_name, pass_total, enclosure),
                                       form_fiss_site |>
                                         dplyr::filter(!is.na(ef)) |>
                                         dplyr::select(local_name, gazetted_names, site_length, avg_wetted_width_m) |>
                                         dplyr::mutate(gazetted_names = stringr::str_trim(gazetted_names),
                                                        gazetted_names = stringr::str_to_title(gazetted_names)) ,
                                       by = "local_name"

  ) |>
  dplyr::distinct(local_name, .keep_all = TRUE) |>
  dplyr::rename(ef_length_m = site_length, ef_width_m = avg_wetted_width_m) |>
  dplyr::mutate(area_m2 = round(ef_length_m * ef_width_m,1)) |>
  dplyr::select(site = local_name, stream = gazetted_names, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)


## Fish sampling density results ------------------------------
# `fish_abund` object for `fpr_table_fish_density()` and `fpr_plot_fish_box()`
fish_abund <- dplyr::left_join(
  fish_data_complete |>
    # exclude visual observations
    dplyr::filter(sampling_method == "electrofishing") |>
    # Add life_stage and pass_total
    dplyr::mutate(
      life_stage = case_when(
        length <= 65 ~ 'fry',
        length > 65 & length <= 110 ~ 'parr',
        length > 110 & length <= 140 ~ 'juvenile',
        length > 140 ~ 'adult',
        TRUE ~ NA_character_
      ),
      life_stage = case_when(
        stringr::str_like(species, '%sculpin%') ~ NA_character_,
        TRUE ~ life_stage
      ),
      # Add pass_total here
      pass_total = max(pass_number)
    ) |>
    # Group and summarize
    dplyr::group_by(local_name, species, life_stage, pass_number,pass_total) |>
    dplyr::summarise(
      catch = n(),
      .groups = "drop" # Ensures the grouping is removed after summarizing
    ) |>
    # Add nfc_pass
    dplyr::mutate(
      catch = case_when(species == 'NFC' ~ 0L, TRUE ~ catch),
      nfc_pass = case_when(
        species != 'NFC' & pass_number == pass_total ~ FALSE,
        TRUE ~ TRUE
      ),
      nfc_pass = case_when(
        species == 'NFC' ~ TRUE,
        TRUE ~ nfc_pass
      )),

  form_fiss_site |>
    dplyr::filter(!is.na(ef)) |>
    dplyr::select(local_name, site, location, site_length, avg_wetted_width_m),

  by = "local_name"
  ) |>

  dplyr::rename(ef_length_m = site_length, ef_width_m = avg_wetted_width_m, species_code = species) |>
  dplyr::mutate(area_m2 = round(ef_length_m * ef_width_m,1),
                density_100m2 = round(catch/area_m2 * 100,1)) |>
  dplyr::select(local_name, site, location, species_code, life_stage, catch, density_100m2, nfc_pass)




## Cost Estimates ------------------------------

# join the road class and surface from `rd_class_surface` pulled from `bcfishpass` to the crossings
tab_cost_est_prep <-  dplyr::left_join(
  pscis_all |>
    dplyr::select(
      pscis_crossing_id,
      my_crossing_reference,
      aggregated_crossings_id,
      stream_name,
      road_name,
      downstream_channel_width_meters,
      barrier_result,
      fill_depth_meters,
      crossing_fix,
      habitat_value,
      recommended_diameter_or_span_meters,
      source),

  rd_class_surface |>
    dplyr::select(stream_crossing_id,
                  my_road_class,
                  my_road_surface),

  by = c('pscis_crossing_id' = 'stream_crossing_id')
) |>
  #add in pscis_crossing_id from `xref_pscis_my_crossing_modelled`
  dplyr::left_join(xref_pscis_my_crossing_modelled |>
                     dplyr::select(external_crossing_reference, stream_crossing_id) |>
                     dplyr::mutate(external_crossing_reference = as.numeric(external_crossing_reference)),

                   by = c('my_crossing_reference' = 'external_crossing_reference')
  )|>
  dplyr::mutate(pscis_crossing_id = dplyr::case_when(
    is.na(pscis_crossing_id) ~ as.integer(stream_crossing_id),
    TRUE ~ pscis_crossing_id
  )) |>
  dplyr::select(-stream_crossing_id)


# Phase 1 Cost Estimates
tab_cost_est_phase1 <- dplyr::left_join(
  # join the bridge costs and embedment costs
  tab_cost_est_prep |>
    # UNTIL PSCIS DATA IS IN, just do reassessments
    dplyr::filter(source == "pscis_reassessments.xlsm"),
  sfpr_xref_road_cost() |>
    dplyr::select(my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class','my_road_surface')
) |>
  dplyr::left_join(
    # join the crossing fix codes
    dplyr::select(fpr_xref_fix, crossing_fix, crossing_fix_code),

    by = c('crossing_fix')
) |>
  # calculate the cost estimates per 1000 square feet?
  dplyr::mutate(cost_est_1000s = dplyr::case_when(
    crossing_fix_code == 'SS-CBS' ~ cost_embed_cv,
    crossing_fix_code == 'OBS' ~ cost_m_1000s_bridge * recommended_diameter_or_span_meters)
  ) |>
  dplyr::mutate(cost_est_1000s = round(cost_est_1000s, 0)) |>
  # add in the upstream modelling data so we can estimate potential habitat gain
  dplyr::left_join(bcfishpass |>
                     dplyr::select(stream_crossing_id,
                            st_network_km,
                            st_belowupstrbarriers_network_km) |>
                     dplyr::mutate(stream_crossing_id = as.numeric(stream_crossing_id)),

                   by = c('pscis_crossing_id' = 'stream_crossing_id')
  ) |>
  dplyr::mutate(cost_net = round(st_belowupstrbarriers_network_km * 1000/cost_est_1000s, 1),
         cost_gross = round(st_network_km * 1000/cost_est_1000s, 1),
         cost_area_net = round((st_belowupstrbarriers_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1), ##this is a triangle area!
         cost_area_gross = round((st_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1)) |>   ##this is a triangle area!
  # add in the priority from form_pscis
  dplyr::left_join(form_pscis |>
                     dplyr::select(pscis_crossing_id, my_priority),

                   by = 'pscis_crossing_id') |>
  dplyr::arrange(pscis_crossing_id) |>
  dplyr::select(pscis_crossing_id,
               my_crossing_reference,
               stream_name,
               road_name,
               barrier_result,
               habitat_value,
               downstream_channel_width_meters,
               my_priority,
               crossing_fix_code,
               cost_est_1000s,
               st_network_km,
               cost_gross, cost_area_gross, source) |>
  dplyr::filter(barrier_result != 'Unknown' & barrier_result != 'Passable') |>
  dplyr::rename(
    `PSCIS ID` = pscis_crossing_id,
    `External ID` = my_crossing_reference,
    Priority = my_priority,
    Stream = stream_name,
    Road = road_name,
    Result = barrier_result,
    `Habitat value` = habitat_value,
    `Stream Width (m)` = downstream_channel_width_meters,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` =  cost_est_1000s,
    `Habitat Upstream (km)` = st_network_km,
    `Cost Benefit (m / $K)` = cost_gross,
    `Cost Benefit (m2 / $K)` = cost_area_gross)




## Phase 2 overview table ------------------------------------------

tab_overview_prep1 <- form_pscis|>
  dplyr::filter(assess_type_phase2 == "Yes") |>
  dplyr::select(pscis_crossing_id, stream_name, road_name, road_tenure, easting, northing, utm_zone, habitat_value)

tab_overview_prep2 <- habitat_confirmations_priorities|>
  dplyr::filter(location == 'us')|>
  dplyr::select(site, species_codes, upstream_habitat_length_m, priority, comments)|>
  dplyr::mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

tab_overview <- dplyr::left_join(
  tab_overview_prep1,
  tab_overview_prep2,
  by = c('pscis_crossing_id' = 'site')
)|>
  dplyr::mutate(utm = paste0(round(easting,0), ' ', round(northing,0)))|>
  dplyr::select(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Tenure = road_tenure,
         `UTM` = utm,
         `UTM zone` = utm_zone,
         `Fish Species` = species_codes,
         `Habitat Gain (km)` = upstream_habitat_length_km,
         `Habitat Value` = habitat_value,
         Priority = priority,
         Comments = comments )

rm(tab_overview_prep1, tab_overview_prep2)



