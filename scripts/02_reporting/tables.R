## new tables.R

# Parameters -------------------------------------------------

# path to form_pscis_2024
path_form_pscis <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_pscis_2024.gpkg')

# path to NEW `form_fiss_site_2024` made from `0205_fiss_extract_inputs.Rmd`
path_form_fiss_site <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_fiss_site_2024.gpkg')

# Onedrive path to the fish data with the pit tags joined.
path_fish_tags_joined <-  fs::path_expand('~/Projects/repo/fish_passage_skeena_2024_reporting/data/fish_data_tags_joined.csv')

# specify which project data we want. for this case `2024-073-sern-peace-fish-passage`
project = "2024-072-sern-skeena-fish-passage"

# specify the repo
repo_name <- "fish_passage_skeena_2024_reporting"

# This is used in the table captions
# specify in index.Rmd YAML which species you want to use for the modelling
# For Skeena we use steelhead
# For Peace we use bull trout
model_species_name <- dplyr::case_when(params$model_species == "bt" ~ "Bull trout",
                                       params$model_species == "st" ~ "Steelhead")



# Load data -------------------------------------------------

## Reload form_pscis -------------------------------------------------

# form_pscis gets read in from `02_reporting/0165-read-sqlite.R`

# If update_form_pscis = TRUE then load form_pscis to sqlite - need to load the params from `index.Rmd`
if (params$update_form_pscis) {
  form_pscis <- fpr::fpr_sp_gpkg_backup(
    path_gpkg = path_form_pscis,
    dir_backup = "data/backup/",
    update_utm = TRUE,
    update_site_id = FALSE, ## This now also checks for duplicates
    write_back_to_path = FALSE,
    write_to_csv = FALSE,
    write_to_rdata = FALSE,
    return_object = TRUE)


  conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
  # won't run on first build if the table doesn't exist
  readwritesqlite::rws_drop_table("form_pscis", conn = conn)
  readwritesqlite::rws_write(form_pscis, exists = F, delete = TRUE,
                             conn = conn, x_name = "form_pscis")
  readwritesqlite::rws_disconnect(conn)
}


## Reload form_fiss_site -------------------------------------------------

# form_fiss_site data gets read in from `02_reporting/0165-read-sqlite.R`

# If update_form_fiss_site = TRUE then load form_fiss_site to sqlite - need to load the params from `index.Rmd`
if (params$update_form_fiss_site) {
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


  conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
  # won't run on first build if the table doesn't exist
  readwritesqlite::rws_drop_table("form_fiss_site", conn = conn)
  readwritesqlite::rws_write(form_fiss_site, exists = F, delete = TRUE,
                             conn = conn, x_name = "form_fiss_site")
  readwritesqlite::rws_disconnect(conn)
}



## Load PSCIS spreadsheets -------------------------------------------------

# For now, import data and build tables we for reporting
pscis_list <- fpr::fpr_import_pscis_all()
pscis_phase1 <- pscis_list |> purrr::pluck('pscis_phase1')
pscis_phase2 <- pscis_list |> purrr::pluck('pscis_phase2') |>
  dplyr::arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list |> purrr::pluck('pscis_reassessments')
pscis_all_prep <- pscis_list |>
  dplyr::bind_rows()



# Used in many other parts of the script
pscis_all <- dplyr::left_join(
  pscis_all_prep,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) |>
  dplyr::mutate(pscis_crossing_id = dplyr::case_when(
    is.na(pscis_crossing_id) ~ as.numeric(stream_crossing_id),
    TRUE ~ pscis_crossing_id
  )) |>
  dplyr::arrange(pscis_crossing_id)


## Load fish data -------------------------------------------------

fish_data_complete <- readr::read_csv(file = path_fish_tags_joined) |>
  janitor::clean_names() |>
  #filter for skeena 2024
  dplyr::filter(project_name == project)


# Bcfishpass modelling table setup for reporting --------------------------

xref_bcfishpass_names <- fpr::fpr_xref_crossings



# Habitat Summaries -------------------------------------------------

## Build hab_site object for fpr_my_habitat_info() ------------------
hab_site <- form_fiss_site


## Build tab_hab_summary object for tables --------------------------

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



# Phase 1 Appendix ----------------------------------------------

## make result summary tables for each of the crossings, used to display phase 1 data in the appendix

## turn spreadsheet into list of data frames
pscis_phase1_for_tables <- pscis_all |>
  dplyr::filter(!source == "pscis_phase2.xlsm") |>
  dplyr::arrange(pscis_crossing_id)

pscis_split <- pscis_phase1_for_tables  |>
  dplyr::group_split(pscis_crossing_id) |>
  purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)


##make result summary tables for each of the crossings
tab_summary <- pscis_split |>
  purrr::map(fpr::fpr_table_cv_detailed)

tab_summary_comments <- pscis_split |>
  purrr::map(fpr::fpr_table_cv_detailed_comments)


tab_photo_url <- fs::dir_ls(path = "data/photos/", recurse = FALSE) |>
  basename() |>
  tibble::as_tibble() |>
  ## for skeena 2024 - remove folder buck_falls
  dplyr::filter(!value == "buck_falls") |>
  dplyr::mutate(value = as.integer(value)) |>  # Convert filenames to integers for sorting
  dplyr::arrange(value) |>
  dplyr::mutate(photo = paste0("![](data/photos/", value, "/crossing_all.JPG)")) |>
  dplyr::filter(value %in% pscis_phase1_for_tables$site_id) |>
  dplyr::left_join(xref_pscis_my_crossing_modelled,
                   by = c("value" = "external_crossing_reference")) |>
  dplyr::mutate(stream_crossing_id = dplyr::case_when(
    is.na(stream_crossing_id) ~ value,
    TRUE ~ stream_crossing_id
  )) |>
  dplyr::arrange(stream_crossing_id) |>
  dplyr::group_split(stream_crossing_id)


# used to build tables for PDF version of the report
tabs_phase1_pdf <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url,
  gitbook_switch = FALSE)


rm(pscis_phase1_for_tables,pscis_split)


# Phase 2 Priority spreadsheet ----------------------------------------------

# Read priority spreadsheet
# spreadsheet that includes site lengths, surveyors initials, time, priority for remediation, updated fish species (if changed from my_fish_sp())

# read in the object
habitat_confirmations_priorities <- readr::read_csv(
  file = "data/habitat_confirmations_priorities.csv")


# Phase 2 overview table ------------------------------------------

# Overview of habitat confirmation sites used in the results section

tab_overview_prep1 <- form_pscis|>
  sf::st_drop_geometry() |>
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



# Fish sampling ----------------------------------------------

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



## Fish sampling site summary ------------------------------
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




# Cost Estimates ------------------------------

# General preparation for cost estimates. Phase 1 and Phase 2 specific code in farther down.

# Step 1: Join the road class and surface data from `rd_class_surface` to the crossings
tab_cost_est_prep <- dplyr::left_join(
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
    dplyr::select(stream_crossing_id, my_road_class, my_road_surface),
  by = c('pscis_crossing_id' = 'stream_crossing_id')
)|>
  ## Unique to Skeena 2024 - two phase 2 sites (58245 and 58245) are missing `my_road_class` and `my_road_surface`, so lets add then by hand so we get a cost estimate.
  dplyr::mutate(my_road_class = dplyr::case_when(pscis_crossing_id == "58245" ~ "collector", TRUE ~ my_road_class),
                my_road_surface = dplyr::case_when(pscis_crossing_id == "58245" ~ "paved", TRUE ~ my_road_surface))

# Step 2: Add `pscis_crossing_id` from `xref_pscis_my_crossing_modelled`
tab_cost_est_prep1 <- dplyr::left_join(
  tab_cost_est_prep,
  xref_pscis_my_crossing_modelled |>
    dplyr::select(external_crossing_reference, stream_crossing_id) |>
    dplyr::mutate(external_crossing_reference = as.numeric(external_crossing_reference)),
  by = c('my_crossing_reference' = 'external_crossing_reference')
) |>
  dplyr::mutate(pscis_crossing_id = dplyr::case_when(
    is.na(pscis_crossing_id) ~ as.integer(stream_crossing_id),
    TRUE ~ pscis_crossing_id
  )) |>
  dplyr::select(-stream_crossing_id)

# Step 3: Join the bridge costs and embedment costs
tab_cost_est_prep2 <- dplyr::left_join(
  tab_cost_est_prep1,
  sfpr_xref_road_cost() |>
    dplyr::select(my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class', 'my_road_surface')
)

# Step 4: Join the crossing fix codes
tab_cost_est_prep3 <- dplyr::left_join(
  tab_cost_est_prep2,
  dplyr::select(fpr_xref_fix, crossing_fix, crossing_fix_code),
  by = c('crossing_fix')
)

# Step 5: Calculate the cost estimates per 1000 square feet
tab_cost_est_prep4 <- tab_cost_est_prep3 |>
  dplyr::mutate(cost_est_1000s = dplyr::case_when(
    crossing_fix_code == 'SS-CBS' ~ cost_embed_cv,
    crossing_fix_code == 'OBS' ~ cost_m_1000s_bridge * recommended_diameter_or_span_meters)
  ) |>
  dplyr::mutate(cost_est_1000s = round(cost_est_1000s, 0))



## Phase 1  ------------------------------

# Now prepare phase 1 cost estimates.

sp_network_km <- rlang::sym(paste0(params$model_species, "_network_km"))
sp_belowupstrbarriers_network_km <- rlang::sym(paste0(params$model_species, "_belowupstrbarriers_network_km"))


# Step 6: Add upstream modelling data to estimate potential habitat gain
tab_cost_est_prep5 <- dplyr::left_join(
  tab_cost_est_prep4,
  bcfishpass |>
    dplyr::select(stream_crossing_id, !!sp_network_km, !!sp_belowupstrbarriers_network_km) |>
    dplyr::mutate(stream_crossing_id = as.numeric(stream_crossing_id)),
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) |>
  dplyr::mutate(
    cost_net = round(!!sp_belowupstrbarriers_network_km * 1000 / cost_est_1000s, 1),
    cost_gross = round(!!sp_network_km * 1000 / cost_est_1000s, 1),
    cost_area_net = round((!!sp_belowupstrbarriers_network_km * 1000 * downstream_channel_width_meters * 0.5) / cost_est_1000s, 1),
    cost_area_gross = round((!!sp_network_km * 1000 * downstream_channel_width_meters * 0.5) / cost_est_1000s, 1),
    st_network_km = round(!!sp_network_km, 1)
  )


# Step 7: Add the priority from `form_pscis`
tab_cost_est_prep6 <- dplyr::left_join(
  tab_cost_est_prep5 |>
    # only for skeena 2024 where the road names are not capitalized in the spreadsheet because I forgot:/
    dplyr::select(-road_name),
  form_pscis |>
    dplyr::select(pscis_crossing_id, my_priority, road_name),
  by = 'pscis_crossing_id'
) |>
  dplyr::arrange(pscis_crossing_id) |>
  dplyr::select(
    pscis_crossing_id,
    my_crossing_reference,
    stream_name,
    road_name,
    barrier_result,
    habitat_value,
    sp_network_km,
    downstream_channel_width_meters,
    my_priority,
    crossing_fix_code,
    cost_est_1000s,
    cost_gross, cost_area_gross, source
  ) |>
  dplyr::filter(barrier_result != 'Unknown' & barrier_result != 'Passable')

# Step 8: Final adjustments and renaming columns
tab_cost_est_phase1 <- tab_cost_est_prep6 |>
  dplyr::rename(
    `PSCIS ID` = pscis_crossing_id,
    `External ID` = my_crossing_reference,
    Priority = my_priority,
    Stream = stream_name,
    Road = road_name,
    `Barrier Result` = barrier_result,
    `Habitat value` = habitat_value,
    `Habitat Upstream (km)` = sp_network_km,
    `Stream Width (m)` = downstream_channel_width_meters,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` = cost_est_1000s,
    `Cost Benefit (m / $K)` = cost_gross,
    `Cost Benefit (m2 / $K)` = cost_area_gross
  ) |>
  dplyr::filter(!source == "pscis_phase2.xlsm") |>
  dplyr::select(-source)



## Phase 2 ------------------------------

# Now prepare phase 2 cost estimates.

# Step 1: Join habitat confirmation priorities data to upstream habitat length
tab_cost_est_prep7 <- dplyr::left_join(
  tab_cost_est_prep4,
  dplyr::select(
    dplyr::filter(habitat_confirmations_priorities, location == 'us'),
    site,
    upstream_habitat_length_m
  ),
  by = c('pscis_crossing_id' = 'site')
) |>
  dplyr::mutate(
    cost_net = round(upstream_habitat_length_m * 1000 / cost_est_1000s, 1),
    cost_area_net = round((upstream_habitat_length_m * 1000 * downstream_channel_width_meters * 0.5) / cost_est_1000s, 1) ## this is a triangle area!
  )

# Step 2: Join habitat site data for average channel width using stringr for pattern matching
tab_cost_est_prep8 <- dplyr::left_join(
  tab_cost_est_prep7,
  dplyr::select(
    hab_site |>
      dplyr::filter(
        !stringr::str_detect(local_name, 'ds') &
          !stringr::str_detect(local_name, 'ef') &
          !stringr::str_detect(local_name, '\\d$')
      ),
    site, avg_channel_width_m
  ),
  by = c('pscis_crossing_id' = 'site')
)


# Step 3: Filter and select relevant columns for Phase 2 cost estimates
tab_cost_est_prep9 <- tab_cost_est_prep8 |>
  dplyr::filter(source == "pscis_phase2.xlsm") |>
  dplyr::select(
    pscis_crossing_id,
    stream_name,
    road_name,
    barrier_result,
    habitat_value,
    avg_channel_width_m,
    crossing_fix_code,
    cost_est_1000s,
    upstream_habitat_length_m,
    cost_net,
    cost_area_net,
    source
  )

# Step 4: Prepare the Phase 2 cost estimates for the table
tab_cost_est_phase2 <- tab_cost_est_prep9 |>
  dplyr::arrange(pscis_crossing_id) |>
  dplyr::select(-source) |>
  dplyr::rename(
    `PSCIS ID` = pscis_crossing_id,
    Stream = stream_name,
    Road = road_name,
    `Barrier Result` = barrier_result,
    `Habitat value` = habitat_value,
    `Habitat Upstream (m)` = upstream_habitat_length_m,
    `Stream Width (m)` = avg_channel_width_m,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` = cost_est_1000s,
    `Cost Benefit (m / $K)` = cost_net,
    `Cost Benefit (m2 / $K)` = cost_area_net
  )

# Clean up unnecessary objects
rm(tab_cost_est_prep,
   tab_cost_est_prep1,
   tab_cost_est_prep2,
   tab_cost_est_prep3,
   tab_cost_est_prep4,
   tab_cost_est_prep5,
   tab_cost_est_prep6,
   tab_cost_est_prep7,
   tab_cost_est_prep8)



# Map Tables --------------------------------------------------------------

## Phase 1 --------------------------------------------------------------

tab_map_phase_1_prep <- dplyr::left_join(form_pscis |>
                                           dplyr::select(-c(barrier_result, source)),
                                         pscis_all |>
                                   dplyr::select(pscis_crossing_id, barrier_result, source),
                                 by = c('pscis_crossing_id')) |>
  dplyr::select(pscis_crossing_id,
                my_crossing_reference,
                utm_zone,
                utm_easting = easting,
                utm_northing = northing,
                stream_name,
                road_name,
                site_id,
                priority_phase1 = my_priority,
                habitat_value,
                barrier_result,
                source) |>
  # we must transform the data to latitude/longitude (CRS 4326)
  sf::st_transform(4326)




tab_map_phase_1 <- tab_map_phase_1_prep |>
  dplyr::mutate(priority_phase1 = dplyr::case_when(priority_phase1 == 'mod' ~ 'moderate',
                                     TRUE ~ priority_phase1),
                priority_phase1 = stringr::str_to_title(priority_phase1)) |>
  dplyr::mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) |>
  dplyr::mutate(photo_link = dplyr::case_when(is.na(my_crossing_reference) ~ paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/', repo_name, '/main/data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
                                                                                    'target="_blank">Culvert Photos</a>'),
                                              TRUE ~ paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/', repo_name, '/main/data/photos/', my_crossing_reference, '/crossing_all.JPG ',
                                                            'target="_blank">Culvert Photos</a>'))) |>
  dplyr::mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>')) |>
  dplyr::distinct(site_id, .keep_all = TRUE) #just for now



## Phase 2 --------------------------------------------------------------

#please note that the photos are only in those files because they are referenced in other parts of the document
tab_map_phase_2 <- dplyr::left_join(
  tab_cost_est_prep9,
  form_fiss_site |>
    dplyr::filter(is.na(ef) & location == "us") |>
    dplyr::select(site, utm_zone, easting = utm_easting, northing = utm_northing, comments),
  by = c('pscis_crossing_id' = 'site')
) |>
  # we must transform the data to latitude/longitude (CRS 4326)
  fpr::fpr_sp_assign_sf_from_utm() |>
  sf::st_transform(4326) |>
  # We don't have a priority ranking for the hab con sites at the moment so just just add the priority from the phase 1 assessments.
  dplyr::left_join(tab_map_phase_1 |>
                     sf::st_drop_geometry() |>
                     dplyr::select(pscis_crossing_id, priority = priority_phase1),
                   by = 'pscis_crossing_id') |>

  # Update the data link to point to the new location in docs
  dplyr::mutate(
    data_link = paste0(
      '<a href =',
      'sum/cv/', pscis_crossing_id,
      '.html ', 'target="_blank">Culvert Data</a>'
    )
  ) |>
  dplyr::mutate(
    model_link = paste0(
      '<a href =',
      'sum/bcfp/', pscis_crossing_id,
      '.html ', 'target="_blank">Model Data</a>'
    )
  ) |>
  dplyr::mutate(
    photo_link = paste0(
      '<a href =',
      'https://raw.githubusercontent.com/NewGraphEnvironment/fish_passage_skeena_2024_reporting/main/data/photos/',
      pscis_crossing_id, '/crossing_all.JPG ',
      'target="_blank">Culvert Photos</a>'
    )
  )
