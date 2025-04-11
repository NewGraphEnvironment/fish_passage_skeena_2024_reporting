
# Read data from sqlite -------------------------------------------------

##this is the sqlite made in `0160-load-bcfishpass-data.R`
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")


readwritesqlite::rws_list_tables(conn)

# Bcfishpass crossing data for the watersheds in this project
bcfishpass <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)

# PSCIS assessment data for the watersheds in this project
pscis_assessment_svw <- readwritesqlite::rws_read_table("pscis_assessment_svw", conn = conn)

# bcfishpass modelling parameters for the spawning and rearing tables
bcfishpass_spawn_rear_model <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)

# Cross reference table for the `stream_crossing_id` and the `external_crossing_reference` which is the crossing id we assigned it.??
# Empty until data submission is accepted
xref_pscis_my_crossing_modelled <- readwritesqlite::rws_read_table("xref_pscis_my_crossing_modelled", conn = conn)

# Table containing rd cost multiplier
rd_class_surface <- readwritesqlite::rws_read_table("rd_class_surface", conn = conn)

# Table containing photo metadata. Used in the iteractive map
photo_metadata <- readwritesqlite::rws_read_table("photo_metadata", conn = conn)

#Read in the form_pscis
form_pscis <- readwritesqlite::rws_read_table("form_pscis", conn = conn)

#Read in the form_fiss_site
form_fiss_site <- readwritesqlite::rws_read_table("form_fiss_site", conn = conn)

#Read in the form_monitoring
form_monitoring <- readwritesqlite::rws_read_table("form_monitoring", conn = conn)

#Read in the cleaned habitat confirmation gps tracks
habitat_confirmation_tracks <- readwritesqlite::rws_read_table("habitat_confirmation_tracks", conn = conn)

#Read in the uav imagery table
project_uav <- readwritesqlite::rws_read_table("project_uav", conn = conn)

#Read in the watershed polygons for the watersheds included in the project study area
wshd_study_areas <- readwritesqlite::rws_read_table("wshd_study_areas", conn = conn)


# You must run `0170-load-wshd_stats.R` at before this
# watershed stats for the watersheds in this project
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn) |>
  # remove any negative values
  dplyr::mutate(dplyr::across(contains('elev'), ~ replace(., . < 0, NA))) |>
  dplyr:: mutate(aspect = as.character(aspect))



readwritesqlite::rws_disconnect(conn)

