
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


## Do we still use this objects? Try to clean up as you go Lucy
# form_pscis_raw <- readwritesqlite::rws_read_table("form_pscis_raw", conn = conn) |>
#   sf::st_drop_geometry()


# You must run `0170-load-wshd_stats.R` at before this
# watershed stats for the watersheds in this project
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn) |>
  # remove any negative values
  dplyr::mutate(dplyr::across(contains('elev'), ~ replace(., . < 0, NA))) |>
  dplyr:: mutate(aspect = as.character(aspect))



readwritesqlite::rws_disconnect(conn)

