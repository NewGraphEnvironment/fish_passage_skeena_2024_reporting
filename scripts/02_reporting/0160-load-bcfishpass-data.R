# load the sqlite database with bcfishpass and other info-----------------------------------------------------------------------------------------------------

# this is the name of the funding project we used to submit our phase 1 data to the province.  we use it to filter the raw
# pscis data for our entire study area to obtain just the data we submitted. We use it to filter xref_pscis_my_crossing_modelled
# but not sure that filtering is actually necessary - we could test and remove if it is not
my_funding_project_number = "peace_2023_Phase1"


# name the watershed groups in our study area
wsg <- c('PARS', 'CARP', 'CRKD', 'NATR')

# this object should be called bcfishpass.crossings_vw or something that better reflects what it is
bcfishpass <- fpr::fpr_db_query(
  glue::glue(
    "SELECT * from bcfishpass.crossings_vw
  WHERE watershed_group_code IN (
  {glue::glue_collapse(glue::single_quote(wsg), sep = ', ')}
  );"
  )
) |>
  sf::st_drop_geometry()

# grab the bcfishpass spawning and rearing table and put in the database so it can be used to populate the methods
# like solutions provided here https://github.com/smnorris/bcfishpass/issues/490
bcfishpass_spawn_rear_model <- fpr::fpr_db_query(
  query = "SELECT * FROM bcfishpass.log_parameters_habitat_thresholds
  WHERE model_run_id = (SELECT MAX(model_run_id)
  FROM bcfishpass.log_parameters_habitat_thresholds);"
)

# get all the pscis data for the watershed from the database which is updated weekly on our server
# could consider nameing more effectively in the future
pscis_assessment_svw <- fpr::fpr_db_query(
  glue::glue(
    "SELECT p.*, wsg.watershed_group_code
   FROM whse_fish.pscis_assessment_svw p
   INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
   ON ST_Intersects(wsg.geom,p.geom)
  WHERE wsg.watershed_group_code IN (
  {glue::glue_collapse(glue::single_quote(wsg), sep = ', ')}
  );"
  )
)

xref_pscis_my_crossing_modelled <- pscis_assessment_svw |>
  dplyr::filter(funding_project_number == my_funding_project_number) |>
  dplyr::select(external_crossing_reference, stream_crossing_id) |>
  dplyr::mutate(external_crossing_reference = as.numeric(external_crossing_reference)) |>
  dplyr::arrange(external_crossing_reference) |>
  sf::st_drop_geometry()


# Initiliaze the database-----------------------------------------------------------------------------------------------------
### RUN FIRST TIME ONLY ###
# mydb <- DBI::dbConnect(RSQLite::SQLite(), "data/bcfishpass.sqlite")

# burn to sqlite-----------------------------------------------------------------------------------------------------
## time format format(Sys.time(), "%Y%m%d-%H%M%S")
## May need to drop tables if this has been done before
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)

# we need to drop the tables if they exist so we can replace them
readwritesqlite::rws_drop_table("bcfishpass", conn = conn)
readwritesqlite::rws_write(bcfishpass, exists = F, delete = TRUE,
                           conn = conn, x_name = "bcfishpass")

readwritesqlite::rws_drop_table("bcfishpass_spawn_rear_model", conn = conn)
readwritesqlite::rws_write(bcfishpass_spawn_rear_model, exists = FALSE, delete = TRUE,
                           conn = conn, x_name = "bcfishpass_spawn_rear_model")

readwritesqlite::rws_drop_table("pscis_assessment_svw", conn = conn)
readwritesqlite::rws_write(pscis_assessment_svw, exists = F, delete = TRUE,
                           conn = conn, x_name = "pscis_assessment_svw")

readwritesqlite::rws_drop_table("xref_pscis_my_crossing_modelled", conn = conn)
readwritesqlite::rws_write(xref_pscis_my_crossing_modelled, exists = F, delete = TRUE,
                           conn = conn, x_name = "xref_pscis_my_crossing_modelled")

# building the comments no longer works so we have `fpr::fpr_xref_crossings` - https://github.com/smnorris/bcfishpass/issues/492 -in the meantime
# !better way to rename table is ?readwritesqlite::rws_rename_table
# bcfishpass_column_comments_archive <- readwritesqlite::rws_read_table("bcfishpass_column_comments", conn = conn)
# rws_write(bcfishpass_column_comments_archive, exists = F, delete = TRUE,
#           conn = conn, x_name = paste0("bcfishpass_column_comments_archive_", format(Sys.time(), "%Y-%m-%d-%H%m")))
# readwritesqlite::rws_drop_table("bcfishpass_column_comments", conn = conn) ##now drop the table so you can replace it
# readwritesqlite::rws_write(bcfishpass_column_comments, exists = F, delete = TRUE,
#           conn = conn, x_name = "bcfishpass_column_comments")
# This one is not made but is made like here by matching our field data to `bcfishpass.crossings_vw`
# https://github.com/NewGraphEnvironment/fish_passage_template_reporting/blob/main/scripts/tutorials/road_tenure.Rmd
# readwritesqlite::rws_write(xref_pscis_my_crossing_modelled, exists = F, delete = TRUE,
#                            conn = conn, x_name = "xref_pscis_my_crossing_modelled")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)


