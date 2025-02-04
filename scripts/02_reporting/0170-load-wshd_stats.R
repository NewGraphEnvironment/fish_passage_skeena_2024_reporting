# retrieve the watershed stats and elevations of the pscis sites then burn to the sqlite

# Load required objects -------------------------------------------------

# `0165-read-sqlite.R` reads in the `bcfishpass` object
source("scripts/02_reporting/0165-read-sqlite.R")

# `tables.R` reads in the `form_pscis_2024` object
source('scripts/02_reporting/tables.R')



## Filter the bcfishpass data to just the phase 2 sites -------------------------------------------------
bcfishpass_phase2 <- bcfishpass |>
  dplyr::filter(
    stringr::str_detect(
      stream_crossing_id,
      paste0(pscis_phase2 |>
               pull(pscis_crossing_id),
             collapse = "|")
    ))


## Remove crossings on first order streams -------------------------------------------------
# we needed to remove crossings that are first order because the fwapgr api kicks us off

bcfishpass_phase2_clean <- bcfishpass_phase2 |>
  dplyr::filter(stream_order != 1)

# for this years data there is none
bcfishpass_phase2_1st_order <- bcfishpass_phase2 |>
  dplyr::filter(stream_order == 1)



## Remove first order crossings if needed -------------------------------------------------

# conn <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = dbname,
#   host = host,
#   port = port,
#   user = user,
#   password = password
# )
#
# dat <- bcfishpass_phase2 |>
#   filter(stream_order == 1)
#   # distinct(localcode_ltree, .keep_all = T)
#
# # add a unique id - we could just use the reference number
# dat$misc_point_id <- seq.int(nrow(dat))
#
#
# # # lets get our fist order watersheds
# # dat <- bcfishpass_phase2 |>
# #   filter(stream_order == 1)
#
# ##pull out the localcode_ltrees we want
# ids <-  dat |>
#   pull(localcode_ltree) |>
#   # unique() |>
#   as_vector() |>
#   na.omit()
#
# ids2 <- dat |>
#   pull(wscode_ltree) |>
#   # unique() |>
#   as_vector() |>
#   na.omit()
#
# # note that we needed to specifiy the order here.  Not sure that is always going to save us....
# sql <- glue::glue_sql(
#
#                                 "SELECT localcode_ltree, wscode_ltree, area_ha, geom as geometry from whse_basemapping.fwa_watersheds_poly
#                                 WHERE localcode_ltree IN ({ids*})
#                                 AND wscode_ltree IN ({ids2*})
#                                 AND watershed_order = 1
#                                 ",
#   .con = conn
# )
#
# wshds_1ord_prep <- sf::st_read(conn,
#                         query = sql) |>
#   st_transform(crs = 4326)
#
#
# # test <- wshds_1ord_prep |>
# #   filter(localcode_ltree == wscode_ltree)
#
#
# # i think we need to be joining in the stream_crossing_id and joining on that....
#
# wshds_1ord <- left_join(
#   dat |>
#     distinct(stream_crossing_id, .keep_all = T) |>
#     select(
#     stream_crossing_id,
#     localcode_ltree,
#     wscode_ltree,
#     stream_order
#     ) |>
#     mutate(stream_crossing_id = as.character(stream_crossing_id)),
#     # filter(stream_order == 1),
#   wshds_1ord_prep |>
#     mutate(localcode_ltree = as.character(localcode_ltree),
#            wscode_ltree = as.character(wscode_ltree)),
#   by = c('localcode_ltree','wscode_ltree')
# )



## Extract the watershed data -------------------------------------------------

# call fwapgr
wshds_fwapgr <- fpr::fpr_sp_watershed(bcfishpass_phase2_clean)

# If there was first order watersheds, then combine the following:
# wshds_combined <- bind_rows(
#   wshds_fwapgr,
#   wshds_1ord
# )



## Calculate the watershed stats -------------------------------------------------
wshds_raw <- fpr::fpr_sp_wshd_stats(dat = wshds_fwapgr) |>
  dplyr::mutate(area_km = round(area_ha/100, 1)) |>
  dplyr::mutate(dplyr::across(contains('elev'), round, 0)) |>
  dplyr::arrange(stream_crossing_id)



## Add the site elevations -------------------------------------------------

# This should eventually get done in `0130_pscis_export_to_template.Rmd`, see issue  https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/56
# extract the site elevations
pscis_all_sf <- form_pscis |>
  dplyr::group_split(source) |>
  purrr::map(sfpr_get_elev) |>
  dplyr::bind_rows()


# add in the site elevations to the watershed stats
wshds <-  dplyr::left_join(
  wshds_raw |> dplyr::mutate(stream_crossing_id = as.numeric(stream_crossing_id)),

  pscis_all_sf |> dplyr::distinct(pscis_crossing_id, .keep_all = T) |>
    sf::st_drop_geometry() |>
    dplyr::select(pscis_crossing_id, elev_site = elev),

  by = c('stream_crossing_id' = 'pscis_crossing_id')) |>
  # put elev_site before elev_min
  dplyr::relocate(elev_site, .before = elev_min)



## Add to the geopackage -------------------------------------------------
path_gis_wshds <- fs::path("~/Projects/gis/sern_skeena_2023/data_field/2024/fishpass_mapping.gpkg")

wshds |>
  sf::st_write(dsn = path_gis_wshds,
               layer = 'hab_wshds',
               delete_layer = T,
               append = F) ##might want to f the append....



## Burn to a kml -------------------------------------------------
#burn to kml as well so we can see elevations
sf::st_write(wshds |>
        rename(name = stream_crossing_id),
         append = F,
         delete_layer = T,
         driver = 'kml',
         dsn = "data/inputs_extracted/wshds.kml")



## Add to the sqlite -------------------------------------------------
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_drop_table("wshds", conn = conn) ##now drop the table so you can replace it
readwritesqlite::rws_write(wshds, exists = F, delete = TRUE,
          conn = conn, x_name = "wshds")
readwritesqlite::rws_list_tables(conn)
readwritesqlite::rws_disconnect(conn)
