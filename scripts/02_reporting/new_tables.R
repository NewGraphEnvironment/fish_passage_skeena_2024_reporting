## new tables.R

## Params -------------------------------------------------

path_form_fiss_site <- fs::path('~/Projects/gis/sern_skeena_2023/data_field/2024/form_fiss_site_2024.gpkg')



## Load PSCIS data -------------------------------------------------

# What we should be doing here is just reading in form_pscis_2024.gpkg which should have site elevations, barrier scores,
# and replacement structure, size, type, see issue: https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/56

# import data and build tables we for reporting
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



## priorities phase 1 ------------------------------------------------------

##uses habitat value to initially screen but then refines based on what are likely not barriers to most most the time
phase1_priorities <- pscis_all %>%
  dplyr::filter(!source %ilike% 'phase2') %>%
  dplyr::select(aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, utm_zone:northing, habitat_value, barrier_result, source) %>%
  dplyr::mutate(priority_phase1 =  dplyr::case_when(habitat_value == 'High' & barrier_result != 'Passable' ~ 'high',
                                     habitat_value == 'Medium' & barrier_result != 'Passable' ~ 'mod',
                                     habitat_value == 'Low' & barrier_result != 'Passable' ~ 'low',
                                     TRUE ~ NA_character_)) %>%
  dplyr::mutate(priority_phase1 =  dplyr::case_when(habitat_value == 'High' & barrier_result == 'Potential' ~ 'mod',
                                     TRUE ~ priority_phase1)) %>%
  dplyr::mutate(priority_phase1 =  dplyr::case_when(habitat_value == 'Medium' & barrier_result == 'Potential' ~ 'low',
                                     TRUE ~ priority_phase1)) %>%
  # mutate(priority_phase1 = case_when(my_crossing_reference == 99999999999 ~ 'high', ##this is where we can make changes to the defaults
  #                                    TRUE ~ priority_phase1)) %>%
  dplyr::rename(utm_easting = easting, utm_northing = northing)


##turn spreadsheet into list of data frames
pscis_phase1_for_tables <- pscis_all %>%
  dplyr::filter(source %ilike% 'phase1' |
           source %ilike% 'reassessments' ) %>%
  arrange(pscis_crossing_id) %>%
  # because we have reassessments too
  mutate(site_id = case_when(is.na(my_crossing_reference) ~ pscis_crossing_id,
                             TRUE ~ my_crossing_reference))


pscis_split <- pscis_phase1_for_tables  %>% #pscis_phase1_reassessments
  # sf::st_drop_geometry() %>%
  # mutate_if(is.numeric, as.character) %>% ##added this to try to get the outlet drop to not disapear
  # tibble::rownames_to_column() %>%
  dplyr::group_split(pscis_crossing_id) %>%
  purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)

##make result summary tables for each of the crossings
tab_summary <- pscis_split %>%
  purrr::map(fpr::fpr_table_cv_detailed)

tab_summary_comments <- pscis_split %>%
  purrr::map(fpr::fpr_table_cv_detailed_comments)

##had a hickup where R cannot handle the default size of the integers we used for numbers so we had to change site names!!
tab_photo_url <- list.files(path = paste0(getwd(), '/data/photos/'), full.names = TRUE) %>%
  basename() %>%
  as_tibble() %>%
  mutate(value = as.integer(value)) %>%  ##need this to sort
  dplyr::arrange(value)  %>%
  mutate(photo = paste0('![](data/photos/', value, '/crossing_all.JPG)')) %>%
  dplyr::filter(value %in% pscis_phase1_for_tables$site_id) %>%
  left_join(., xref_pscis_my_crossing_modelled, by = c('value' = 'external_crossing_reference'))  %>%  ##we need to add the pscis id so that we can sort the same
  mutate(stream_crossing_id = case_when(is.na(stream_crossing_id) ~ value, TRUE ~ stream_crossing_id)) %>%
  arrange(stream_crossing_id) %>%
  dplyr::group_split(stream_crossing_id)


#   # purrr::set_names(nm = . %>% bind_rows() %>% arrange(value) %>% pull(stream_crossing_id)) %>%
#   # bind_rows()
#   # arrange(stream_crossing_id) %>%
#   # dplyr::group_split(value)
#
#
# html tables
tabs_phase1 <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url)


# html tables for the pdf version
# tabs_phase1_pdf <- mapply(
#   fpr::fpr_table_cv_detailed_print,
#   tab_sum = tab_summary,
#   comments = tab_summary_comments,
#   photos = tab_photo_url,
#   gitbook_switch = FALSE
#   ) %>%
#   head()
# fpr_print_tab_summary_all_pdf <- function(tab_sum, comments, photos){
#   kable(tab_sum, booktabs = TRUE) %>%
#     kableExtra::kable_styling(c("condensed"), full_width = TRUE, font_size = 11) %>%
#     kableExtra::add_footnote(label = paste0('Comments: ', comments[[1]]), notation = 'none') %>% #this grabs the comments out
#     kableExtra::add_footnote(label = paste0('Photos: PSCIS ID ', photos[[2]],
#                                             '. From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
#                                             photos[[1]]), notation = 'none') %>%
#     kableExtra::add_footnote(label = '<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>', escape = FALSE, notation = 'none')
# }

tabs_phase1_pdf <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url,
  gitbook_switch = FALSE)

# tabs_phase1_pdf <- mapply(fpr_print_tab_summary_all_pdf, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)


