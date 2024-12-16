source('scripts/packages.R')

wshd_codes <- c('MORR', 'ZYMO', 'KISP', 'BULK', 'KLUM')

conn <- fpr_db_conn()
##get the observations from the fiss layer
fish_species_watershed <- fpr::fpr_db_query(query = glue::glue_sql("SELECT DISTINCT ws.watershed_group_code, x.species_code,x.species_name
                   FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
                   INNER JOIN
                   whse_basemapping.fwa_watershed_groups_poly ws
                   ON ST_intersects(x.geom, ws.geom)
                   WHERE ws.watershed_group_code IN ({wshd_codes*})",
                                                                   .con = conn))

DBI::dbDisconnect(conn)

# split data frame into list of data frames based on watershed group code
fish_spp_prep <- fish_species_watershed |>
  group_split(watershed_group_code)

# fish_spp_prep <- list(fish_species_watershed %>% filter(watershed_group_code == 'MORR')%>% rename(Morice = watershed_group_code),
#                       fish_species_watershed %>% filter(watershed_group_code == 'ZYMO')%>% rename(Zymoetz = watershed_group_code),
#                       fish_species_watershed %>% filter(watershed_group_code == 'KISP')%>% rename(Kispiox = watershed_group_code),
#                       fish_species_watershed %>% filter(watershed_group_code == 'BULK')%>% rename(Bulkley = watershed_group_code),
#                       fish_species_watershed %>% filter(watershed_group_code == 'KLUM')%>% rename(Kitsumkalum = watershed_group_code))

# grab watershed names from xref table - https://github.com/NewGraphEnvironment/rfp/issues/5
xref_wsg <- fpr::fpr_db_query(
  query= "SELECT watershed_group_code, watershed_group_name
  FROM whse_basemapping.fwa_watershed_groups_poly"
) |>
  dplyr::arrange(watershed_group_code)


wshd_names <- xref_wsg |>
  dplyr::filter(watershed_group_code %in% wshd_codes) |>
  dplyr::pull(watershed_group_name) |>
  # remove " River" from the names
  stringr::str_remove(" River")

names_tbl <- c('species_name', 'species_code', wshd_names)

#merge all data frames in list
fish_spp <- fish_spp_prep %>%
  purrr::reduce(full_join, by= c('species_code', 'species_name')) |>
  dplyr::relocate(c(species_name, species_code), .before = everything()) |>
  purrr::set_names(names_tbl)

fish_all <- fishbc::freshwaterfish
fish_cdc <- fishbc::cdc

fish_spp2 <- left_join(fish_spp,
                       fish_all,
                       by = c("species_code" = "Code")) %>%
  filter(!is.na(Class) & !species_code == 'TR') |> ##mottled sculpin has some sort of error going on
  # mutate(CDCode = case_when(species_code == 'BT' ~ 'F-SACO-11', ##pacific population yo
  #                           T ~ CDCode)) %>%
  select(species_code,
         species_name,
         all_of(wshd_names),
         CDCode)

fish_spp3 <- left_join(
  fish_spp2,
  fish_cdc,
  by = c('CDCode' = 'Species Code')
) %>%
  select(`Scientific Name`,
         'Species Name' = species_name,
         # 'Species Code' = species_code,
         `BC List`,
         # `Provincial FRPA`,
         COSEWIC,
         # SARA,
         all_of(wshd_names)
         ) %>%
  mutate(across(all_of(wshd_names), ~ifelse(!is.na(.), "Yes", .))) |>
  arrange(`Scientific Name`, `Species Name`)

##print your table to input_raw for use in the report
fish_spp3 %>% readr::write_csv(file = 'data/inputs_extracted/fiss_species_table.csv')

