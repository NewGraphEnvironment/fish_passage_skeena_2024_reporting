source('scripts/packages.R')


# connect to the database
conn <- fpr_db_conn()

# grab all the crossings from our study area
# I will use the same nameing convention as our repository
bcfishpass <- fpr_db_query(
  query = "SELECT * from bcfishpass.crossings_vw WHERE watershed_group_code IN ('LCHL', 'NECR', 'FRAN');"
)

# always disconnect from the database
DBI::dbDisconnect(conn = conn)
