library(DBI)
library(odbc)
#Must be on the network/VPN

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 18 for SQL Server",
  Server = "swc-estrella-g",
  Database = "California_Groundfish_Catch_Reconstructions",
  Trusted_Connection = "Yes",
  TrustServerCertificate = "Yes",
  Encrypt = "Optional"
)

CA_Recon_comm_data <- DBI::dbGetQuery(
  con,
  "SELECT * FROM RECON_COMM_ALL_SPECIES_1916_1968"
)
CA_Recon_rockfish_rec_north_data <- DBI::dbGetQuery(
  con,
  "SELECT * FROM RECON_REC_ROCKFISH_1928_1980_NORTH_BY_MODE"
)
CA_Recon_rockfish_rec_south_data <- DBI::dbGetQuery(
  con,
  "SELECT * FROM RECON_REC_ROCKFISH_1928_1980_SOUTH_BY_MODE"
)

DBI::dbDisconnect(con)

# Do some data clean up before saving files
ca_comm_recon_data <- CA_Recon_comm_data |>
  dplyr::mutate(
    species = gsub("\\s+", "", species),
    catch_mt = 0.000453592 * pounds
  ) |>
  dplyr::rename_with(tolower)

ca_rec_recon_rockfish_north <- CA_Recon_rockfish_rec_north_data |>
  tidyr::pivot_longer(
    cols = 5:ncol(CA_Recon_rockfish_rec_north_data),
    names_to = "species",
    values_to = "catch_lbs"
  ) |>
  dplyr::filter(Units == "pounds") |>
  dplyr::mutate(
    catch_mt = 0.000453592 * catch_lbs
  ) |>
  dplyr::rename_with(tolower)

ca_rec_recon_rockfish_south <- CA_Recon_rockfish_rec_south_data |>
  tidyr::pivot_longer(
    cols = 5:ncol(CA_Recon_rockfish_rec_south_data),
    names_to = "species",
    values_to = "catch_lbs"
  ) |>
  dplyr::filter(Units == "pounds") |>
  dplyr::mutate(
    catch_mt = 0.000453592 * catch_lbs
  ) |>
  dplyr::rename_with(tolower)

ca_rec_recon_rockfish <- dplyr::bind_rows(
  ca_rec_recon_rockfish_north,
  ca_rec_recon_rockfish_south
)

# Save the data to the network
drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

readr::write_rds(
  x = ca_comm_recon_data,
  file = file.path(
    drive,
    "Catch reconstructions",
    "CA_commercial",
    "ca_comm_recon_data.rds"
  )
)
readr::write_rds(
  x = ca_rec_recon_rockfish,
  file = file.path(
    drive,
    "Catch reconstructions",
    "CA recreational",
    "ca_rec_recon_rockfish.rds"
  )
)
