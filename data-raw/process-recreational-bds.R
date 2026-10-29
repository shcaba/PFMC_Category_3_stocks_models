drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

rec_bds_raw <- read.csv(
  file.path(
    drive,
    "Bank Rockfish",
    "recfin",
    "SD501-WASHINGTON-OREGON-CALIFORNIA-1979---2025.csv"
  )
)

mrfss_bds_raw <- read.csv(
  file.path(
    drive,
    "Bank Rockfish",
    "recfin",
    "SD517-1980---2003.csv"
  )
)

recreational_composition <- rec_bds_raw |>
  dplyr::filter(IS_RETAINED == "RETAINED") |>
  dplyr::mutate(
    year = as.numeric(RECFIN_YEAR),
    source = "recfin",
    state = tolower(STATE_NAME),
    length_cm = RECFIN_LENGTH_MM / 10,
    sex = "U"
  ) |>
  as.data.frame()

mrfss_composition <- mrfss_bds_raw |>
  dplyr::filter(AREA_NAME == "OCEAN", !is.na(LNGTH)) |>
  dplyr::mutate(
    year = as.numeric(YEAR),
    source = "mrfss",
    state = tolower(ST_NAME),
    length_cm = LNGTH / 10,
    sex = "U"
  ) |>
  as.data.frame()

col_names <- c("year", "source", "state", "length_cm", "sex")
combined_comps <- dplyr::bind_rows(
  recreational_composition[, col_names],
  mrfss_composition[, col_names]
) |>
  dplyr::mutate(
    trawl_id = dplyr::row_number()
  )


length_bins <- seq(12, 50, 2)
comps <- nwfscSurvey::get_raw_comps(
  data = combined_comps,
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  input_n_method = "total_samples",
  two_sex_comps = TRUE,
  month = 7,
  fleet = "rec"
)
length_composition_data_recreational_bank <- comps$unsexed
nwfscSurvey::plot_comps(data = length_composition_data_recreational_bank)
write_named_csvs(
  length_composition_data_recreational_bank,
  dir = here::here("data-tables")
)
