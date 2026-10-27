drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

rec_bds_raw <- read.csv(
  file.path(
    drive, 
    "Bank Rockfish",
    "recfin",
    "SD501-WASHINGTON-OREGON-CALIFORNIA-1979---2025.csv"
  )
)

recreational_composition <- rec_bds_raw |>
  dplyr::filter(IS_RETAINED == "RETAINED") |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    source = "recfin",
    state = tolower(STATE_NAME),
    length_cm = RECFIN_LENGTH_MM / 10,
    sex = nwfscSurvey::codify_sex(RECFIN_SEX_CODE),
    trawl_id = dplyr::row_number()
  )

length_bins <- seq(12, 50, 2)

comps <- nwfscSurvey::get_raw_comps(
  data = recreational_composition,
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  input_n_method = "total_samples",
  two_sex_comps = TRUE,
  month = 7, 
  fleet = "rec-retained"
)
length_composition_data_recreational_bank <- comps$unsexed
nwfscSurvey::plot_comps(data = length_composition_data_recreational_bank)
write_named_csvs(
  length_composition_data_recreational_bank,
  dir = here::here("data-tables")
)
