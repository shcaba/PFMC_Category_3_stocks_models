# Pull survey data to inform biological parameters
bio_data <- nwfscSurvey::pull_bio(
  common_name = c("Pacific sanddab", "stripetail rockfish")
)
maturity_data_raw <- nwfscSurvey::pull_biological_samples(
  common_name = c("Pacific sanddab", "stripetail rockfish"),
  standard_filtering = FALSE
)

bio_data <- bio_data |>
  dplyr::mutate(
    species = Common_name
  )

weight_length_estimates <- bio_data |>
  dplyr::filter(!is.na(Weight_kg), !is.na(Length_cm)) |>
  dplyr::rename(sex = Sex) |>
  dplyr::group_by(species) |>
  dplyr::group_modify(
    ~ nwfscSurvey::estimate_weight_length(
      data = .x,
      col_length = "Length_cm",
      col_weight = "Weight_kg"
    )
  ) |>
  dplyr::ungroup() |>
  as.data.frame()


growth <- nwfscSurvey::est_growth(
  dat = bio_data |> dplyr::filter(!is.na(Age_years), Sex != "U"),
  return_df = FALSE
)

growth_estimates <- rbind(
  growth$female_growth,
  growth$male_growth
) |>
  as.data.frame() |>
  dplyr::mutate(
    species = "pacific sanddab",
    sex = c("female", "male")
  ) |>
  dplyr::relocate(
    c(species, sex),
    .before = K
  )

usethis::use_data(
  weight_length_estimates,
  overwrite = TRUE
)

usethis::use_data(
  growth_estimates,
  overwrite = TRUE
)

maturity_data_table <- maturity_data_raw |>
  dplyr::mutate(
    species = tolower(common_name)
  ) |>
  dplyr::filter(!is.na(biologically_mature_indicator)) |>
  dplyr::select(
    species,
    year,
    length_cm,
    age_years,
    ovary_id,
    ovary_proportion_atresia,
    biologically_mature_certain_indicator,
    biologically_mature_indicator
  )

write_named_csvs(
  maturity_data_table,
  dir = here::here("data-tables")
)


data_survey_specimens <- bio_data |>
  dplyr::select(
    Year,
    Project,
    Common_name,
    Sex,
    Length_cm,
    Age,
    Weight_kg,
    Depth_m,
    Latitude_dd,
    Longitude_dd
  ) |>
  dplyr::rename_all(tolower) |>
  dplyr::rename(
    species = common_name,
    source = project
  )

usethis::use_data(
  data_survey_specimens,
  overwrite = TRUE
)
