# Pull survey data to inform biological parameters
bio_data <- nwfscSurvey::pull_bio(
  common_name = c("Pacific sanddab", "stripetail rockfish")
)
maturity_data <- nwfscSurvey::pull_biological_samples(
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
