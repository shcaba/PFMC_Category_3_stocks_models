species_list <- c(
  "bank rockfish",
  "pacific sanddab",
  "stripetail rockfish",
  "yellowmouth rockfish",
  "pacific cod",
  "starry rockfish",
  "shortraker rockfish",
  "redstripe rockfish",
  "silvergray rockfish",
  "rosethorn rockfish",
  "flathead sole"
)

data <- nwfscSurvey::pull_gemm(
  common_name = species_list,
  verbose = FALSE
) |>
  dplyr::filter(
    !sector %in%
      c(
        "Research",
        "Washington Recreational",
        "Oregon Recreational",
        "California Recreational"
      )
  )

commercial_discard_rates <- data |>
  dplyr::mutate(
    species = tolower(species)
  ) |>
  dplyr::summarise(
    .by = c("species", "year"),
    discard = sum(discard_mortality),
    landings = sum(landings),
    discard_rate = round(discard / (discard + landings), 4),
    source = "GEMM"
  ) |>
  dplyr::mutate(
    discard = round(discard, 4),
    landings = round(landings, 4)
  ) |>
  dplyr::arrange(species, year)


usethis::use_data(
  commercial_discard_rates,
  overwrite = TRUE
)

write_named_csvs(
  commercial_discard_rates,
  dir = here::here("data-tables")
)
