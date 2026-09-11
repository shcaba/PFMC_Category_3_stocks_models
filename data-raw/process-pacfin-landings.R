drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

load(here::here(
  drive,
  "Stripetail Rockfish",
  "pacfin",
  "2026-09-10",
  "PacFIN.STRK.CompFT.10.Sep.2026.RData"
))
landings_stripetail <- catch.pacfin

load(here::here(
  drive,
  "Pacific Sanddab",
  "pacfin",
  "2026-09-10",
  "PacFIN.PDAB.CompFT.10.Sep.2026.RData"
))
landings_sanddab <- catch.pacfin

landings_by_state_gear <- dplyr::bind_rows(
  landings_stripetail,
  landings_sanddab
) |>
  dplyr::rename_with(tolower) |>
  dplyr::filter(
    !(landing_year < 1987 & agency_code == "O")
  ) |>
  dplyr::mutate(
    species = tolower(pacfin_species_common_name),
    species = dplyr::replace_values(
      species,
      "nom. pacific sanddab" ~ "pacific sanddab",
      "nom. stripetail rockfish" ~ "stripetail rockfish"
    ),
    landings_mt = round_weight_mtons,
    year = landing_year,
    gear = pacfin_group_gear_code,
    state = agency_code,
    gear_group = dplyr::case_when(
      gear %in% c("TLS", "HKL", "POT") ~ "fixed-gear",
      TRUE ~ "trawl"
    )
  ) |>
  dplyr::summarise(
    .by = c(species, year, state, gear_group),
    landings_mt = round(sum(landings_mt), 4)
  )

write_named_csvs(
  landings_by_state_gear,
  dir = "data-tables"
)

landings <- landings_by_state_gear |>
  dplyr::summarise(
    .by = c(species, year),
    landings_mt = round(sum(landings_mt), 4)
  )

usethis::use_data(
  landings,
  overwrite = TRUE
)
