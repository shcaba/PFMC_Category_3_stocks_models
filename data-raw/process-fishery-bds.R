drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

load(here::here(
  drive,
  "Stripetail Rockfish",
  "pacfin",
  "2026-09-10",
  "PacFIN.STRK.bds.10.Sep.2026.RData"
))
bds_raw_stripetail <- bds.pacfin

load(here::here(
  drive,
  "Pacific Sanddab",
  "pacfin",
  "2026-09-10",
  "PacFIN.PDAB.bds.10.Sep.2026.RData"
))
bds_raw_sanddab <- bds.pacfin

bds_clean_stripetail <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_stripetail,
  keep_gears = "TWL"
)
# 6845 records: CA 4705, OR 1978, WA 162
# TWL: 6073, MID: 678
# SEX F: 5906, M: 454, U: 485
# observed size of females > males

bds_clean_sanddab <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_sanddab,
  keep_gears = "TWL"
)
# 41776 records: CA 25050, OR 16606, WA 120
# HKL: 5883, TWL: 35147
# SEX F: 32605, M: 5527, U: 3644
# observed size of females > males

# Stripetail rockfish ==========================================================
formatted_catch_stripetail <- pacfintools::formatCatch(
  catch = pacfin_landings_by_state |>
    dplyr::filter(species == "stripetail rockfish"),
  strat = c("state"),
  valuename = "landings_mt"
) |>
  # there are no species-specific landings in CA-1999, these samples were primarily
  # from unspecified small rockfish and unspecified rockfish. Add a minimal amount
  # of landings to facilitate second-stage expansion
  dplyr::mutate(
    C = dplyr::if_else(year == 1999, 1, C)
  ) |>
  dplyr::arrange(year)

expanded_comps <- pacfintools::get_pacfin_expansions(
  Pdata = bds_clean_stripetail |>
    # temporarily removing records before 1981 (all in CA) because we don't have
    # CA historical catches yet
    dplyr::filter(year >= 1981) |>
    dplyr::mutate(stratification = AGENCY_CODE),
  Catch = formatted_catch_stripetail,
  weight_length_estimates = weight_length_estimates |>
    dplyr::filter(species == "stripetail rockfish"),
  Units = "MT",
  maxExp = 0.90,
  Exp_WA = TRUE,
  verbose = FALSE
)

length_comps_long <- pacfintools::getComps(
  Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm)),
  Comps = "LEN",
  weightid = "Final_Sample_Size_L",
  verbose = FALSE
)

stripetail_pacfin_length_composition_data <- pacfintools::writeComps(
  inComps = length_comps_long,
  column_with_input_n = "n_stewart",
  comp_bins = length_bins,
  verbose = FALSE
) |>
  dplyr::mutate(
    fleet = "retained-comps"
  )

# Pacific sanddab ==============================================================
length_bins <- seq(8, 32, 2)
formatted_catch_sanddab <- pacfintools::formatCatch(
  catch = pacfin_landings_by_state |>
    dplyr::filter(species == "pacific sanddab"),
  strat = c("state"),
  valuename = "landings_mt"
)

expanded_comps <- pacfintools::get_pacfin_expansions(
  Pdata = bds_clean_sanddab |>
    dplyr::filter(year > 1980) |>
    dplyr::mutate(stratification = AGENCY_CODE),
  Catch = formatted_catch_sanddab,
  weight_length_estimates = weight_length_estimates |>
    dplyr::filter(species == "pacific sanddab"),
  Units = "MT",
  maxExp = 0.90,
  Exp_WA = TRUE,
  verbose = TRUE
)

length_comps_long <- getComps(
  Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm)),
  Comps = "LEN",
  weightid = "Final_Sample_Size_L",
  verbose = TRUE
)

sanddab_pacfin_length_composition_data <- writeComps(
  inComps = length_comps_long,
  column_with_input_n = "n_stewart",
  comp_bins = length_bins,
  verbose = FALSE
) |>
  dplyr::mutate(
    fleet = "retained-comps"
  )

#===============================================================================
# Save composition data formatted for SS3
#===============================================================================
write_named_csvs(
  stripetail_pacfin_length_composition_data,
  sanddab_pacfin_length_composition_data,
  dir = here::here("data-tables")
)
