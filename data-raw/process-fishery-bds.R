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

landings <- landings_by_state_for_expansion |>
  dplyr::mutate(
    state = dplyr::if_else(
      state == "california",
      "C",
      dplyr::if_else(state == "oregon", "O", "W")
    )
  )

# Stripetail rockfish ==========================================================
length_bins <- seq(6, 28, 2)
formatted_catch_stripetail <- pacfintools::formatCatch(
  catch = landings |>
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

retained_lengths_stripetail <- pacfintools::writeComps(
  inComps = length_comps_long,
  column_with_input_n = "n_stewart",
  comp_bins = length_bins,
  verbose = FALSE
) |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps"
  )

# Pacific sanddab ==============================================================
length_bins <- seq(8, 32, 2)
formatted_catch_sanddab <- pacfintools::formatCatch(
  catch = landings |>
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

retained_lengths_sanddab <- writeComps(
  inComps = length_comps_long,
  column_with_input_n = "n_stewart",
  comp_bins = length_bins,
  verbose = FALSE
) |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps"
  )

#===============================================================================
# Grab the WCGOP comp files
#===============================================================================
discard_comp_stripetail <-
  read.csv(here::here(
    drive,
    "Stripetail Rockfish",
    "wcgop",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 2
  )
discard_comp_sanddab <-
  read.csv(here::here(
    drive,
    "Pacific Sanddab",
    "wcgop",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 2
  )

discard_lengths_sanddab <- cbind(
  discard_comp_sanddab,
  discard_comp_sanddab[, 7:ncol(discard_comp_sanddab)]
)
discard_lengths_stripetail <- cbind(
  discard_comp_stripetail,
  discard_comp_stripetail[, 7:ncol(discard_comp_stripetail)]
)


#===============================================================================
# Save composition data formatted for SS3
#===============================================================================
colnames(discard_lengths_stripetail) <- colnames(retained_lengths_stripetail)
colnames(discard_lengths_sanddab) <- colnames(retained_lengths_sanddab)

length_composition_data_sanndab <- dplyr::bind_rows(
  as.data.frame(retained_lengths_sanddab) |>
    dplyr::mutate(month = as.numeric(month)),
  discard_lengths_sanddab
)
length_composition_data_stripetail <- dplyr::bind_rows(
  as.data.frame(retained_lengths_stripetail) |>
    dplyr::mutate(month = as.numeric(month)),
  as.data.frame(discard_lengths_stripetail)
)
write_named_csvs(
  length_composition_data_sanndab,
  length_composition_data_stripetail,
  dir = here::here("data-tables")
)
