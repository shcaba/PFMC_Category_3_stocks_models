drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"


#===============================================================================
# Load discard data files
#===============================================================================
discard_comp_stripetail <-
  read.csv(here::here(
    drive,
    "Stripetail Rockfish",
    "wcgop",
    "recommended",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 0
  )
discard_comp_sanddab <-
  read.csv(here::here(
    drive,
    "Pacific Sanddab",
    "wcgop",
    "recommended",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 0
  )
discard_comp_bank <-
  read.csv(here::here(
    drive,
    "Bank Rockfish",
    "wcgop",
    "recommended",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 0
  )
discard_comp_rosethorn <-
  read.csv(here::here(
    drive,
    "Rosethorn Rockfish",
    "wcgop",
    "recommended",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 0
  )
discard_comp_flathead <-
  read.csv(here::here(
    drive,
    "Flathead Sole",
    "wcgop",
    "recommended",
    "wcgop_discard_lengths.csv"
  )) |>
  dplyr::mutate(
    fleet = "discard-comps",
    partition = 0
  )

#===============================================================================
# Load PacFIN data by species
#===============================================================================
load(here::here(
  drive,
  "Flathead Sole",
  "pacfin",
  "PacFIN.FSOL.bds.25.Sep.2026.RData"
))
bds_raw_flathead <- bds.pacfin

load(here::here(
  drive,
  "Silvergray Rockfish",
  "pacfin",
  "PacFIN.SLGR.bds.25.Sep.2026.RData"
))
bds_raw_silvergray <- bds.pacfin

load(here::here(
  drive,
  "Rosethorn Rockfish",
  "pacfin",
  "PacFIN.RSTN.bds.25.Sep.2026.RData"
))
bds_raw_rosethorn <- bds.pacfin

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
  "Yellowmouth Rockfish",
  "pacfin",
  "PacFIN.YMTH.bds.25.Sep.2026.RData"
))
bds_raw_yellowmouth <- bds.pacfin

load(here::here(
  drive,
  "Bank Rockfish",
  "pacfin",
  "PacFIN.BANK.bds.25.Sep.2026.RData"
))
bds_raw_bank <- bds.pacfin

load(here::here(
  drive,
  "Pacific Sanddab",
  "pacfin",
  "2026-09-10",
  "PacFIN.PDAB.bds.10.Sep.2026.RData"
))
bds_raw_sanddab <- bds.pacfin

#===============================================================================
# Load the PacFIN landings data
#===============================================================================

landings <- landings_by_state_for_expansion |>
  dplyr::mutate(
    state = dplyr::if_else(
      state == "california",
      "C",
      dplyr::if_else(state == "oregon", "O", "W")
    )
  )


#===============================================================================
# Bank rockfish
#===============================================================================
species_name <- "bank rockfish"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)

bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_bank
) |>
  dplyr::mutate(
    fleet = dplyr::if_else(
      geargroup %in% c("TWL", "TWS"),
      "TWL",
      "FG"
    )
  )
# 50448 records: CA 48212, OR 1988, WA 248
# SEX F: 19512, M: 17205, U: 13731
# data start in 1977 in CA (1977: n = 64, 1978: n= 185, 1979: n= 26, 1980: n=28)

length_bins <- seq(bins$low, bins$high, bins$bin)
formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
  strat = c("state", "gear"),
  valuename = "landings_mt"
) |>
  # there are no species-specific landings in 2004-W, 2015-W, 2017-W, 2024-W
  dplyr::mutate(
    W.FG = dplyr::if_else(
      year %in% c(2004, 2014, 2015, 2017, 2020, 2024, 2025),
      0.01,
      W.FG
    ),
    W.TWL = dplyr::if_else(year %in% c(2015, 2021), 0.01, W.TWL),
    O.FG = dplyr::if_else(year %in% c(2018, 2025), 0.01, O.FG)
  ) |>
  dplyr::arrange(year)

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = paste0(AGENCY_CODE, ".", fleet)),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean |>
    dplyr::mutate(stratification = paste0(AGENCY_CODE, ".", fleet)),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)

discard_lengths <- format_two_sex_comps(
  data = discard_comp_bank
)

length_trips_samples_bank <- comps$length_trips_samples

length_composition_data_bank <- combine_comps(
  comps_1 = retained_lengths,
  comps_2 = discard_lengths
)
length_composition_data_bank_split_by_sex <- combine_comps(
  comps_1 = retained_lengths_by_sex_wide,
  comps_2 = discard_lengths
)

write_named_csvs(
  length_composition_data_bank,
  length_composition_data_bank_split_by_sex,
  length_trips_samples_bank,
  dir = here::here("data-tables")
)

#===============================================================================
# Stripetail rockfish
#===============================================================================
species_name <- "stripetail rockfish"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)

bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_stripetail,
  keep_gears = "TWL"
)
# 6845 records: CA 4705, OR 1978, WA 162
# TWL: 6073, MID: 678
# SEX F: 5906, M: 454, U: 485
# observed size of females > males

length_bins <- seq(bins$low, bins$high, bins$bin)
formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
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

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )
length_trips_samples_stripetail <- comps$length_trips_samples |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE, SEX_CODE = "U"),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths_unsexed <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
) |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

discard_lengths <- format_two_sex_comps(
  data = discard_comp_stripetail
)

length_composition_data_stripetail <- combine_comps(
  comps_1 = retained_lengths,
  comps_2 = discard_lengths
)
length_composition_data_stripetail_split_by_sex <- combine_comps(
  comps_1 = retained_lengths_by_sex_wide,
  comps_2 = discard_lengths
)
length_composition_data_unsexed_stripetail <- combine_comps(
  comps_1 = retained_lengths_unsexed,
  comps_2 = discard_comp_stripetail
)

write_named_csvs(
  length_composition_data_stripetail,
  length_composition_data_unsexed_stripetail,
  length_composition_data_stripetail_split_by_sex,
  length_trips_samples_stripetail,
  dir = here::here("data-tables")
)

#===============================================================================
# Pacific sanddab
#===============================================================================
species_name <- "pacific sanddab"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)

bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_sanddab,
  keep_gears = "TWL"
)
# 41776 records: CA 25050, OR 16606, WA 120
# HKL: 5883, TWL: 35147
# SEX F: 32605, M: 5527, U: 3644
# observed size of females > males

length_bins <- seq(bins$low, bins$high, bins$bin)
formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
  strat = c("state"),
  valuename = "landings_mt"
)

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::filter(year > 1980) |>
    dplyr::mutate(stratification = AGENCY_CODE),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == "Pacific sanddab"),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )
length_trips_samples_sanddab <- comps$length_trips_samples |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean,
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == "Pacific sanddab"),
  length_bins = length_bins
) |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

discard_lengths <- format_two_sex_comps(
  data = discard_comp_sanddab
)
length_composition_data_sanndab <- combine_comps(
  comps_1 = retained_lengths,
  comps_2 = discard_lengths
)
length_composition_data_sanndab_split_by_sex <- combine_comps(
  comps_1 = retained_lengths_by_sex_wide,
  comps_2 = discard_lengths
)

write_named_csvs(
  length_composition_data_sanndab,
  length_composition_data_sanndab_split_by_sex,
  length_trips_samples_sanddab,
  dir = here::here("data-tables")
)

#===============================================================================
# Yellowmouth rockfish
#===============================================================================
bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_yellowmouth,
  keep_gears = "TWL"
)
# 4615 records: CA 112, OR 3374, WA 1129
# SEX F: 2093, M: 1867, U: 655
# data start in 1973 in WA
species_name <- "yellowmouth rockfish"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)

bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_yellowmouth,
  keep_gears = "TWL"
)
# 4615 records: CA 112, OR 3374, WA 1129
# SEX F: 2093, M: 1867, U: 655
# data start in 1973 in WA

length_bins <- seq(24, 54, 2)
formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
  strat = c("state"),
  valuename = "landings_mt"
)

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE),
  catch = formatted_catch,
  weight_lengt = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )
length_trips_samples_yellowmouth <- comps$length_trips_samples |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE, SEX_CODE = "U"),
  catch = formatted_catch,
  weight_lengt = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths_unsexed <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
) |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

length_composition_data_yellowmouth <- retained_lengths
length_composition_data_unsexed_yellowmouth <- retained_lengths_unsexed
length_composition_data_yellowmouth_split_by_sex <- retained_lengths_by_sex_wide

write_named_csvs(
  length_composition_data_yellowmouth,
  length_composition_data_unsexed_yellowmouth,
  length_composition_data_yellowmouth_split_by_sex,
  length_trips_samples_yellowmouth,
  dir = here::here("data-tables")
)

#===============================================================================
# Flathead sole
#===============================================================================
species_name <- "flathead sole"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)

bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_flathead
) |>
  # remove the single unsexed length
  dplyr::filter(SEX_CODE != "U", state != "WA") |>
  dplyr::mutate(stratification = AGENCY_CODE)
# 2137 records: CA 1, OR 2030, WA 106
# there are no WA landings data: these samples come from 2021 and 2024 only
# these WA records come from one ftid in 2021 and 3 ftid in 2024
# MID: 105, TWL: 2032
# SEX F: 1652, M: 484, U: 1
# females are larger than males

length_bins <- seq(bins$low, bins$high, bins$bin)
formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
  strat = c("state"),
  valuename = "landings_mt"
)

comps <- get_expanded_length_comps_standard(
  data = bds_clean,
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )
length_trips_samples_flathead <- comps$length_trips_samples |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean,
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
) |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

discard_lengths <- format_two_sex_comps(
  data = discard_comp_flathead
)
length_composition_data_flathead <- combine_comps(
  comps_1 = retained_lengths,
  comps_2 = discard_lengths
)
length_composition_data_flathead_split_by_sex <- combine_comps(
  comps_1 = retained_lengths_by_sex_wide,
  comps_2 = discard_lengths
)

write_named_csvs(
  length_composition_data_flathead,
  length_composition_data_flathead_split_by_sex,
  length_trips_samples_flathead,
  dir = here::here("data-tables")
)

#===============================================================================
# Rosethorn rockfish
#===============================================================================
species_name <- "rosethorn rockfish"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)

bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_rosethorn
) |>
  # very few samples by year before this
  dplyr::filter(year >= 1983) |>
  dplyr::mutate(
    #gear = dplyr::if_else(
    #  geargroup %in% c("TWL", "TWS", "MID"),
    #  "TWL",
    #  "FG"
    #),
    fleet = "all",
    stratification = AGENCY_CODE
  )
# 5515 records: CA 1220, OR 2382, WA 1913
# TWL: 3412, FG 2103
# SEX F: 1919, M: 2761, U: 835
# all sexes are similar and similar across gears

ggplot2::ggplot(bds_clean, ggplot2::aes(x = lengthcm, fill = SEX_CODE)) +
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_grid("gear")

length_bins <- seq(bins$low, bins$high, bins$bin)
formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
  strat = c("state"),
  valuename = "landings_mt"
)

comps <- get_expanded_length_comps_standard(
  data = bds_clean,
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )
length_trips_samples_rosethorn <- comps$length_trips_samples |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE, SEX_CODE = "U"),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths_unsexed <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean,
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
) |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

discard_lengths <- format_two_sex_comps(
  data = discard_comp_rosethorn
)
length_composition_data_rosethorn <- combine_comps(
  comps_1 = retained_lengths,
  comps_2 = discard_lengths
)
length_composition_data_rosethorn_split_by_sex <- combine_comps(
  comps_1 = retained_lengths_by_sex_wide,
  comps_2 = discard_lengths
)
length_composition_data_rosethorn_unsexed <- combine_comps(
  comps_1 = retained_lengths_unsexed,
  comps_2 = discard_comp_rosethorn
)

write_named_csvs(
  length_composition_data_rosethorn,
  length_composition_data_rosethorn_split_by_sex,
  length_composition_data_rosethorn_unsexed,
  length_trips_samples_rosethorn,
  dir = here::here("data-tables")
)

#===============================================================================
# Silvergray rockfish
#===============================================================================
bds_clean <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_silvergray
) |>
  dplyr::mutate(
    gear = dplyr::if_else(
      geargroup %in% c("TWL", "TWS", "MID"),
      "TWL",
      "FG"
    ),
    fleet = "all",
    stratification = AGENCY_CODE
  )
# 9034 records: CA 98, OR 3332, WA 5604
# SEX F: 3635, M: 4067, U: 1332

ggplot2::ggplot(bds_clean, ggplot2::aes(x = lengthcm, fill = SEX_CODE)) +
  ggplot2::geom_density(alpha = 0.25) +
  ggplot2::facet_grid("gear")

species_name <- "silvergray rockfish"
bins <- length_bins_all_species |> dplyr::filter(species == species_name)
length_bins <- seq(bins$low, bins$high, bins$bin)

formatted_catch <- pacfintools::formatCatch(
  catch = landings |>
    dplyr::filter(species == species_name),
  strat = c("state"),
  valuename = "landings_mt"
)

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE) |>
    dplyr::filter(!(state == "CA" & year == 1999)),
  catch = formatted_catch,
  weight_lengt = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )
length_trips_samples_yellowmouth <- comps$length_trips_samples |>
  dplyr::mutate(
    fleet = "landed-comps"
  )

comps <- get_expanded_length_comps_standard(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE, SEX_CODE = "U") |>
    dplyr::filter(!(state == "CA" & year == 1999)),
  catch = formatted_catch,
  weight_lengt = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
)
retained_lengths_unsexed <- comps$formatted_length_comps |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

retained_lengths_by_sex_wide <- get_expanded_length_comps_by_sex(
  data = bds_clean |>
    dplyr::mutate(stratification = AGENCY_CODE) |>
    dplyr::filter(!(state == "CA" & year == 1999)),
  catch = formatted_catch,
  weight_length = weight_length_estimates |>
    dplyr::filter(species == species_name),
  length_bins = length_bins
) |>
  dplyr::mutate(
    month = 7,
    fleet = "landed-comps",
    partition = 0
  )

length_composition_data_yellowmouth <- retained_lengths
length_composition_data_unsexed_yellowmouth <- retained_lengths_unsexed
length_composition_data_yellowmouth_split_by_sex <- retained_lengths_by_sex_wide

write_named_csvs(
  length_composition_data_yellowmouth,
  length_composition_data_unsexed_yellowmouth,
  length_composition_data_yellowmouth_split_by_sex,
  length_trips_samples_yellowmouth,
  dir = here::here("data-tables")
)
