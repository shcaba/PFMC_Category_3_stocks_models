# This script does the following:
# 1. summarized landings from PacFIN
# 2. pulls in historical reconstructions (or catch from the 2013 assessments)
# 3. brings in the GEMM discard rates to calculated catch for pacfin and historical
# years.

drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

# Historical catch reconstructions
WA_rf_ct_recon <- read.csv(here::here(
  drive,
  "Catch reconstructions",
  "WA rockfish catch reconstruction",
  "SpeciesSumOutput2_2017.csv"
))

OR_recon <- readxl::read_excel(
  path = here::here(
    drive,
    "Catch reconstructions",
    "ODFW catch reconstruction",
    "1889-1986 OR Commercial Landings_v1.0.xls"
  ),
  sheet = "final_odfw_landings"
) |>
  dplyr::filter(SPECIES_NAME %in% c("Stripetail Rockfish", "Pacific Sanddab"))

OR_urock <- read.csv(
  here::here(
    drive,
    "Catch reconstructions",
    "ODFW catch reconstruction",
    "Speciated_URCK_POP1_Tickets.csv"
  )
) |>
  dplyr::filter(COMMON_NAM %in% c("STRIPETAIL ROCKFISH"))

# 2013 Assessment historical catch
historical_catch_sanddab_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "historical_data",
  "pacific_sanddab_historical_catches_2013_assessment.csv"
))
historical_catch_stripetail_raw <- read.csv(here::here(
  drive,
  "Stripetail Rockfish",
  "historical_data",
  "stripetail_rockfish_historical_catches_2013_assessment.csv"
))

# PacFIN data
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

load(here::here(
  drive,
  "Pacific Sanddab",
  "pacfin",
  "2026-09-10",
  "PacFIN.UDAB.CompFT.16.Sep.2026.RData"
))
landings_sanddab_general <- catch.pacfin
# all of the unspecified sanddab landings are from CA and WA, no OR
# the are records from 1981-2026, species-specific landings are similar to
# the GEMM starting in 2002. The previous assessment notes that 98% of the
# landed sanddabs were Pacific sanddabs between 2003-2011. The amount of
# unspecified sanddabs in CA declines to single digits starting in 2003,
# this implies that 2002 needs to be allocated and that pacfin landings
# will exceed those in the GEMM for that year
# It appears that WA does not sort out sanddabs (even post-2003). General sanddab
# landings is Washington is low and there are 0 specified landings in Washington.
# There are bds samples in 2021 and 2023 in WA where the FTID matches landings
# in the unspecified sanddab landings.

#===============================================================================
# State provided historical reconstructions
#===============================================================================
commercial_reconstruction_wa <- WA_rf_ct_recon |>
  dplyr::filter(
    SPID %in% c("BANK", "RSTN", "SRKR", "STAR", "STRK", "SLGR", "YMTH"),
    CompositionType != "Existing PacFIN"
  ) |>
  dplyr::rename_all(tolower) |>
  dplyr::mutate(
    species = dplyr::recode_values(
      spid,
      "BANK" ~ "bank rockfish",
      "RSTN" ~ "rosethorn rockfish",
      "SRKR" ~ "shortraker rockfish",
      "STAR" ~ "starry rockfish",
      "STRK" ~ "stripetail rockfish",
      "SLGR" ~ "silvergrey rockfish",
      "YMTH" ~ "yellowmouth rockfish"
    ),
    #gear_group = dplyr::recode_values(
    #  geargroup,
    #  "JIG" ~ "HKL",
    #  "HKL" ~ "HKL",
    #  "TWL" ~ "TWL"
    #),
    species_metric_tons = 0.000453592 * speciespounds,
    source = "wa_rockfish_reconstruction",
    fleet = "commercial",
    state = "washington"
  ) |>
  dplyr::summarise(
    .by = c(species, source, fleet, year),
    state = "washington",
    landings_mt = round(sum(species_metric_tons, na.rm = TRUE), 4),
    discard_mt = 0
  ) |>
  dplyr::filter(
    species == "stripetail rockfish",
    year < 1981
  )

# cols needed: species, source, fleet, year, gear_group, landings_mt, discard_mt
commercial_reconstruction_rockfish_or <- OR_urock |>
  dplyr::mutate(
    species = tolower(COMMON_NAM),
    year = LANDING_YEAR
  ) |>
  dplyr::summarise(
    .by = c(species, year),
    source = "or_urock_reconstruction",
    fleet = "commercial",
    state = "oregon",
    landings_mt = 0.000453592 * round(sum(Species.lbs), 4),
    discard_mt = 0
  )

commercial_reconstruction_species_specific_or <- OR_recon |>
  dplyr::mutate(
    species = tolower(SPECIES_NAME),
    year = as.numeric(YEAR),
    fleet = dplyr::if_else(
      MKT_CAT_NAME == "Mink Food" & species == "pacific sanddab",
      "mink fishery",
      "commercial"
    )
  ) |>
  dplyr::summarise(
    .by = c(species, year, fleet),
    source = "or_reconstruction",
    state = "oregon",
    landings_mt = round(sum(ROUND_MTONS), 4),
    discard_mt = 0
  ) |>
  as.data.frame()

commercial_reconstructions <- dplyr::bind_rows(
  commercial_reconstruction_wa,
  commercial_reconstruction_rockfish_or,
  commercial_reconstruction_species_specific_or
)

#ggplot2::ggplot(commercial_reconstructions, ggplot2::aes(x = year, y = landings_mt, fill = state)) +
#  ggplot2::geom_bar(stat = "identity") +
#  ggplot2::facet_grid("species", scales = "free_y")

#===============================================================================
# PacFIN landings
#===============================================================================
cols_to_keep <- c(
  "LANDING_YEAR",
  "AGENCY_CODE",
  "PACFIN_SPECIES_COMMON_NAME",
  "ROUND_WEIGHT_MTONS",
  "PACFIN_GROUP_GEAR_CODE"
)
pacfin_landings_by_state <- dplyr::bind_rows(
  landings_stripetail[, cols_to_keep],
  landings_sanddab[, cols_to_keep],
  landings_sanddab_general[, cols_to_keep] |>
    dplyr::filter(!(AGENCY_CODE == "C" & LANDING_YEAR >= 2003)) |>
    dplyr::mutate(
      ROUND_WEIGHT_MTONS = ROUND_WEIGHT_MTONS * 0.98,
      PACFIN_SPECIES_COMMON_NAME = "pacific sanddab"
    )
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
    state = dplyr::if_else(
      agency_code == "C",
      "california",
      dplyr::if_else(agency_code == "O", "oregon", "washington")
    ),
    fleet = "commercial",
    source = "pacfin"
  ) |>
  dplyr::summarise(
    .by = c(species, source, fleet, state, year),
    landings_mt = round(sum(landings_mt), 4),
    discard_mt = 0
  )

#===============================================================================
# Catches from the 2013 assessment
#===============================================================================
historical_catch_sanddab <- historical_catch_sanddab_raw |>
  dplyr::filter(
    # historical recreational catch prior to 1980 are brought in in the
    # process-recreational-landings script based on the 2013 assessment.
    # It assumes that all of the historical catch in the 2013 model are
    # from california.
    year != 1888,
    fleet != "recreational",
    # Working from the assumption that "mink" catches from the 2013
    # assessment are captured in PacFIN. Keep only the historical values.
    !(fleet == "mink" & year >= 1981),
    # PacFIN appears to be missing catches prior to 1994. The values from
    # the 2013 assessment were significantly higher.
    # Issue: do not know what the discard assumptions were applied to the
    # the PacFIN years 1981-1993 and historically.
    !(fleet == "commercial" & year >= 1981)
  ) |>
  dplyr::summarise(
    .by = c(species, source, state, fleet, year),
    landings_mt = 0,
    catch_mt = sum(catch_mt)
  )

historical_catch_stripetail <- historical_catch_stripetail_raw |>
  dplyr::filter(
    year < 1981
  ) |>
  dplyr::summarise(
    .by = c(species, source, state, fleet, year),
    landings_mt = 0,
    catch_mt = sum(catch_mt)
  )

historical_assessment_catch <- dplyr::bind_rows(
  historical_catch_stripetail,
  historical_catch_sanddab
)

#===============================================================================
# Bring all sources together
#===============================================================================
commercial_mortality_by_source <- dplyr::bind_rows(
  pacfin_landings_by_state,
  commercial_reconstructions
) |>
  dplyr::summarise(
    .by = c(species, source, state, fleet, year),
    landings_mt = sum(landings_mt),
    discard_mt = sum(discard_mt)
  )

ggplot2::ggplot(
  commercial_mortality_by_source,
  ggplot2::aes(x = year, y = landings_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")

ggplot2::ggplot(
  commercial_mortality_by_source,
  ggplot2::aes(x = year, y = landings_mt, fill = state)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")


#===============================================================================
# Format removals for used to composition expansions
#===============================================================================
landings_by_state_for_expansion <- commercial_mortality_by_source |>
  dplyr::summarise(
    .by = c(species, state, year),
    landings_mt = round(sum(landings_mt), 4)
  ) |>
  dplyr::arrange(species, state, year)

usethis::use_data(
  landings_by_state_for_expansion,
  overwrite = TRUE
)

#===============================================================================
# Add discard mortality to get to catch
#===============================================================================
commercial_catch <- dplyr::left_join(
  x = commercial_mortality_by_source,
  y = commercial_discard_rates |>
    dplyr::group_by(species, year) |>
    dplyr::mutate(
      gemm_mortality = sum(discard + landings)
    ) |>
    dplyr::select(species, year, discard_rate, gemm_mortality),
  by = c("species", "year")
) |>
  dplyr::mutate(
    # 2002 in CA includes both sanddab and urock-apply the GEMM discard rate
    # ends up with > 2200 mt in discards for this year which seem unrealistic
    # apply lower rate to all states
    discard_rate = dplyr::if_else(
      year == 2002 & species == "pacific sanddab",
      0.22,
      discard_rate
    ),
    # landings for stripetail are unusually high in 1994, assume that discard
    # was different for this reason for unknown reasons
    discard_rate = dplyr::if_else(
      year == 1994 & species == "stripetail rockfish",
      0.10,
      discard_rate
    )
  ) |>
  dplyr::filter(year < 2026) |>
  dplyr::group_by(species) |>
  dplyr::mutate(
    average_discard_rate = mean(discard_rate, na.rm = TRUE),
    rate_applied = dplyr::if_else(
      is.na(discard_rate),
      dplyr::if_else(species == "stripetail rockfish", 0.45, 0.22),
      discard_rate
    ),
    discard_mt = round(
      (rate_applied * landings_mt) / (1 - rate_applied),
      4
    ),
    catch_mt = round(discard_mt + landings_mt, 4)
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    .by = c(species, year),
    landings_mt = sum(landings_mt),
    discard_mt = sum(discard_mt),
    catch_mt = sum(catch_mt)
  ) |>
  dplyr::arrange(species, year)


plot_1 <- ggplot2::ggplot(
  commercial_catch,
  ggplot2::aes(x = year, y = catch_mt)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("species", scales = "free_y")

plot_2 <- ggplot2::ggplot(
  commercial_catch,
  ggplot2::aes(x = year, y = discard_mt)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("species", scales = "free_y")

plot_3 <- ggplot2::ggplot(
  commercial_catch,
  ggplot2::aes(x = year, y = landings_mt)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("species", scales = "free_y")

cowplot::plot_grid(
  plot_1,
  plot_2,
  plot_3,
  nrow = 3
)
#===============================================================================
# Save output
#===============================================================================
write_named_csvs(
  commercial_catch,
  dir = "data-tables"
)

usethis::use_data(
  commercial_catch,
  overwrite = TRUE
)
