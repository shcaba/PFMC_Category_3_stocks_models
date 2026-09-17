# This script does the following:
# 1. summarized landings from PacFIN
# 2. pulls in historical reconstructions (or catch from the 2013 assessments)
# 3. brings in the GEMM discard rates to calculated catch for pacfin and historical
# years.

drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

# Historical catch reconstructions
WA_rf_ct_recon <- read.csv(here::here(
  drive,
  "WA rockfish catch reconstruction",
  "SpeciesSumOutput2_2017.csv"
))

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
pacfin_landings_by_state_gear <- dplyr::bind_rows(
  landings_stripetail[, cols_to_keep],
  landings_sanddab[, cols_to_keep],
  landings_sanddab_general[, cols_to_keep] |>
    dplyr::filter(LANDING_YEAR < 2003) |>
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
    gear = pacfin_group_gear_code,
    state = agency_code,
    fleet = "commercial",
    gear_group = dplyr::case_when(
      gear %in% c("TLS", "HKL", "POT") ~ "fixed-gear",
      TRUE ~ "trawl"
    ),
    source = "pacfin"
  ) |>
  dplyr::summarise(
    .by = c(species, source, fleet, year, gear_group),
    landings_mt = round(sum(landings_mt), 4),
    catch_mt = 0
  )


#===============================================================================
# State provided historical reconstructions
#===============================================================================
# need to check the area regions: should VN be included, Oregon?
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
    gear_group = dplyr::recode_values(
      geargroup,
      "JIG" ~ "HKL",
      "HKL" ~ "HKL",
      "TWL" ~ "TWL"
    ),
    species_metric_tons = 0.000453592 * speciespounds,
    source = "wa_rockfish_reconstruction",
    fleet = "commercial",
    state = "washington"
  ) |>
  dplyr::summarise(
    .by = c(species, source, fleet, year, gear_group),
    landings_mt = round(sum(species_metric_tons, na.rm = TRUE), 4),
    catch_mt = 0
  ) |>
  dplyr::filter(
    species == "stripetail rockfish",
    year < 1981
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
  dplyr::mutate(
    gear_group = "unknown"
  ) |>
  dplyr::summarise(
    .by = c(species, source, fleet, year, gear_group),
    landings_mt = 0,
    catch_mt = sum(catch_mt)
  )

historical_catch_stripetail <- historical_catch_stripetail_raw |>
  dplyr::filter(
    year < 1981
  ) |>
  dplyr::mutate(
    gear_group = "unknown"
  ) |>
  dplyr::summarise(
    .by = c(species, source, fleet, year, gear_group),
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
  pacfin_landings_by_state_gear,
  commercial_reconstruction_wa,
  historical_catch_sanddab
) |>
  dplyr::summarise(
    .by = c(species, source, fleet, year),
    landings_mt = sum(landings_mt),
    catch_mt = sum(catch_mt)
  )

# Issues: without a new catch reconstruction from OR, the above
# data frame does not include Oregon landings from 1981-1985

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
  dplyr::filter(year < 2026) |>
  dplyr::group_by(species) |>
  dplyr::mutate(
    average_discard_rate = mean(discard_rate, na.rm = TRUE),
    rate_applied = dplyr::if_else(
      is.na(discard_rate),
      average_discard_rate,
      discard_rate
    ),
    discard_mortality = round(
      (rate_applied * landings_mt) / (1 - rate_applied),
      4
    ),
    final_catch_mt = round(discard_mortality + landings_mt + catch_mt, 4)
  ) |>
  #dplyr::mutate(
  #  fleet = "commercial",
  #  source = "pacfin-with-discard"
  #) |>
  dplyr::select(
    source,
    species,
    fleet,
    year,
    final_catch_mt
  ) |>
  dplyr::rename(catch_mt = final_catch_mt)

# Note: the above approach does not apply any discard mortality adjustment to
# values from the 2013 assessments. Hence, the discard mortality assumed before
# 1981 may be inconsistent with the 1981+ rates applied.

#===============================================================================
# Visual check
#===============================================================================

ggplot2::ggplot(
  commercial_catch,
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")

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
