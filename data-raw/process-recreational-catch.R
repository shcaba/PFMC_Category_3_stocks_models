drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

# Note: currently no additional mortality is applied

# data source by state and years
# California
# ca_rec_hist_raw: 1928-1980
# mrfss_catch_raw: 1980-2004, missing 1990-1992
# rec_catch_raw: 2005+,
# Oregon:
# or_rec_hist_raw: 1979-1990,
# mrfss_catch_raw 1993-2000, missing 1990-1992
# rec_catch_raw: 2001+
# Wasington:
# wa_rec_hist_raw: 1968-2002,
# rec_catch_raw: 1990+

species_list <- c(
  "bank rockfish",
  "pacific sanddab",
  "stripetail rockfish",
  "yellowmouth rockfish",
  "pacific cod",
  "starry rockfish",
  # There is essentially no data for shortrake
  #"shortraker rockfish",
  "redstripe rockfish",
  "silvergray rockfish",
  "rosethorn rockfish",
  "flathead sole"
)

# available years by state in CTE051:
# WA: 1990+, OR: 2001+, CA:2005+
rec_catch_raw <- read.csv(here::here(
  drive,
  "Catch reconstructions",
  "recfin",
  "CTE051-WASHINGTON-OREGON-CALIFORNIA-1990---2025.csv"
))

# MRFSS 1980-2003
mrfss_catch_all_raw <- read.csv(here::here(
  drive,
  "Catch reconstructions",
  "mrfss",
  "CTE008-1980---2004.csv"
))

# historical catch
# WA: 1968-1986
wa_rec_hist_raw <- read.csv(here::here(
  drive,
  "Catch reconstructions", #"Pacific Sanddab",
  "WA recreational", #"recfin",
  "CTE503-1967---2002.csv"
))

# Oregon: 1979-1990
or_rec_hist_raw <- read.csv(here::here(
  drive,
  "Catch reconstructions",
  "OR recreational",
  "CTE507-1979---2000.csv"
))

ca_rec_hist_raw <- read.csv(here::here(
  drive,
  "Catch reconstructions",
  "CA recreational",
  "CTE508-1928---1980.csv"
))

# there were no CA records for Pacific sanddab found in the historical catches
# use the values from the assessment which used a regression estimator
historical_catch_sanddab_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "historical_data",
  "pacific_sanddab_historical_catches_2013_assessment.csv"
)) |>
  dplyr::filter(state == "coastwide", fleet == "recreational", year < 1980)

# Read in bio samples in order to fill in missing weights for the catch
rec_bio_raw <- dplyr::bind_rows(
  read.csv(here::here(
    drive,
    "Catch reconstructions",
    "recfin",
    "SD501-WASHINGTON-1980---2025.csv"
  )),
  read.csv(here::here(
    drive,
    "Catch reconstructions",
    "recfin",
    "SD501-OREGON-1980---2025.csv"
  ))
)

rec_bio_average <- rec_bio_raw |>
  dplyr::mutate(
    species = tolower(SPECIES_NAME),
  ) |>
  dplyr::filter(
    IS_RETAINED == "RETAINED",
    species %in% species_list
  ) |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = tolower(STATE_NAME),
    mode = RECFIN_MODE_NAME
  ) |>
  dplyr::summarise(
    .by = c("species"), # the weights are very similar for OR and WA
    average_weight = mean(RECFIN_IMPUTED_WEIGHT_KG, na.rm = TRUE),
    agency_average_weight = mean(AGENCY_WEIGHT, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    average_weight = dplyr::if_else(
      species == "yellowmouth rockfish",
      agency_average_weight,
      average_weight
    )
  ) |>
  dplyr::select(-agency_average_weight)


#==============================================================================
# Format all the catch files
#==============================================================================
rec_catch_format <- rec_catch_raw |>
  dplyr::mutate(species = tolower(SPECIES_NAME)) |>
  dplyr::filter(
    species %in% species_list
  ) |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = tolower(STATE_NAME)
  )

# CA starts in 2005,
# OR starts in 2001,
# WA starts in 1990 - remove all records before 2003 in lieu of the reconstr.
rec_catch_recent <- rec_catch_format |>
  dplyr::mutate(
    source = "recfin",
    fleet = "receational"
  ) |>
  dplyr::summarise(
    .by = c("source", "species", "fleet", "state", "year"),
    landings_mt = round(sum(TOTAL_MORTALITY_MT), 4),
    discard_mt = 0
  ) |>
  dplyr::filter(
    !(state == "washington" & year <= 2002)
  )

# 1981-2004, remove oregon data, remove ca data for 1980
mrfss_catch <- mrfss_catch_all_raw |>
  dplyr::mutate(
    species = tolower(SPECIES_NAME)
  ) |>
  dplyr::filter(
    species %in% species_list
  ) |>
  dplyr::group_by(YEAR, species) |>
  dplyr::mutate(
    ave_weight_mt = mean(0.001 * WGT_AB1 / TOT_CAT, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    year = YEAR,
    state = tolower(ST_NAME),
    catch_raw = 0.001 * WGT_AB1,
    catch_fill = dplyr::if_else(
      is.na(catch_raw),
      ave_weight_mt * TOT_CAT,
      catch_raw
    ),
    source = "recfin",
    fleet = "receational"
  ) |>
  dplyr::filter(
    state != "oregon",
    year != 1980
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state", "year"),
    landings_mt = round(sum(catch_fill), 4),
    discard_mt = 0
  )

# Need to fill in CA for 1990 to 1992
mrfss_catch_filled <- mrfss_catch |>
  tidyr::complete(
    species,
    fleet,
    state,
    year = min(year):max(year),
    fill = list(source = "interpolate", landings_mt = NA)
  ) |>
  dplyr::group_by(species, state, fleet) |>
  dplyr::mutate(
    discard_mt = 0,
    landings_mt = stats::approx(year, landings_mt, n = length(year))$y
  ) |>
  # remove all interpolated years outside of 1990-1992
  dplyr::filter(
    !(source == "interpolate" & !year %in% 1990:1992)
  )

# 1967-2002
# only pacific cod data goes to 2002, most others only go to ~1985
wa_rec_hist <- dplyr::left_join(
  wa_rec_hist_raw |> dplyr::mutate(species = tolower(SPECIES_NAME)),
  rec_bio_average,
  by = dplyr::join_by(species)
) |>
  dplyr::filter(
    species %in% species_list
  ) |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = "washington",
    source = "recfin-historical",
    fleet = "receational"
  ) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state", "year"),
    landings_mt = round(sum(0.001 * average_weight * RETAINED_NUM), 4),
    discard_mt = 0
  ) |>
  dplyr::arrange(year)

# Update the average weight
# 1979-2000
or_rec_hist <- dplyr::left_join(
  or_rec_hist_raw |> dplyr::mutate(species = tolower(SPECIES_NAME)),
  rec_bio_average,
  by = dplyr::join_by(species)
) |>
  dplyr::filter(
    species %in% species_list
  ) |>
  dplyr::mutate(
    year = YEAR,
    state = "oregon",
    source = "recfin-historical",
    fleet = "receational"
  ) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state", "year"),
    landings_mt = round(sum(0.001 * average_weight * NUMBER_OF_FISH), 4),
    discard_mt = 0
  ) |>
  dplyr::arrange(year)

# 1928 -1980
ca_rec_hist <- ca_rec_hist_raw |>
  dplyr::mutate(
    species = tolower(SPECIES_NAME)
  ) |>
  dplyr::filter(
    species %in% species_list
  ) |>
  dplyr::mutate(
    year = YEAR,
    source = "recfin-historical",
    fleet = "receational",
    state = tolower(STATE_NAME)
  ) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state", "year"),
    landings_mt = sum(RETAINED_MT, na.rm = TRUE),
    discard_mt = 0
  )

recreational_catch <- dplyr::bind_rows(
  or_rec_hist, # 1979-2000
  wa_rec_hist, # 1967-2002
  ca_rec_hist, # 1928-1980
  mrfss_catch_filled, # 1981-2004 ca only
  # wa: 1990-2025 <- need to fix remove 1990-2002
  # or: 2001-2025
  # ca: 2005-2025
  rec_catch_recent,
  historical_catch_sanddab_raw |>
    dplyr::rename(landings_mt = catch_mt) |>
    dplyr::mutate(discard_mt = 0)
) |>
  dplyr::filter(!is.na(species)) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "year"),
    discard_mt = sum(discard_mt),
    landings_mt = sum(landings_mt),
    catch_mt = sum(discard_mt + landings_mt)
  )

#===============================================================================
# Save data
#===============================================================================

usethis::use_data(
  recreational_catch,
  overwrite = TRUE
)

#===============================================================================
# Visual check
#===============================================================================

ggplot2::ggplot(
  recreational_catch,
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("species", scales = "free_y")


ggplot2::ggplot(
  recreational_catch |>
    dplyr::filter(species %in% c("redstripe rockfish", "silvergray rockfish")),
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")

ggplot2::ggplot(
  recreational_catch |>
    dplyr::filter(species %in% c("rosethorn rockfish", "starry rockfish")),
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")

ggplot2::ggplot(
  recreational_catch |>
    dplyr::filter(species %in% c("pacific cod", "flathead sole")),
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")
