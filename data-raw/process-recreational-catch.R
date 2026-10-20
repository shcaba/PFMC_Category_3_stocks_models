drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

# Note: currently no additional mortality is applied

# data source by state and years
# CA: 2013 assessment 1964-1979, mrfss_catch_raw: 1980-2003, fill in 2004, rec_catch_raw: 2005+
# OR: or_rec_hist_raw: 1979-1990, mrfss_catch_raw 1991-2000, rec_catch_raw: 2001+
# WA: wa_rec_hist_raw: 1968-1986, mrfss_catch_raw 1987-1989, rec_catch_raw: 1990+

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
# mrfss_catch_raw <- read.csv(here::here(
#   drive,
#   "Pacific Sanddab",
#   "recfin",
#   "CTE510-1980---2002.csv"
# ))

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
  "Catch reconstructions", #"Pacific Sanddab",
  "OR recreational", #"recfin",
  "CTE507-1979---2000.csv"
))
# there were no CA records for Pacific sanddab found in the historical catches
# use the values from the assessment which used a regression estimator
#ca_rec_hist_raw <- read.csv(here::here(
#  drive,
#  "Pacific Sanddab",
#  "recfin",
#  "california_historical_rec_catch_2013_assessment.csv"
#)) |>
#  dplyr::mutate(source = "2013 assessment")
ca_rec_hist_raw <- read.csv(here::here(
  drive,
  "Catch reconstructions",
  "CA recreational",
  "CTE508-1928---1980.csv"
))
# This matches the above data set for bank and stripetail
# ca_rec_hist_rock_raw <- readr::read_rds(here::here(
#   drive,
#   "Catch reconstructions",
#   "CA recreational",
#   "ca_rec_recon_rockfish.rds"
# ))

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
  dplyr::filter(
    IS_RETAINED == "RETAINED",
    SPECIES_NAME %in%
      c("BANK ROCKFISH", "STRIPETAIL ROCKFISH", "PACIFIC SANDDAB")
  ) |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    species = tolower(SPECIES_NAME),
    state = tolower(STATE_NAME),
    mode = RECFIN_MODE_NAME
  ) |>
  dplyr::summarise(
    .by = c("species"), # the weights are very similar for OR and WA
    average_weight = mean(RECFIN_IMPUTED_WEIGHT_KG, na.rm = TRUE)
  )

# There are no biological samples from Washington
# Ave weight for modes were similar in CA, except shore modes
# Ave weight for modes were noisy in OR due to low samples
# Calculating a single average weight for all modes and states
# for each year

#==============================================================================
# Format all the catch files
#==============================================================================
rec_catch_format <- rec_catch_raw |>
  dplyr::filter(
    SPECIES_NAME %in%
      c("PACIFIC SANDDAB", "BANK ROCKFISH", "STRIPETAIL ROCKFISH")
  ) |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = tolower(STATE_NAME),
    species = tolower(SPECIES_NAME)
  )
# by species there are some records with TOTAL_MORTALITY_MT == 0 bu TOTAL_MORTALITY_NUM > 0
#             species TOTAL_MORTALITY_NUM
#       bank rockfish            798.8527 ~ 1%
#     pacific sanddab            192.5523 ~ 0%
# stripetail rockfish            618.5901 ~ 12%
# for each species, these numbers are very limited compared to the records with mortality:
#             species TOTAL_MORTALITY_NUM
#       bank rockfish           79260.984
#     pacific sanddab         5411080.413
# stripetail rockfish            3583.08
# The largest percent is for stripetail, but overall recreational mortality is very small
# aggregate(TOTAL_MORTALITY_MT~species, rec_catch_format, sum)
#             species TOTAL_MORTALITY_MT
#       bank rockfish         59.8516370
#     pacific sanddab        801.7993693
# stripetail rockfish          0.9697197

# rec_catch_mean_weight <- dplyr::left_join(
#   x = rec_catch_format,
#   y = rec_bio_average,
#   by = c("year", "species")
# )

rec_catch_recent <- rec_catch_format |>
  dplyr::mutate(
    source = "recfin",
    fleet = "receational"
  ) |>
  dplyr::summarise(
    .by = c("source", "species", "fleet", "state", "year"),
    landings_mt = round(sum(TOTAL_MORTALITY_MT), 4),
    discard_mt = 0
  )

mrfss_catch <- mrfss_catch_all_raw |>
  dplyr::filter(
    SPECIES_NAME %in%
      c(
        "PACIFIC SANDDAB",
        "BANK ROCKFISH",
        "STRIPETAIL ROCKFISH"
      )
  ) |>
  dplyr::group_by(YEAR, SPECIES_NAME) |>
  dplyr::mutate(
    ave_weight_mt = mean(0.001 * WGT_AB1 / TOT_CAT, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    year = YEAR,
    species = tolower(SPECIES_NAME),
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
    !(year < 1987 & state == "washington"),
    state != "oregon"
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state", "year"),
    landings_mt = round(sum(catch_fill), 4),
    discard_mt = 0
  )

wa_rec_hist <- dplyr::left_join(
  wa_rec_hist_raw,
  rec_bio_average |> dplyr::rename(AGENCY_SPECIES_NAME = species)
) |>
  dplyr::filter(
    AGENCY_SPECIES_NAME %in%
      c(
        "BANK ROCKFISH",
        "PACIFIC SANDDAB",
        "STRIPETAIL ROCKFISH"
      )
  ) |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = "washington",
    species = tolower(AGENCY_SPECIES_NAME),
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
or_rec_hist <- dplyr::left_join(
  or_rec_hist_raw |> dplyr::mutate(SPECIES_NAME = tolower(SPECIES_NAME)),
  rec_bio_average |> dplyr::rename(SPECIES_NAME = species)
) |>
  dplyr::filter(
    SPECIES_NAME %in%
      c(
        "bank rockfish",
        "pacific sanddab",
        "stripetail rockfish"
      )
  ) |>
  dplyr::mutate(
    year = YEAR,
    state = "oregon",
    species = tolower(SPECIES_NAME),
    source = "recfin-historical",
    fleet = "receational"
  ) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state", "year"),
    landings_mt = round(sum(0.001 * average_weight * NUMBER_OF_FISH), 4),
    discard_mt = 0
  ) |>
  dplyr::arrange(year)

# Need to fill in CA for 2003 and 2004
ca_ave_catch_2003 <- dplyr::bind_rows(
  rec_catch_recent |>
    dplyr::filter(state == "california", year %in% 2005:2006),
  mrfss_catch |>
    dplyr::filter(state == "california", year %in% 2001:2002)
) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state"),
    year = 2003,
    landings_mt = mean(landings_mt),
    discard_mt = 0
  )
ca_ave_catch_2004 <- ca_ave_catch_2003 |>
  dplyr::mutate(year = 2004)

ca_ave_catch_1990 <- dplyr::bind_rows(
  mrfss_catch |>
    dplyr::filter(state == "california", year %in% 1988:1994)
) |>
  dplyr::summarise(
    .by = c("species", "source", "fleet", "state"),
    year = 1990,
    landings_mt = mean(landings_mt),
    discard = 0
  )
ca_ave_catch_1991 <- ca_ave_catch_1990 |>
  dplyr::mutate(year = 1991)
ca_ave_catch_1992 <- ca_ave_catch_1990 |>
  dplyr::mutate(year = 1992)

ca_rec_hist <- ca_rec_hist_raw |>
  dplyr::filter(
    SPECIES_NAME %in%
      c(
        "Bank Rockfish",
        "Pacific Sanddab",
        "Stripetail Rockfish"
      )
  ) |>
  dplyr::mutate(
    year = YEAR,
    species = tolower(SPECIES_NAME),
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
  or_rec_hist,
  wa_rec_hist,
  ca_rec_hist,
  mrfss_catch,
  rec_catch_recent,
  ca_ave_catch_2003,
  ca_ave_catch_2004,
  ca_ave_catch_1990,
  ca_ave_catch_1991,
  ca_ave_catch_1992,
  ca_rec_hist,
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
# Visual check
#===============================================================================

ggplot2::ggplot(
  recreational_catch,
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")

usethis::use_data(
  recreational_catch,
  overwrite = TRUE
)
