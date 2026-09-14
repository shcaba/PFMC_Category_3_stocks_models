drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

# data source by state and years
# CA: mrfss_catch_raw: 1980-2003, fill in 2004, rec_catch_raw: 2005+
# OR: or_rec_hist_raw: 1979-1990, mrfss_catch_raw 1991-2000, rec_catch_raw: 2001+
# WA: wa_rec_hist_raw: 1968-1986, mrfss_catch_raw 1987-1989, rec_catch_raw: 1990+

# recent catch
rec_catch_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "recfin",
  "CTE051-WASHINGTON-OREGON-CALIFORNIA-1990---2025.csv"
))
# available years by state in CTE051:
# WA: 1990+, OR: 2001+, CA:2005+

# MRFSS 1980-2003
mrfss_catch_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "recfin",
  "CTE510-1980---2002.csv"
))

# historical catch
# WA: 1968-1986
wa_rec_hist_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "recfin",
  "CTE503-1967---2002.csv"
))
# Oregon: 1979-1990
or_rec_hist_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "recfin",
  "CTE507-1979---2000.csv"
))
# there were no CA records for Pacific sanddab found in the historical catches
# use the values from the assessment which used a regression estimator
ca_rec_hist_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "recfin",
  "california_historical_rec_catch_2013_assessment.csv"
))

# Read in bio samples in order to fill in missing weights for the catch
rec_bio_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "recfin",
  "SD501-WASHINGTON-OREGON-CALIFORNIA-1980---2025.csv"
))

rec_bio_average <- rec_bio_raw |>
  dplyr::filter(IS_RETAINED == "RETAINED") |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = tolower(STATE_NAME),
    mode = RECFIN_MODE_NAME,
    species = "pacific sanddab"
  ) |>
  dplyr::summarise(
    .by = c("year", "species"),
    average_weight = mean(RECFIN_IMPUTED_WEIGHT_KG, na.rm = TRUE)
  )
# There are no biological samples from Washington
# Ave weight for modes were similar in CA, except shore modes
# Ave weight for modes were noisy in OR due to low samples
# Calculating a single average weight for all modes and states
# for each year

rec_catch_format <- rec_catch_raw |>
  dplyr::filter(SPECIES_NAME == "PACIFIC SANDDAB") |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = tolower(STATE_NAME),
    species = "pacific sanddab"
  )

rec_catch_mean_weight <- dplyr::left_join(
  x = rec_catch_format,
  y = rec_bio_average,
  by = c("year", "species")
)

rec_catch_recent <- rec_catch_mean_weight |>
  dplyr::mutate(
    catch_mt = dplyr::if_else(
      TOTAL_MORTALITY_MT > 0,
      TOTAL_MORTALITY_MT,
      0.001 * average_weight * TOTAL_MORTALITY_NUM
    ),
    catch_mt = dplyr::if_else(
      !is.na(catch_mt),
      catch_mt,
      0.001 * average_weight * TOTAL_MORTALITY_NUM
    )
  ) |>
  dplyr::summarise(
    .by = c("species", "state", "year"),
    catch_mt = round(sum(catch_mt), 4)
  )

mrfss_catch <- mrfss_catch_raw |>
  dplyr::group_by(YEAR) |>
  dplyr::mutate(
    ave_weight_mt = mean(0.001 * WGT_AB1 / TOT_CAT, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    year = YEAR,
    state = tolower(ST_NAME),
    species = "pacific sanddab",
    catch_raw = 0.001 * WGT_AB1,
    catch_fill = dplyr::if_else(
      is.na(catch_raw),
      ave_weight_mt * TOT_CAT,
      catch_raw
    )
  ) |>
  dplyr::filter(
    !(year < 1987 & state == "washington"),
    state != "oregon"
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    .by = c("species", "state", "year"),
    catch_mt = round(sum(catch_fill), 4)
  )

ave_weight <- rec_bio_average |>
  dplyr::summarise(
    ave_weight = mean(average_weight)
  )
wa_rec_hist <- wa_rec_hist_raw |>
  dplyr::mutate(
    year = RECFIN_YEAR,
    state = "washington",
    species = "pacific sanddab",
    ave_weight = ave_weight
  ) |>
  dplyr::summarise(
    .by = c("species", "state", "year"),
    catch_mt = round(sum(0.001 * ave_weight * RETAINED_NUM), 4)
  ) |>
  dplyr::arrange(year)

or_rec_hist <- or_rec_hist_raw |>
  dplyr::filter(SPECIES_NAME == "Pacific Sanddab") |>
  dplyr::mutate(
    year = YEAR,
    state = "oregon",
    species = "pacific sanddab",
    ave_weight = ave_weight
  ) |>
  dplyr::summarise(
    .by = c("species", "state", "year", ),
    catch_mt = round(sum(0.001 * ave_weight * NUMBER_OF_FISH), 4)
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
    .by = c("species", "state"),
    year = 2003,
    catch_mt = mean(catch_mt)
  )
ca_ave_catch_2004 <- ca_ave_catch_2003 |>
  dplyr::mutate(year = 2004)

ca_ave_catch_1990 <- dplyr::bind_rows(
  mrfss_catch |>
    dplyr::filter(state == "california", year %in% 1988:1994)
) |>
  dplyr::summarise(
    .by = c("species", "state"),
    year = 1990,
    catch_mt = mean(catch_mt)
  )
ca_ave_catch_1991 <- ca_ave_catch_1990 |>
  dplyr::mutate(year = 1991)
ca_ave_catch_1992 <- ca_ave_catch_1990 |>
  dplyr::mutate(year = 1992)

recreational_catch <- dplyr::bind_rows(
  or_rec_hist,
  wa_rec_hist,
  mrfss_catch,
  rec_catch_recent,
  ca_ave_catch_2003,
  ca_ave_catch_2004,
  ca_ave_catch_1990,
  ca_ave_catch_1991,
  ca_ave_catch_1992,
  ca_rec_hist_raw
) |>
  dplyr::summarise(
    .by = c("species", "year"),
    catch_mt = sum(catch_mt)
  )

usethis::use_data(
  recreational_catch,
  overwrite = TRUE
)
