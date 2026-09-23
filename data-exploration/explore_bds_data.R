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
  Pdata = bds_raw_stripetail
)
# 6845 records: CA 4705, OR 1978, WA 162
# TWL: 6073, MID: 678
# SEX F: 5906, M: 454, U: 485
# observed size of females > males

bds_clean_sanddab <- pacfintools::cleanPacFIN(
  Pdata = bds_raw_sanddab
)
# 41776 records: CA 25050, OR 16606, WA 120
# HKL: 5883, TWL: 35147
# SEX F: 32605, M: 5527, U: 3644
# observed size of females > males

ggplot2::ggplot(
  bds_clean_sanddab |> dplyr::mutate(count = 1),
  ggplot2::aes(x = lengthcm, y = count)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("geargroup")
ggplot2::ggplot(
  bds_clean_sanddab |> dplyr::mutate(count = 1),
  ggplot2::aes(x = Age, y = count)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("geargroup")

ggplot2::ggplot(
  bds_clean_stripetail |> dplyr::mutate(count = 1),
  ggplot2::aes(x = lengthcm, y = count)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap("geargroup")


ggplot2::ggplot(
  bds_clean_sanddab |> dplyr::mutate(count = 1),
  ggplot2::aes(x = lengthcm, y = count)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("SEX_CODE")

ggplot2::ggplot(
  bds_clean_stripetail |> dplyr::mutate(count = 1),
  ggplot2::aes(x = lengthcm, y = count)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("SEX_CODE")

biological_data = fs::dir_ls(
  "C:/Assessments/wcgop/data_2023",
  regex = "Observer_Biological_Data_2002"
) |>
  purrr::map_df(
    .f = function(x) {
      load(x)
      return(OBBio2)
    }
  )


wcgop_data <- biological_data |>
  dplyr::filter(
    CATCH_DISPOSITION == "D",
    species %in% c("Stripetail Rockfish", "Pacific Sanddab")
  ) |>
  dplyr::mutate(
    year = RYEAR,
    # nearly all observations are unsexed
    sex = nwfscSurvey::codify_sex(SEX)
  ) |>
  tidyr::uncount(
    FREQUENCY
  ) |>
  dplyr::filter(
    LENGTH < 60 #,
    #gear != "Shrimp Trawl"
  )
# Nearly all observations are for bottom trawl
# Pacific sanddab: 27201 (w/ st 33505)
# Stripetail rockfish: 22924 (w/ st 43139)
ggplot2::ggplot(
  wcgop_data |>
    dplyr::mutate(count = 1) |>
    dplyr::filter(gear %in% c("Bottom Trawl", "Shrimp Trawl")),
  ggplot2::aes(x = LENGTH, y = count)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid(c("gear", "species"))

all_bds <- dplyr::bind_rows(
  bds_clean_sanddab |>
    dplyr::select(year, geargroup, SEX_CODE, lengthcm, Age) |>
    dplyr::rename(
      sex = SEX_CODE,
      age = Age,
      length_cm = lengthcm
    ) |>
    dplyr::filter(geargroup %in% c("TWL", "MID", "HKL", "POT")) |>
    dplyr::mutate(
      source = "pacfin",
      species = "pacific sanddab",
      count = 1,
      gear_group = dplyr::recode_values(
        geargroup,
        "HKL" ~ "FG",
        "POT" ~ "FG",
        "MID" ~ "MID",
        "TWL" ~ "TWL"
      )
    ),
  bds_clean_stripetail |>
    dplyr::select(year, geargroup, SEX_CODE, lengthcm, Age) |>
    dplyr::rename(
      sex = SEX_CODE,
      age = Age,
      length_cm = lengthcm
    ) |>
    dplyr::filter(geargroup %in% c("TWL", "MID", "HKL", "POT")) |>
    dplyr::mutate(
      source = "pacfin",
      species = "stripetail rockfish",
      count = 1,
      gear_group = dplyr::recode_values(
        geargroup,
        "HKL" ~ "FG",
        "POT" ~ "FG",
        "MID" ~ "MID",
        "TWL" ~ "TWL"
      )
    ),
  wcgop_data |>
    dplyr::select(species, year, gear, sex, LENGTH, AGE) |>
    dplyr::rename(
      length_cm = LENGTH,
      age = AGE
    ) |>
    dplyr::mutate(
      source = "wcgop",
      species = tolower(species),
      count = 1,
      gear_group = dplyr::recode_values(
        gear,
        "Bottom Trawl" ~ "TWL",
        "Fixed Gears" ~ "FG",
        "Hook & Line" ~ "FG",
        "Pot" ~ "FG"
      )
    )
)

ggplot2::ggplot(
  all_bds,
  ggplot2::aes(x = length_cm, y = count, fill = sex)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::facet_grid(c("source", "species"))


#===============================================================================
# Look at the bds records in WA where there is no PacFIN landings
#===============================================================================

wa_sanddab <- bds_clean_sanddab |>
  dplyr::filter(AGENCY_CODE == "W") |>
  dplyr::summarise(
    .by = c(year, FTID),
    n = dplyr::n()
  )
# year     FTID  n
# 2021 EA001769 50
# 2021 EA001783 50
# 2023 EA010270 20

load(here::here(
  drive,
  "Pacific Sanddab",
  "pacfin",
  "2026-09-10",
  "PacFIN.UDAB.CompFT.16.Sep.2026.RData"
))
landings_sanddab_general <- catch.pacfin

load(here::here(
  drive,
  "Pacific Sanddab",
  "pacfin",
  "2026-09-10",
  "PacFIN.PDAB.CompFT.10.Sep.2026.RData"
))
landings_sanddab <- catch.pacfin
landings_sanddab |>
  dplyr::filter(AGENCY_CODE == "W", LANDING_YEAR %in% c(2021, 2023)) |>
  dplyr::summarise(
    .by = c(LANDING_YEAR, FTID),
    total = sum(ROUND_WEIGHT_MTONS)
  )
landings_sanddab_general |>
  dplyr::filter(AGENCY_CODE == "W", LANDING_YEAR %in% c(2021, 2023)) |>
  dplyr::summarise(
    .by = c(LANDING_YEAR, FTID),
    total = sum(ROUND_WEIGHT_MTONS)
  )

# Look at the observations of speckled vs. Pacific in survey data
survey_catch <- nwfscSurvey::pull_catch(
  common_name = c("Pacific sanddab", "speckled sanddab")
)

data <- survey_catch |>
  dplyr::mutate(
    state = dplyr::if_else(
      Latitude_dd < 42.0,
      "C",
      dplyr::if_else(Latitude_dd > 46.25, "W", "O")
    )
  ) |>
  dplyr::summarise(
    .by = c(Common_name, state, Year),
    weight = sum(total_catch_wt_kg),
    numbers = sum(total_catch_numbers),
    cpue = sum(cpue_kg_km2)
  )
# no observations of speckled sanddab in the survey data

# there are no landings in pacfin for speckled sanddab
load(
  "C:/Assessments/pacfin/speckled_sanddab/2026-09-22/PacFIN.SSDB.CompFT.22.Sep.2026.RData"
)
landings_speckled <- catch.pacfin


#===============================================================================
# Look at the bds records in CA-1999 where there is no PacFIN landings
#===============================================================================

ca_stripetail_ftid <- bds_clean_stripetail |>
  dplyr::filter(AGENCY_CODE == "C", year == 1999) |>
  dplyr::summarise(
    .by = c(year, FTID),
    n = dplyr::n()
  )

landings_stripetail |>
  dplyr::filter(LANDING_YEAR == 1999, AGENCY_CODE == "C")
# no species-specific landings...

st_wl <- weight_length_estimates |>
  dplyr::filter(species == "stripetail rockfish")
fa <- st_wl |>
  dplyr::filter(sex == "female") |>
  dplyr::pull("A")
fb <- st_wl |>
  dplyr::filter(sex == "female") |>
  dplyr::pull("B")
ma <- st_wl |>
  dplyr::filter(sex == "male") |>
  dplyr::pull("A")
mb <- st_wl |>
  dplyr::filter(sex == "male") |>
  dplyr::pull("B")
ua <- st_wl |>
  dplyr::filter(sex == "all") |>
  dplyr::pull("A")
ub <- st_wl |>
  dplyr::filter(sex == "all") |>
  dplyr::pull("B")
get_sample_weight <- getExpansion_1(
  Pdata = ca_stripetail |>
    dplyr::filter(AGENCY_CODE == "C", year == 1999),
  fa = fa,
  fb = fb,
  ma = ma,
  mb = mb,
  ua = ua,
  ub = ub,
  maxExp = 0.90
)
0.000453592 * sum(get_sample_weight$Trip_Sampled_Lbs)
# 159.5 mt
0.000453592 * sum(get_sample_weight$CLUSTER_WEIGHT_LBS)
# 0.27 mt
#===============================================================================
# The nightmare of unspecified small rockfish....
#===============================================================================
load(
  "C:/Assessments/pacfin/unspecified_small_rockfish/2026-09-22/PacFIN.RCK5.CompFT.22.Sep.2026.RData"
)
small_rockfish_unspecified <- catch.pacfin

load(
  "C:/Assessments/pacfin/unspecified_rockfish/2026-09-22/PacFIN.URCK.CompFT.22.Sep.2026.RData"
)
rockfish_unspecified <- catch.pacfin

small_unspecified <- catch.pacfin |>
  dplyr::summarise(
    .by = c("LANDING_YEAR", "AGENCY_CODE"),
    landings_mt = round(sum(ROUND_WEIGHT_MTONS), 4)
  )

general_unspecified <- rockfish_unspecified |>
  dplyr::summarise(
    .by = c("LANDING_YEAR", "AGENCY_CODE"),
    landings_mt = round(sum(ROUND_WEIGHT_MTONS), 4)
  )

only_stripetail_ca <- small_rockfish_unspecified |>
  dplyr::filter(FTID %in% toupper(ca_stripetail_ftid$FTID)) |>
  dplyr::summarise(
    .by = c("LANDING_YEAR", "AGENCY_CODE"),
    landings_mt = round(sum(ROUND_WEIGHT_MTONS), 4)
  )
# 15.43 mt for these fish tickets only

only_stripetail_general_ca <- rockfish_unspecified |>
  dplyr::filter(FTID %in% toupper(ca_stripetail_ftid$FTID)) |>
  dplyr::summarise(
    .by = c("LANDING_YEAR", "AGENCY_CODE"),
    landings_mt = round(sum(ROUND_WEIGHT_MTONS), 4)
  )
# 9.843 mt for these fish tickets only

found_ftid <- unique(dplyr::bind_rows(
  small_rockfish_unspecified |>
    dplyr::filter(FTID %in% toupper(ca_stripetail_ftid$FTID)) |>
    dplyr::summarise(
      .by = FTID
    ),
  rockfish_unspecified |>
    dplyr::filter(FTID %in% toupper(ca_stripetail_ftid$FTID)) |>
    dplyr::summarise(
      .by = FTID
    )
))

missing_ftid <- bds_clean_stripetail |>
  dplyr::filter(AGENCY_CODE == "C", year == 1999) |>
  dplyr::summarise(
    .by = FTID
  ) |>
  dplyr::mutate(FTID = toupper(FTID)) |>
  dplyr::filter(!FTID %in% found_ftid$FTID)

# Missing FTID
# J094262 - rockfish, group rosefish RCK6
# X211337 = 2024 WA tuna

# K105642 - rockfish unspecified 1366 lbs 0.62 mt - actually attributed to nom. chilipepper
# based on the landed weight of 11451
# X214404 - rockfish unspecified 8231 lbs and small rockfish 3264 lbs

bds_clean_stripetail |>
  dplyr::filter(FTID == "k105642") # 11451

get_sample_weight |>
  dplyr::filter(FTID == "J096781")
