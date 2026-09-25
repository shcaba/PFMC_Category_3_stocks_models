drive <- "G:/Shared drives/NMFS NWC FRAM Population Ecology/Assessment Data (contains PII)/2027 Assessment Cycle/Category_3"

# 2013 Assessment historical catch
historical_catch_sanddab_raw <- read.csv(here::here(
  drive,
  "Pacific Sanddab",
  "historical_data",
  "pacific_sanddab_historical_catches_2013_assessment.csv"
)) |>
  dplyr::summarise(
    .by = c(species, source, year),
    catch_mt = sum(catch_mt)
  )
historical_catch_stripetail_raw <- read.csv(here::here(
  drive,
  "Stripetail Rockfish",
  "historical_data",
  "stripetail_rockfish_historical_catches_2013_assessment.csv"
)) |>
  dplyr::summarise(
    .by = c(species, source, year),
    catch_mt = sum(catch_mt)
  )

compare_catches <- dplyr::bind_rows(
  processed_final_catches |>
    dplyr::ungroup() |>
    dplyr::mutate(source = "new estimates") |>
    dplyr::summarise(
      .by = c(species, source, year),
      catch_mt = sum(catch_mt)
    ),
  historical_catch_stripetail_raw,
  historical_catch_sanddab_raw
)


ggplot2::ggplot(
  compare_catches,
  ggplot2::aes(x = year, y = landings_mt, color = source)
) +
  ggplot2::geom_line(size = 1) +
  ggplot2::theme_bw() +
  ggplot2::facet_grid("species", scales = "free_y")


gemm <- nwfscSurvey::pull_gemm(
  common_name = c("stripetail rockfish", "pacific sanddab")
)
gemm_formatted <- gemm |>
  dplyr::summarize(
    .by = c(year, species),
    source = "gemm",
    species = tolower(unique(species)),
    catch_mt = round(sum(mortality_landings_and_discard_mortality), 4)
  )
compare_with_gemm <- dplyr::bind_rows(
  processed_final_catches |>
    dplyr::filter(year >= 2002) |>
    dplyr::ungroup() |>
    dplyr::mutate(source = "new estimates") |>
    dplyr::summarise(
      .by = c(species, source, year),
      catch_mt = sum(catch_mt)
    ),
  gemm_formatted,
  historical_catch_stripetail_raw |> dplyr::filter(year >= 2002),
  historical_catch_sanddab_raw |> dplyr::filter(year >= 2002)
)

ggplot2::ggplot(
  compare_with_gemm |> dplyr::filter(year >= 2003),
  ggplot2::aes(x = year, y = catch_mt, color = source)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::theme_bw() +
  ggplot2::facet_grid("species", scales = "free_y")

# Shrimp Trawl
# sanddab: 113 mt, stripetail: 71 mt
