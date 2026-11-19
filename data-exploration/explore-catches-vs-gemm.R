species_list <- c(
  "bank rockfish",
  "pacific sanddab",
  "stripetail rockfish",
  "yellowmouth rockfish",
  "pacific cod",
  "starry rockfish",
  "shortraker rockfish",
  "redstripe rockfish",
  "silvergray rockfish",
  "rosethorn rockfish",
  "flathead sole"
)

gemm_raw <- nwfscSurvey::pull_gemm(
  common_name = species_list,
  verbose = FALSE
)

gemm_data <- gemm_raw |>
  dplyr::mutate(
    species = tolower(species)
  ) |>
  dplyr::summarise(
    .by = c(species, year),
    source = "GEMM",
    catch_mt = sum(mortality_landings_and_discard_mortality)
  )

model_catches <- processed_final_catches |>
  dplyr::filter(year >= 2002) |>
  dplyr::summarise(
    .by = c(species, year),
    source = "model",
    catch_mt = sum(catch_mt)
  )

all_data <- dplyr::bind_rows(model_catches, gemm_data)

ggplot2::ggplot(
  all_data,
  ggplot2::aes(x = year, y = catch_mt, color = source, linetype = source)
) +
  ggplot2::geom_line(size = 1) +
  ggplot2::theme_bw() +
  ggplot2::facet_wrap("species", scales = "free_y")


# Let's look at stripetail close
gemm_select <- gemm_raw |>
  dplyr::filter(
    species == "Stripetail Rockfish"
  ) |>
  dplyr::mutate(
    gear = dplyr::case_when(
      sector %in%
        c(
          "CS - Hook & Line",
          "Directed P Halibut",
          "LE Fixed Gear DTL - Hook & Line",
          "LE Sablefish - Hook & Line",
          "OA Fixed Gear - Hook & Line",
          "Nearshore",
          "CS - Pot",
          "CS EM - Pot",
          "LE Fixed Gear DTL - Pot",
          "OA Fixed Gear - Pot",
          "LE Sablefish - Pot"
        ) ~
        "Fixed Gears",
      sector %in%
        c(
          "California Recreational",
          "Oregon Recreational",
          "Washington Recreational"
        ) ~
        "Recreational",
      sector %in%
        c(
          "Research",
          "Incidental"
        ) ~
        "Research-Incidental",
      sector %in%
        c(
          "At-Sea Hake CP",
          "At-Sea Hake MSCV"
        ) ~
        "At-Sea",
      .default = "Bottom Trawl"
    )
  ) |>
  dplyr::summarise(
    .by = c(year, gear),
    catch_mt = round(sum(mortality_landings_and_discard_mortality), 2)
  )

ggplot2::ggplot(
  gemm_select,
  ggplot2::aes(x = year, y = catch_mt, fill = gear)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::xlab("Year") +
  ggplot2::ylab("GEMM Catch (mt)") +
  ggplot2::theme_bw()

# The difference appears to be coming from research catches.
# There is very little research catch in PacFIN data. These
# catches appear to be coming from WCGBTS.

# Let's look at redstripe closer
gemm_select <- gemm_raw |>
  dplyr::filter(
    species == "Redstripe Rockfish"
  ) |>
  dplyr::mutate(
    gear = dplyr::case_when(
      sector %in%
        c(
          "CS - Hook & Line",
          "Directed P Halibut",
          "LE Fixed Gear DTL - Hook & Line",
          "LE Sablefish - Hook & Line",
          "OA Fixed Gear - Hook & Line",
          "Nearshore",
          "CS - Pot",
          "CS EM - Pot",
          "LE Fixed Gear DTL - Pot",
          "OA Fixed Gear - Pot",
          "LE Sablefish - Pot"
        ) ~
        "Fixed Gears",
      sector %in%
        c(
          "California Recreational",
          "Oregon Recreational",
          "Washington Recreational"
        ) ~
        "Recreational",
      sector %in%
        c(
          "Research",
          "Incidental"
        ) ~
        "Research-Incidental",
      sector %in%
        c(
          "At-Sea Hake CP",
          "At-Sea Hake MSCV"
        ) ~
        "At-Sea",
      .default = "Bottom Trawl"
    )
  ) |>
  dplyr::summarise(
    .by = c(year, gear),
    catch_mt = round(sum(mortality_landings_and_discard_mortality), 2)
  )

ggplot2::ggplot(
  gemm_select,
  ggplot2::aes(x = year, y = catch_mt, fill = gear)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::xlab("Year") +
  ggplot2::ylab("GEMM Catch (mt)") +
  ggplot2::theme_bw()

# The difference appears to be coming from the at-sea fishery
# We are waiting for these catch estimates from Vanessage
