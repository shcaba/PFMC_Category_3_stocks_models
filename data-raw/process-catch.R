processed_final_catches <- dplyr::bind_rows(
  commercial_catch,
  recreational_catch |>
    dplyr::select(-source, -fleet)
) |>
  dplyr::summarise(
    .by = c(species, year),
    landings_mt = sum(landings_mt),
    discard_mt = sum(discard_mt),
    catch_mt = sum(catch_mt)
  )

# format for stock synthesis
catches_ss3_formatted <- dplyr::bind_rows(
  processed_final_catches |>
    dplyr::select(species, year, landings_mt) |>
    dplyr::mutate(
      season = 1,
      fleet = "landings-fleet",
      catch_se = 0.01
    ) |>
    dplyr::rename(catch = landings_mt),
  processed_final_catches |>
    dplyr::select(species, year, discard_mt) |>
    dplyr::mutate(
      season = 1,
      fleet = "discard-fleet",
      catch_se = 0.01
    ) |>
    dplyr::rename(catch = discard_mt)
) |>
  dplyr::relocate(catch, .before = catch_se) |>
  dplyr::arrange(species)


usethis::use_data(
  processed_final_catches,
  overwrite = TRUE
)

write_named_csvs(
  catches_ss3_formatted,
  dir = "data-tables"
)

#===============================================================================
# Visual check
#===============================================================================

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
