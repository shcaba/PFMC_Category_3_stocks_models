processed_final_catches <- dplyr::bind_rows(
  commercial_catch,
  recreational_catch
)

#===============================================================================
# Visual check
#===============================================================================

ggplot2::ggplot(
  processed_final_catches,
  ggplot2::aes(x = year, y = catch_mt, fill = source)
) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_grid("species", scales = "free_y")

usethis::use_data(
  processed_final_catches,
  overwrite = TRUE
)
