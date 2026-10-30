#' Combine composition data from multiple sources
#'
#'
#' @param comps_1 Data frame of formatted SS3 length composition data.
#' @param comps_2 Data frame of formatted SS3 length composition data.
#'
#' @author Chantel Wetzel
#' @export
#'
combine_comps <- function(
  comps_1,
  comps_2
) {
  colnames(comps_2) <- colnames(comps_1)
  all_comps <- dplyr::bind_rows(
    as.data.frame(comps_1) |>
      dplyr::mutate(month = as.numeric(month)),
    comps_2
  )
  return(all_comps)
}
