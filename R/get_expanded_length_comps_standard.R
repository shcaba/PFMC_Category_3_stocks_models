#' Expand fishery composition data and format for SS3
#'
#'
#' @param data PacFIN bds data that has been cleaned using [pacfintools::cleanPacFIN()]
#' @param catch Formatted catch for 2nd stage composition expansion. The catch
#'   should be formatted using [pacfintools::formatCatch()]
#' @param weight_length Weight length paramaters used for first stage expansion. The
#'   weight length parameters should be created using [nwfscSurvey::estimate_weight_length()]
#' @param length_bins String of numerical length bins
#' @param input_n_method Method for calculating input sample size
#' @param verbose TRUE/FALSE Print messages to the screen
#'
#' @author Chantel Wetzel
#' @export
#'
get_expanded_length_comps_standard <- function(
  data,
  catch,
  weight_length,
  length_bins,
  input_n_method = "n_stewart",
  verbose = FALSE
) {
  expanded_comps <- pacfintools::get_pacfin_expansions(
    Pdata = data,
    Catch = catch,
    weight_length_estimates = weight_length,
    Units = "MT",
    maxExp = 0.90,
    Exp_WA = TRUE,
    verbose = verbose
  )

  length_comps_long <- pacfintools::getComps(
    Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm)),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L",
    verbose = verbose
  )

  formatted_length_comps <- pacfintools::writeComps(
    inComps = length_comps_long,
    column_with_input_n = input_n_method,
    comp_bins = length_bins,
    verbose = FALSE
  ) |>
    dplyr::mutate(
      month = 7,
      partition = 0
    )

  length_trips_samples <- length_comps_long |>
    dplyr::ungroup() |>
    dplyr::summarise(
      .by = c(fishyr, fleet),
      trips = sum(unique(n_tows)),
      samples = sum(unique(n_fish)),
      input_n = sum(unique(n_stewart))
    ) |>
    dplyr::rename(year = fishyr)

  output <- list()
  output$formatted_length_comps <- formatted_length_comps
  output$length_trips_samples <- length_trips_samples

  return(output)
}
