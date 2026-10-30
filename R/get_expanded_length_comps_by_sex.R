#' Expand fishery composition data by sex and format for SS3
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
get_expanded_length_comps_by_sex <- function(
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

  length_comps_long_f <- pacfintools::getComps(
    Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm), SEX_CODE == "F"),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L",
    verbose = verbose
  )
  length_comps_long_m <- pacfintools::getComps(
    Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm), SEX_CODE == "M"),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L",
    verbose = verbose
  )
  length_comps_long_u <- pacfintools::getComps(
    Pdata = expanded_comps |> dplyr::filter(!is.na(lengthcm), SEX_CODE == "U"),
    Comps = "LEN",
    weightid = "Final_Sample_Size_L",
    verbose = verbose
  )

  new_col_names <- c(
    "year",
    "month",
    "fleet",
    "sex",
    "partition",
    "input_n",
    length_bins
  )
  retained_lengths_by_sex <- dplyr::bind_rows(
    pacfintools::writeComps(
      inComps = length_comps_long_u |> dplyr::filter(SEX_CODE == "U"),
      column_with_input_n = input_n_method,
      comp_bins = length_bins,
      verbose = verbose
    ) |>
      dplyr::mutate(
        month = 7,
        sex = 0
      ) |>
      dplyr::rename_with(
        ~new_col_names
      ),
    pacfintools::writeComps(
      inComps = length_comps_long_f |> dplyr::filter(SEX_CODE == "F"),
      column_with_input_n = input_n_method,
      comp_bins = length_bins,
      verbose = verbose
    ) |>
      dplyr::mutate(
        month = 7,
        sex = 1
      ) |>
      dplyr::rename_with(
        ~new_col_names
      ),
    pacfintools::writeComps(
      inComps = length_comps_long_m |> dplyr::filter(SEX_CODE == "M"),
      column_with_input_n = input_n_method,
      comp_bins = length_bins,
      verbose = verbose
    ) |>
      dplyr::mutate(
        month = 7,
        sex = 2
      ) |>
      dplyr::rename_with(
        ~new_col_names
      )
  ) |>
    dplyr::mutate(partition = 0)

  new_col_names <- c(
    "year",
    "month",
    "fleet",
    "sex",
    "partition",
    "input_n",
    length_bins,
    paste0(length_bins, ".1")
  )

  formatted_length_comps <- dplyr::bind_cols(
    retained_lengths_by_sex,
    retained_lengths_by_sex[, 7:dim(retained_lengths_by_sex)[2]]
  ) |>
    dplyr::rename_with(
      ~new_col_names
    )

  return(formatted_length_comps)
}
