#' Format single sex comps for a two-sex model
#'
#'
#' @param data Data frame of formatted SS3 single-sex length composition data.
#'
#' @author Chantel Wetzel
#' @export
#'
format_two_sex_comps <- function(
  data
) {
  comps_wide <- cbind(
    data,
    data[, 7:ncol(data)]
  )
  colnames(comps_wide) <- colnames(comps_wide)
  return(comps_wide)
}
