n <- 11

length_bins <- matrix(
  data = c(
    "bank rockfish",
    12,
    50,
    2,
    "flathead sole",
    10,
    44,
    2,
    "pacific cod",
    NA,
    NA,
    2,
    "pacific sanddab",
    8,
    40,
    2,
    "redstripe rockfish",
    NA,
    NA,
    2,
    "rosethorn rockfish",
    10,
    40,
    2,
    "shortraker rockfish",
    NA,
    NA,
    2,
    "silvergray rockfish",
    30,
    64,
    2,
    "starry rockfish",
    NA,
    NA,
    2,
    "stripetail rockfish",
    6,
    36,
    2,
    "yellowmouth rockfish",
    24,
    54,
    2
  ),
  nrow = n,
  ncol = 4,
  byrow = TRUE
)

colnames(length_bins) <- c("species", "low", "high", "bin")
length_bins_all_species <- length_bins |>
  tibble::as_tibble() |>
  dplyr::mutate(
    low = as.numeric(low),
    high = as.numeric(high),
    bin = as.numeric(bin)
  )

usethis::use_data(
  length_bins_all_species,
  overwrite = TRUE
)
