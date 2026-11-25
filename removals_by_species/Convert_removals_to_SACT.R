Removals.in <- read.csv(
  "C:/Users/Jason.Cope/Documents/Github/PFMC_Category_3_stocks_models/data-tables/commercial_catch.csv"
)

output_dir <- file.path(getwd(), "Input SACT files")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

removals_by_species <- split(Removals.in, Removals.in$species)

lapply(names(removals_by_species), function(species_name) {
  species_data <- removals_by_species[[species_name]]
  species_data$landings_mt[is.na(species_data$landings_mt)] <- 0
  species_data$discard_mt[is.na(species_data$discard_mt)] <- 0

  if (any(unique(species_data$gear) == "research")) {
    catches <- aggregate(
      cbind(landings_mt, discard_mt, catch_mt) ~ year + gear,
      data = species_data,
      FUN = sum
    )
    names(catches)[names(catches) == "landings_mt"] <- "Landings"
    names(catches)[names(catches) == "discard_mt"] <- "Discards"
    names(catches)[names(catches) == "catch_mt"] <- "Research"
    catches <- catches[order(catches$gear, catches$year), ]
  } else {
    catches <- aggregate(
      cbind(landings_mt, discard_mt) ~ year + gear,
      data = species_data,
      FUN = sum
    )
    names(catches)[names(catches) == "landings_mt"] <- "Landings"
    names(catches)[names(catches) == "discard_mt"] <- "Discards"
    catches <- catches[order(catches$gear, catches$year), ]
  }

  object_name <- paste0(
    "Catches_",
    gsub("[^a-z0-9]+", "_", tolower(trimws(species_name)))
  )
  assign(object_name, catches, envir = .GlobalEnv)

  file_name <- paste0(
    "Catches_",
    gsub("[^a-z0-9]+", "_", tolower(trimws(species_name))),
    ".csv"
  )
  write.csv(catches, file.path(output_dir, file_name), row.names = FALSE)
})

#sum(vapply(mget(ls(pattern = "^Catches_")), function(x) anyNA(x), logical(1)))
#ls(pattern = "^Catches_")
#head(Catches_stripetail_rockfish)
