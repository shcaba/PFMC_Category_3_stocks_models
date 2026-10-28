library(tinytex)
library(asar)
library(here)

# Template
asar::create_template(
  format = "pdf",
  office = "NWFSC",
  region = "U.S. West Coast",
  species = "Category 3 stocks",
  year = 2023,
  spp_latin = "",
  #file_dir = here::here(),
  file_dir = getwd(),
  author = c("Jason M. Cope" = "NWFSC", "Chantel R. Wetzel" = "NWFSC"),
  include_affiliation = TRUE,
  simple_affiliation = FALSE
  #param_names = c("nf","sf"),
  #param_values = c("North fleet", "South fleet"),
  # model_results = petrale_output.rda # comment out this line when there is no standard output
)

#### Debugging ####

# Uncomment to read in output from converted in create_template and execute fxns below
# output <- utils::read.csv(here::here("report","Petrale_sole_std_res_2023.csv"))

# See set up of standard output file
# View(output)

# Uncomment if rdas were not created from create_template
# satf::exp_all_figs_tables(
#   output,
#   ref_line = "msy",
#   ref_line_sb = "msy"
# )

# Uncomment if biomass plot was not created
# satf::plot_biomass(
#   output,
#   ref_line = "msy", # change reference to fit data
#   make_rda = TRUE
# )

# Uncomment if spawning biomass plot was not created
# satf::plot_spawning_biomass(
#   output,
#   ref_line = "msy", # change reference to fit data
#   make_rda = TRUE
# )

# Uncomment if spawn recruitment plot was not created
# satf::plot_spawn_recruitment(
#   output,
#   end_year = 2022,
#   make_rda = TRUE
# )
