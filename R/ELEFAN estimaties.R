library(TropFishR)
stripetail.lts <- read.csv(
  "C:\\Users\\Jason.Cope\\Documents\\Github\\SS-DL-tool\\Scenarios\\StripetailRF_Ctfull_recs_Fr_FBLH\\Scenario_Inputs\\Lengths_stripetail.csv"
)

#Filter fleet and sex
stripetail.lts <- subset(
  stripetail.lts,
  Fleet == 1 & Sex == 1
)

stripetail.lts <- read.csv(
  "C:\\Users\\Jason.Cope\\Documents\\Github\\PFMC_Category_3_stocks_models\\R\\Stripetail_lengths.csv"
)

#Filter fleet and sex
stripetail.lts <- subset(
  stripetail.lts,
  Fleet == 1 & Sex == 0
)


stripetail.lts.dates <- as.Date(
  paste0("1/", stripetail.lts$Month, "/", stripetail.lts$Year),
  format = "%d/%m/%Y"
)
stripetail.lts.bins <- seq(9, 35, 2)
stripetail.lts.frq <- as.matrix(stripetail.lts[, -(1:7)])
colnames(stripetail.lts.frq) <- stripetail.lts.bins


stripetail.lfq <- list(
  #sample.no= 1:length(stripetail.lts.dates),
  midLengths = stripetail.lts.bins,
  dates = stripetail.lts.dates,
  catch = t(stripetail.lts.frq)
)
class(stripetail.lfq) <- "lfq"
plot(stripetail.lfq, Fname = "catch")

output <- ELEFAN_SA(
  stripetail.lfq,
  SA_time = 60 * 0.5,
  seasonalised = TRUE,
  MA = 11,
  init_par = list(Linf = 30, K = 0.05, t_anchor = -2, C = 0.5, ts = 0.5),
  low_par = list(Linf = 20, K = 0.1, t_anchor = -4, C = 0, ts = 0),
  up_par = list(Linf = 40, K = 0.3, t_anchor = 0, C = 1, ts = 1)
)
