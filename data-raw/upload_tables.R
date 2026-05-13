# This script updates the internal tables with species data that are used
# for the calculations

library(googlesheets4)
library(dplyr)

ss_id <- "1S0DlNVOViHMMiRZWN0QdZYMZ718I-iGVRoHIXxKpHbQ"

# var_species
var_species <- read_sheet(ss = ss_id, sheet = "var_species", na = c("", "NA"))
write.csv2(
  var_species, file = "inst/input_variables/var_species.csv", row.names = FALSE
)


# terr_food_herbivores
terr_food_herbivores <-
  read_sheet(ss = ss_id, sheet = "terr_food_herbivores", na = c("", "NA")) |>
  select("species", "season", "f_t")
write.csv2(
  terr_food_herbivores,
  file = "inst/input_variables/terr_food_herbivores.csv",
  row.names = FALSE
)


# breeding_carnivores
breeding_carnivores <- read_sheet(
  ss = ss_id, sheet = "breeding_carnivores",
  col_types = "cciicci",
  na = c("", "NA")
)
breeding_carnivores$clutch_size <-
  as.numeric(gsub(",", ".", breeding_carnivores$clutch_size))
breeding_carnivores$breeding_success <-
  as.numeric(gsub(",", ".", breeding_carnivores$breeding_success))
write.csv2(
  breeding_carnivores,
  file = "inst/input_variables/breeding_carnivores.csv",
  row.names = FALSE
)
