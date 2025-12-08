#' Calculate intake model for non-breeding carnivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by carnivorous waterbirds based on nutrient concentrations of ingested food,
#' the birds’ daily energy requirements and the assumption that birds are in
#' steady-state with respect to the focal nutrient (intake model,
#' \emph{Hahn et al., 2007}).
#'
#' @param species_name (vector of) scientific name(s) of the carnivorous
#'   waterbird species (without author name)
#' @param prop_nutr_rel portion of total nutrient release (A in article) as a
#'   named vector giving 2 values:
#'   "internal" giving the value for internal loaders
#'     (= species that only forage in aquatic habitats) and
#'   "external" the average of estimations from species of external loaders.
#'   Defaults to 1 for internal loaders and 0.6 for external loaders (values
#'   from article \emph{Hahn et al., 2007}).
#' @inheritParams give_intake_herbivores
#'
#' @return data.frame with ...
#'
#' @references
#' \itemize{
#' \item Hahn S., Bauwer S., Klaassen M. (2007). Estimating the contribution of
#' carnivorous waterbirds to nutrient loading in freshwater habitats.
#' Freshwater Biology 52: 2421-2433.
#' \doi{10.1111/j.1365-2427.2007.01838.x}
#' }
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom utils read.csv2
#'
#' @export
#'
#' @examples
#' library(waterbirds1.1)
#' give_intake_carnivores(
#'   species_name = "Ardea cinerea",
#'   n_individuals = 1,
#'   n_days = 1
#' )

give_intake_carnivores <- function(
  species_name, n_individuals, n_days,
  var_species = read.csv2(
    system.file("input_variables/var_species.csv", package = "waterbirds1.1")
  ),
  var_food = read.csv2(
    system.file("input_variables/var_food.csv", package = "waterbirds1.1")
  ),
  prop_nutr_rel = c(internal = 1, external = 0.6)
) {

  assert_that(inherits(var_species, "data.frame"))
  assert_that(has_name(var_species, "species"))
  assert_that(inherits(var_species$species, "character"))
  assert_that(has_name(var_species, "body_mass"))
  assert_that(inherits(var_species$body_mass, "integer") |
                inherits(var_species$body_mass, "numeric"))
  assert_that(has_name(var_species, "diet"))
  assert_that(inherits(var_species$diet, "character"))
  var_species <- var_species[var_species$diet != "herbivore", ]

  assert_that(inherits(var_food, "data.frame"))
  assert_that(has_name(var_food, "food"))
  assert_that(inherits(var_food$food, "character"))
  assert_that(has_name(var_food, "energy"))
  assert_that(inherits(var_food$energy, "numeric"))
  assert_that(has_name(var_food, "AM"))
  assert_that(inherits(var_food$AM, "numeric"))
  assert_that(has_name(var_food, "N25"))
  assert_that(inherits(var_food$N25, "numeric"))
  assert_that(all(var_food$N25 > 0))
  assert_that(all(var_food$N25 < 1))
  assert_that(has_name(var_food, "N50"))
  assert_that(inherits(var_food$N50, "numeric"))
  assert_that(all(var_food$N50 > 0))
  assert_that(all(var_food$N50 < 1))
  assert_that(has_name(var_food, "N75"))
  assert_that(inherits(var_food$N75, "numeric"))
  assert_that(all(var_food$N75 > 0))
  assert_that(all(var_food$N75 < 1))
  assert_that(has_name(var_food, "P25"))
  assert_that(inherits(var_food$P25, "numeric"))
  assert_that(all(var_food$P25 > 0))
  assert_that(all(var_food$P25 < 1))
  assert_that(has_name(var_food, "P50"))
  assert_that(inherits(var_food$P50, "numeric"))
  assert_that(all(var_food$P50 > 0))
  assert_that(all(var_food$P50 < 1))
  assert_that(has_name(var_food, "P75"))
  assert_that(inherits(var_food$P75, "numeric"))
  assert_that(all(var_food$P75 > 0))
  assert_that(all(var_food$P75 < 1))

  assert_that(inherits(species_name, "character"))
  assert_that(all(species_name %in% var_species$species))

  assert_that(inherits(n_individuals, "numeric") |
                inherits(n_individuals, "integer"))
  assert_that(length(n_individuals) == length(species_name))

  assert_that(inherits(n_days, "numeric") | inherits(n_days, "integer"))
  assert_that(length(n_days) == length(species_name))

  assert_that(inherits(prop_nutr_rel, "numeric"))
  assert_that(length(prop_nutr_rel) == 2)
  assert_that(has_name(prop_nutr_rel, "internal"))
  assert_that(has_name(prop_nutr_rel, "external"))
  assert_that(all(prop_nutr_rel > 0))
  assert_that(all(prop_nutr_rel <= 1))


  # portion of total nutrient release (A in article)
  a <- prop_nutr_rel[var_species[var_species$species == species_name, "loader"]]
  a <- unname(a)

  # body mass (g, M in article)
  body_mass <- var_species[var_species$species == species_name, "body_mass"]

  # daily energy requirement (kJ/day, DER in article)
  der <- 10 ^ 1.0195 * body_mass ^ 0.6808

  type_food <- var_species[var_species$species == species_name, "diet"]

  # gross energy content of food (kJ/g, E in article)
  energy <- var_food[var_food$food == type_food, "energy"]

  # apparent metabolizable energy coëfficiënt (AM in article and table)
  # (=utilizable energy per unit food)
  am <- var_food[var_food$food == type_food, "AM"]

  # X_intake = nutrient composition of food (g/g), in table var_food

  # nutrient input of non-breeding birds (g/day)
  # X_nb-intake = A * DER / (E * AM) * X_intake  #nolint: commented_code_linter
  # units: g/day = kJ/day / (kJ/g) * g/g         #nolint: commented_code_linter
  x_nb_intake <- a * der / (energy * am) #* X_intake, which we add later
  # total nutrient input in kg
  x_tot <- x_nb_intake * n_individuals * n_days * 10 ^ -3 # * X_intake

  n_tot_25 <- x_tot * var_food[var_food$food == type_food, "N25"]
  n_tot_50 <- x_tot * var_food[var_food$food == type_food, "N50"]
  n_tot_75 <- x_tot * var_food[var_food$food == type_food, "N75"]
  p_tot_25 <- x_tot * var_food[var_food$food == type_food, "P25"]
  p_tot_50 <- x_tot * var_food[var_food$food == type_food, "P50"]
  p_tot_75 <- x_tot * var_food[var_food$food == type_food, "P75"]

  return(
    data.frame(
      species_name = species_name,
      n_individuals = n_individuals,
      n_tot_25 = n_tot_25,
      n_tot_50 = n_tot_50,
      n_tot_75 = n_tot_75,
      p_tot_25 = p_tot_25,
      p_tot_50 = p_tot_50,
      p_tot_75 = p_tot_75
    )
  )
}
