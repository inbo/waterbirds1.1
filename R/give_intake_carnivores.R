#' Calculate intake model for non-breeding carnivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by carnivorous waterbirds based on nutrient concentrations of ingested food,
#' the birds’ daily energy requirements and the assumption that birds are in
#' steady-state with respect to the focal nutrient (intake model,
#' \emph{Hahn et al., 2007}).
#'
#' @param species_abundance data.frame giving the number of individuals of
#'   waterbirds that is seen each day in the given number of days with at least
#'   columns
#'   - `species_name`: the scientific name of the carnivorous waterbird species
#'   (without author name),
#'   - `n_individuals` number of individuals of the species that are present
#'   on the water body,
#'   - `n_days` number of days that the (given number of) individuals of the
#'   species are present on the water body.
#' @param prop_nutr_rel portion of total nutrient release (A in article) as a
#'   named vector giving 2 values:
#'   "internal" giving the value for internal loaders
#'     (= species that only forage in aquatic habitats) and
#'   "external" the average of estimations from species of external loaders.
#'   Defaults to 1 for internal loaders and 0.6 for external loaders (values
#'   from article \emph{Hahn et al., 2007}).
#' @inheritParams give_intake_herbivores
#'
#' @return data.frame with columns `species_name` and `n_individuals` as
#'   reference columns from the input,
#'   and columns `n_tot` and `p_tot` with the nitrogen and phosphorus input
#'   in kg by the birds in the given number of days.
#'   Each result is given with suffixes `_25`, `_50` and `_75`,
#'   corresponding to low (25th percentile), average (mean) and high
#'   (75th percentile) level of nutrients in the food type
#'   (see argument `var_food`).
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
#' @family carnivores
#'
#' @examples
#' library(waterbirds1.1)
#' dataset <- data.frame(
#'   species_name = c("Ardea cinerea", "Phalacrocorax carbo"),
#'   n_individuals = c(1, 1),
#'   n_days = c(1, 1)
#' )
#'
#' give_intake_carnivores(
#'   species_abundance = dataset
#' )

give_intake_carnivores <- function(
  species_abundance,
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

  assert_that(inherits(species_abundance, "data.frame"))
  assert_that(has_name(species_abundance, "species_name"))
  assert_that(inherits(species_abundance$species_name, "character"))
  assert_that(all(species_abundance$species_name %in% var_species$species))

  assert_that(has_name(species_abundance, "n_individuals"))
  assert_that(inherits(species_abundance$n_individuals, "numeric") |
                inherits(species_abundance$n_individuals, "integer"))

  assert_that(has_name(species_abundance, "n_days"))
  assert_that(inherits(species_abundance$n_days, "numeric") |
                inherits(species_abundance$n_days, "integer"))

  assert_that(inherits(prop_nutr_rel, "numeric"))
  assert_that(length(prop_nutr_rel) == 2)
  assert_that(has_name(prop_nutr_rel, "internal"))
  assert_that(has_name(prop_nutr_rel, "external"))
  assert_that(all(prop_nutr_rel > 0))
  assert_that(all(prop_nutr_rel <= 1))


  # portion of total nutrient release (A in article)
  sa <- merge(
    species_abundance, var_species, by.x = "species_name", by.y = "species"
  )
  sa$a <- prop_nutr_rel[sa$loader]

  # body mass (g, M in article)
  sa$body_mass

  # daily energy requirement (kJ/day, DER in article)
  sa$der <- 10 ^ 1.0195 * sa$body_mass ^ 0.6808

  sa <- merge(sa, var_food, by.x = "diet", by.y = "food")

  # gross energy content of food (kJ/g, E in article)
  sa$energy

  # apparent metabolizable energy coëfficiënt (AM in article and table)
  # (=utilizable energy per unit food)
  sa$am <- sa$AM

  # X_intake = nutrient composition of food (g/g), in table var_food

  # nutrient input of non-breeding birds (g/day)
  # X_nb-intake = A * DER / (E * AM) * X_intake  #nolint: commented_code_linter
  # units: g/day = kJ/day / (kJ/g) * g/g         #nolint: commented_code_linter
  sa$x_nb_intake <- sa$a * sa$der / (sa$energy * sa$am) # * X_intake (add later)
  # total nutrient input in kg
  sa$x_tot <-
    sa$x_nb_intake * sa$n_individuals * sa$n_days * 10 ^ -3 # * X_intake

  sa$n_tot_25 <- sa$x_tot * sa$N25
  sa$n_tot_50 <- sa$x_tot * sa$N50
  sa$n_tot_75 <- sa$x_tot * sa$N75
  sa$p_tot_25 <- sa$x_tot * sa$P25
  sa$p_tot_50 <- sa$x_tot * sa$P50
  sa$p_tot_75 <- sa$x_tot * sa$P75

  return(
    data.frame(
      species_name = sa$species_name,
      n_individuals = sa$n_individuals,
      n_tot_25 = sa$n_tot_25,
      n_tot_50 = sa$n_tot_50,
      n_tot_75 = sa$n_tot_75,
      p_tot_25 = sa$p_tot_25,
      p_tot_50 = sa$p_tot_50,
      p_tot_75 = sa$p_tot_75
    )
  )
}
