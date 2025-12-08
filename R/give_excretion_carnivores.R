#' Calculate excretion model for non-breeding carnivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by carnivorous waterbirds based on nutrient concentrations in the daily
#' excrement productions directly (excretion model, \emph{Hahn et al., 2008}).
#'
#' @param alpha ratio between food intake and excretion, defaults to 0.395
#'   (referenced in the article \emph{Hahn et al., 2007}, value based on 3
#'   species)
#' @param n_excr elemental concentration of nitrogen (N) in droppings in g/g
#'   (defaults to 0.103 derived from tool Waterbirds 1.1)
#' @param p_excr elemental concentration of phosphorus (P) in droppings in g/g
#'   (defaults to 0.04713 derived from tool Waterbirds 1.1)
#' @inheritParams give_intake_carnivores
#' @inheritParams give_intake_herbivores
#'
#' @return data.frame with columns `species_name` and `n_individuals` as
#'   reference columns from the input,
#'   and columns `n_tot` and `p_tot` with the nitrogen and phosphorus input
#'   in kg by the birds in the given number of days.
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
#' give_excretion_carnivores(
#'   species_name = "Ardea cinerea",
#'   n_individuals = 1,
#'   n_days = 1
#' )

give_excretion_carnivores <- function(
  species_name, n_individuals, n_days,
  var_species = read.csv2(
    system.file("input_variables/var_species.csv", package = "waterbirds1.1")
  ),
  var_food = read.csv2(
    system.file("input_variables/var_food.csv", package = "waterbirds1.1")
  ),
  prop_nutr_rel = c(internal = 1, external = 0.6),
  alpha = 0.395,
  n_excr = 0.103,
  p_excr = 0.04713
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

  assert_that(inherits(alpha, "numeric"))
  assert_that(length(alpha) == 1)
  assert_that(alpha > 0)
  assert_that(alpha < 1)

  assert_that(inherits(n_excr, "numeric"))
  assert_that(length(n_excr) == 1)
  assert_that(n_excr > 0)
  assert_that(n_excr < 1)

  assert_that(inherits(p_excr, "numeric"))
  assert_that(length(p_excr) == 1)
  assert_that(p_excr > 0)
  assert_that(p_excr < 1)


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

  # X_excr = nutrient concentration (g/g)
  # n_excr <- 0.103 and P_excr

  # nutrient input of non-breeding birds (g/day)
  # X_nb-excr = A * alpha * DER / (E * AM) * X_excr  #nolint: line_length_linter
  # units: g/day = g/g * kJ/day / (kJ/g) * g/g       #nolint: line_length_linter
  n_nb_excr <- a * alpha * der / (energy * am) * n_excr
  p_nb_excr <- a * alpha * der / (energy * am) * p_excr

  # total nutrient input in kg
  n_tot <- n_nb_excr * n_individuals * n_days * 10 ^ -3
  p_tot <- p_nb_excr * n_individuals * n_days * 10 ^ -3

  return(
    data.frame(
      species_name = species_name,
      n_individuals = n_individuals,
      n_tot = n_tot,
      p_tot = p_tot
    )
  )
}
