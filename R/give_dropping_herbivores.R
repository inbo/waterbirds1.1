#' Calculate dropping model for herbivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by herbivorous waterbirds based on daily faecal output and digestive
#' performance (dropping model, \emph{Hahn et al., 2008}).
#'
#' @param n_drop elemental concentration of nitrogen (N) in droppings in mg/g
#'   (defaults to 45.02 taken from article of \emph{Hahn et al., 2008})
#' @param p_drop elemental concentration of phosphorus (P) in droppings in mg/g
#'   (defaults to 6.18 taken from article of \emph{Hahn et al., 2008})
#' @inheritParams give_intake_herbivores
#'
#' @return data.frame with columns `species_name`, `n_individuals` and
#'   `var_season` as reference columns from the input,
#'   and columns `n_tot` and `p_tot` with the nitrogen and phosphorus input
#'   in kg by the birds in the given number of days.
#'
#' @references
#' \itemize{
#' \item Hahn S., Bauwer S., Klaassen M. (2008). Quantification of allochthonous
#' nutrient input into freshwater bodies by herbivorous waterbirds.
#' Freshwater Biology 53: 181-193.
#' \doi{10.1111/j.1365-2427.2007.01881.x}
#' }
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom utils read.csv2
#'
#' @export
#' @family herbivores
#'
#' @examples
#' library(waterbirds1.1)
#' dataset <- data.frame(
#'   species_name = c("Anas crecca", "Anas platyrhynchos"),
#'   n_individuals = c(1, 1),
#'   n_days = c(1, 1),
#'   var_season = c("spring", "winter")
#' )
#'
#' give_dropping_herbivores(
#'   species_abundance = dataset
#' )

give_dropping_herbivores <- function(
  species_abundance,
  var_species = read.csv2(
    system.file("input_variables/var_species.csv", package = "waterbirds1.1")
  ),
  terr_food_herbivores = read.csv2(
    system.file(
      "input_variables/terr_food_herbivores.csv", package = "waterbirds1.1"
    )
  ),
  n_drop = 45.02,
  p_drop = 6.18
) {

  assert_that(inherits(var_species, "data.frame"))
  assert_that(has_name(var_species, "species"))
  assert_that(inherits(var_species$species, "character"))
  assert_that(has_name(var_species, "body_mass"))
  assert_that(inherits(var_species$body_mass, "integer") |
                inherits(var_species$body_mass, "numeric"))
  assert_that(has_name(var_species, "diet"))
  assert_that(inherits(var_species$diet, "character"))
  var_species <- var_species[var_species$diet == "herbivore", ]

  assert_that(inherits(terr_food_herbivores, "data.frame"))
  assert_that(has_name(terr_food_herbivores, "species"))
  assert_that(inherits(terr_food_herbivores$species, "character"))
  assert_that(has_name(terr_food_herbivores, "season"))
  assert_that(inherits(terr_food_herbivores$season, "character"))
  assert_that(
    all(terr_food_herbivores$season %in% c("spring", "summer", "winter"))
  )
  assert_that(has_name(terr_food_herbivores, "f_t"))
  assert_that(inherits(terr_food_herbivores$f_t, "numeric"))
  assert_that(all(terr_food_herbivores$f_t >= 0))
  assert_that(all(terr_food_herbivores$f_t <= 1))

  assert_that(inherits(species_abundance, "data.frame"))
  assert_that(has_name(species_abundance, "species_name"))
  assert_that(inherits(species_abundance$species_name, "character"))
  assert_that(all(species_abundance$species_name %in% var_species$species))
  assert_that(
    all(species_abundance$species_name %in% terr_food_herbivores$species)
  )

  assert_that(has_name(species_abundance, "n_individuals"))
  assert_that(inherits(species_abundance$n_individuals, "numeric") |
                inherits(species_abundance$n_individuals, "integer"))

  assert_that(has_name(species_abundance, "n_days"))
  assert_that(inherits(species_abundance$n_days, "numeric") |
                inherits(species_abundance$n_days, "integer"))

  assert_that(has_name(species_abundance, "var_season"))
  assert_that(inherits(species_abundance$var_season, "character"))
  assert_that(
    all(species_abundance$var_season %in% c("spring", "summer", "winter"))
  )


  sa <- merge(
    species_abundance, var_species, by.x = "species_name", by.y = "species"
  )

  # body mass (g, M in article)
  sa$body_mass

  # food RT (h) = average time for food to pass a bird's digestive
  sa$rt <- 10 ^ (-0.3196) * sa$body_mass ^ 0.2020

  # dropping mass (g, DrM in article)
  sa$drm <- 10 ^ (-3.065) * sa$body_mass ^ 0.8901

  # DrR = dropping rate (numbers of droppings / h, DrR in article)
  sa$drr <-  10 ^ 2.130 * sa$body_mass ^ (-0.3065)

  # X_drop = elemental concentration in droppings (mg/g)
  # n_drop and p_drop

  # f_t = proportion of droppings originating from terrestrial food
  sa <- merge(
    sa, terr_food_herbivores,
    by.x = c("species_name", "var_season"), by.y = c("species", "season")
  )

  # allochthonous nutrient input into a freshwater body (g/day, X_ai in article)
  # formula: X_ai = f_t * RT * DrM * DrR * X_drop #nolint: commented_code_linter
  # units: g/day = h * g * 1/h * mg/g * 10 ^ -3   #nolint: commented_code_linter

  sa$n_ai <- sa$f_t * sa$rt * sa$drm * sa$drr * n_drop * 10 ^ -3
  sa$p_ai <- sa$f_t * sa$rt * sa$drm * sa$drr * p_drop * 10 ^ -3

  # total nutrient input in kg
  sa$n_tot <- sa$n_ai * sa$n_individuals * sa$n_days * 10 ^ -3
  sa$p_tot <- sa$p_ai * sa$n_individuals * sa$n_days * 10 ^ -3

  return(
    data.frame(
      species_name = sa$species_name,
      n_individuals = sa$n_individuals,
      var_season = sa$var_season,
      n_tot = sa$n_tot,
      p_tot = sa$p_tot
    )
  )
}
