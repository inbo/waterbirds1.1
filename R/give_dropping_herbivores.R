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
#'
#' @examples
#' library(waterbirds1.1)
#' give_dropping_herbivores(
#'   species_name = "Anas crecca",
#'   n_individuals = 1,
#'   n_days = 1,
#'   var_season = "spring"
#' )

give_dropping_herbivores <- function(
  species_name, n_individuals, n_days, var_season,
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

  assert_that(inherits(species_name, "character"))
  assert_that(all(species_name %in% var_species$species))
  assert_that(all(species_name %in% terr_food_herbivores$species))

  assert_that(inherits(n_individuals, "numeric") |
                inherits(n_individuals, "integer"))
  assert_that(length(n_individuals) == length(species_name))

  assert_that(inherits(n_days, "numeric") | inherits(n_days, "integer"))
  assert_that(length(n_days) == length(species_name))

  assert_that(inherits(var_season, "character"))
  assert_that(all(var_season %in% c("spring", "summer", "winter")))
  assert_that(
    length(var_season) == length(species_name) || length(var_season) == 1
  )

  # body mass (g, M in article)
  body_mass <- var_species[var_species$species == species_name, "body_mass"]

  # food RT (h) = average time for food to pass a bird's digestive
  rt <- 10 ^ (-0.3196) * body_mass ^ 0.2020

  # dropping mass (g, DrM in article)
  drm <- 10 ^ (-3.065) * body_mass ^ 0.8901

  # DrR = dropping rate (numbers of droppings / h, DrR in article)
  drr <-  10 ^ 2.130 * body_mass ^ (-0.3065)

  # X_drop = elemental concentration in droppings (mg/g)
  # n_drop and p_drop

  # f_t = proportion of droppings originating from terrestrial food
  f_t <- terr_food_herbivores[
    terr_food_herbivores$species == species_name &
      terr_food_herbivores$season == var_season,
    "f_t"
  ]

  # allochthonous nutrient input into a freshwater body (g/day, X_ai in article)
  # formula: X_ai = f_t * RT * DrM * DrR * X_drop #nolint: commented_code_linter
  # units: g/day = h * g * 1/h * mg/g * 10 ^ -3   #nolint: commented_code_linter

  n_ai <- f_t * rt * drm * drr * n_drop * 10 ^ -3
  p_ai <- f_t * rt * drm * drr * p_drop * 10 ^ -3

  # total nutrient input in kg
  n_tot <- n_ai * n_individuals * n_days * 10 ^ -3
  p_tot <- p_ai * n_individuals * n_days * 10 ^ -3

  return(
    data.frame(
      species_name = species_name,
      n_individuals = n_individuals,
      var_season = var_season,
      n_tot = n_tot,
      p_tot = p_tot
    )
  )
}
