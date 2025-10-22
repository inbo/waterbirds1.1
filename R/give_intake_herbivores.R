#' Calculate intake model for herbivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by herbivorous waterbirds based on mass-specific energy requirement and
#' daily food intake (intake model).
#'
#' @param species_name (vector of) scientific name(s) of the herbivorous
#'   waterbird species (without author name)
#' @param n_individuals (vector of) number of individuals of the species that
#'   are present on the water body
#' @param n_days (vector of) number of days that the individuals of the species
#'   are present on the water body
#' @param var_season (vector of) season(s) in which the given individuals of the
#'   species are present on the water body.
#'   Possible values are `spring`, `summer` and `winter`.
#' @param type_food (vector of) type of food that is eaten by the species.
#'   Defaults to `grass`, the other option is `beet`.
#' @param var_species table (data.frame) with waterbird data with at least
#'   columns `species` (scientific name), `body_mass` (weight in g) and
#'   `diet` with possible values "herbivore",
#'   "vertebrates" (= carnivore with vertebrate diet, e.g. fish) and
#'   "mix (in)vertebrates" (= carnivores with a mixed diet of vertebrates and
#'   invertebrates, e.g. fish and insects).
#'   Defaults to the table provided in this package that can be accessed by
#'   `read.csv2(system.file("input_variables/var_species.csv", package = "waterbirds1.1"))`.
#' @param terr_food_herbivores table (data.frame) with the species and
#'   season-specific proportion of energy obtained from terrestrial food
#'   relative to the total amount of energy required ($f_t$), with at least
#'   columns `species` (scientific name), `season` ("spring", "summer" or
#'   "winter") and `f_t`.
#'   Defaults to the table provided in this package that can be accessed by
#'   `read.csv2(system.file("input_variables/terr_food_herbivores.csv", package = "waterbirds1.1"))`.
#' @param var_food table (data.frame) on food types with at least columns
#'   `food` (with values grass and beet),
#'   `enery` (energy content of the terrestrial diet in kJ/g),
#'   `AM` (apparent metabolizable energy coefficient) and
#'   `N25`, `N50`, `N75`, `P25`, `P50` and `P75` (low (25th percentile),
#'   average (mean) and high (75th percentile) level of nitrogen (N) or
#'   phosphorus (P) in the food type.
#'   Data are given as g N or P per g food dry weight.).
#'   Defaults to the table provided in this package that can be accessed by
#'   `read.csv2(system.file("input_variables/var_food.csv", package = "waterbirds1.1"))`.
#' @param foraging_time total foraging time in hours (T_f), defaults to 12 h
#'
#' @return data.frame with ...
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom utils read.csv2
#'
#' @export
#'
#' @examples
#' library(waterbirds1.1)
#' give_intake_herbivores(
#'   species_name = "Anas crecca",
#'   n_individuals = 1,
#'   n_days = 1,
#'   var_season = "spring"
#' )

give_intake_herbivores <- function(
  species_name, n_individuals, n_days, var_season,
  type_food = "grass",
  var_species = read.csv2(
    system.file("input_variables/var_species.csv", package = "waterbirds1.1")
  ),
  terr_food_herbivores = read.csv2(
    system.file(
      "input_variables/terr_food_herbivores.csv", package = "waterbirds1.1"
    )
  ),
  var_food = read.csv2(
    system.file("input_variables/var_food.csv", package = "waterbirds1.1")
  ),
  foraging_time = 12
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

  assert_that(inherits(type_food, "character"))
  assert_that(all(type_food %in% c("grass", "beet")))
  assert_that(
    length(type_food) == length(species_name) || length(type_food) == 1
  )
  assert_that(all(type_food %in% var_food$food))

  assert_that(inherits(foraging_time, "numeric") |
                inherits(foraging_time, "numeric"))

  # body mass (g, M in article)
  body_mass <- var_species[var_species$species == species_name, "body_mass"]

  # daily energy requirement (kJ/day, DER in article)
  der <- 10 ^ 1.0195 * body_mass ^ 0.6808

  # energy content of the terrestrial diet (kJ/g, E in article)
  energy <- var_food[var_food$food == type_food, "energy"]

  # AM = apparent metabolizable energy coëfficiënt (AM in article and table)
  am <- var_food[var_food$food == type_food, "AM"]

  # f_t = the species and season-specific proportion of energy obtained from
  # terrestrial food relative to the total amount of energy required
  f_t <- terr_food_herbivores[
    terr_food_herbivores$species == species_name &
      terr_food_herbivores$season == var_season,
    "f_t"
  ]

  # daily terrestrial food intake (g/day, DFI_t in article)
  dft_t <- f_t * der / (energy * am)
  # units: g/day = kJ/day / (kJ/g)   #nolint: commented_code_linter

  # ratio of retention time = average time for food to pass a bird's digestive
  # track (h, RT in article)
  rt <- 10 ^ (-0.3196) * body_mass ^ 0.2020

  # T_f = total foraging time (h) = variable foraging_time

  # X_food = elemental concentration N and P in food (g/g)

  # X_ai = allochthonous nutrient input into a freshwater body (g/day)
  # formula: X_ai = RT / T_f * DFI_t * X_food  #nolint: commented_code_linter
  # units: g/day = h / h * g/day * g/g         #nolint: commented_code_linter

  x_ai <- rt / foraging_time * dft_t # * X_food, which we do later
  # total nutrient imput in kg
  x_tot <- x_ai * n_individuals * n_days * 10 ^ -3 # * X_food

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
      var_season = var_season,
      n_tot_25 = n_tot_25,
      n_tot_50 = n_tot_50,
      n_tot_75 = n_tot_75,
      p_tot_25 = p_tot_25,
      p_tot_50 = p_tot_50,
      p_tot_75 = p_tot_75
    )
  )
}
