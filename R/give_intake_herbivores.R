#' Calculate intake model for herbivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by herbivorous waterbirds based on mass-specific energy requirement and
#' daily food intake (intake model, \emph{Hahn et al., 2008}).
#'
#' @param species_abundance data.frame giving the number of individuals of
#'   waterbirds that is seen each day in the given number of days with at least
#'   columns
#'   - `species_name`: the scientific name of the herbivorous waterbird species
#'   (without author name),
#'   - `n_individuals` number of individuals of the species that are present
#'   on the water body,
#'   - `n_days` number of days that the (given number of) individuals of the
#'   species are present on the water body
#'   - `var_season` season(s) in which the given individuals of the
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
#'   `read.csv2(system.file("input_variables/var_species.csv", package =
#'   "waterbirds1.1"))`.
#' @param terr_food_herbivores table (data.frame) with the species and
#'   season-specific proportion of energy obtained from terrestrial food
#'   relative to the total amount of energy required ($f_t$), with at least
#'   columns `species` (scientific name), `season` ("spring", "summer" or
#'   "winter") and `f_t`.
#'   Defaults to the table provided in this package that can be accessed by
#'   `read.csv2(system.file("input_variables/terr_food_herbivores.csv",
#'   package = "waterbirds1.1"))`.
#' @param var_food table (data.frame) on food types with at least columns
#'   `food` (with values grass and beet),
#'   `enery` (energy content of the terrestrial diet in kJ/g),
#'   `AM` (apparent metabolizable energy coefficient) and
#'   `N25`, `N50`, `N75`, `P25`, `P50` and `P75` (low (25th percentile),
#'   average (mean) and high (75th percentile) level of nitrogen (N) or
#'   phosphorus (P) in the food type.
#'   Data are given as g N or P per g food dry weight.).
#'   Defaults to the table provided in this package that can be accessed by
#'   `read.csv2(system.file("input_variables/var_food.csv", package =
#'   "waterbirds1.1"))`.
#' @param foraging_time total foraging time in hours (T_f), defaults to 12 h
#'
#' @return data.frame with columns `species_name`, `n_individuals` and
#'   `var_season` as reference columns from the input,
#'   and columns `n_tot` and `p_tot` with the nitrogen and phosphorus input
#'   in kg by the birds in the given number of days.
#'   Each result is given with suffixes `_25`, `_50` and `_75`,
#'   corresponding to low (25th percentile), average (mean) and high
#'   (75th percentile) level of nutrients in the food type
#'   (see argument `var_food`).
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
#' give_intake_herbivores(
#'   species_abundance = dataset
#' )

give_intake_herbivores <- function(
  species_abundance,
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

  assert_that(inherits(type_food, "character"))
  assert_that(all(type_food %in% c("grass", "beet")))
  assert_that(
    length(type_food) == length(species_abundance$species_name) ||
      length(type_food) == 1
  )
  assert_that(all(type_food %in% var_food$food))
  if (nrow(species_abundance) > 0) {
    species_abundance$type_food <- type_food
  } else {
    species_abundance$type_food <- character(0)
  }

  assert_that(inherits(foraging_time, "numeric") |
                inherits(foraging_time, "numeric"))

  sa <- merge(
    species_abundance, var_species, by.x = "species_name", by.y = "species"
  )

  # body mass (g, M in article)
  sa$body_mass

  # daily energy requirement (kJ/day, DER in article)
  sa$der <- 10 ^ 1.0195 * sa$body_mass ^ 0.6808

  # energy content of the terrestrial diet (kJ/g, E in article)
  sa <- merge(sa, var_food, by.x = "type_food", by.y = "food")
  sa$energy

  # AM = apparent metabolizable energy coëfficiënt (AM in article and table)
  sa$am <- sa$AM

  # f_t = the species and season-specific proportion of energy obtained from
  # terrestrial food relative to the total amount of energy required
  sa <- merge(
    sa, terr_food_herbivores,
    by.x = c("species_name", "var_season"), by.y = c("species", "season")
  )
  sa$f_t

  # daily terrestrial food intake (g/day, DFI_t in article)
  sa$dft_t <- sa$f_t * sa$der / (sa$energy * sa$am)
  # units: g/day = kJ/day / (kJ/g)   #nolint: commented_code_linter

  # ratio of retention time = average time for food to pass a bird's digestive
  # track (h, RT in article)
  sa$rt <- 10 ^ (-0.3196) * sa$body_mass ^ 0.2020

  # T_f = total foraging time (h) = variable foraging_time

  # X_food = elemental concentration N and P in food (g/g)

  # X_ai = allochthonous nutrient input into a freshwater body (g/day)
  # formula: X_ai = RT / T_f * DFI_t * X_food  #nolint: commented_code_linter
  # units: g/day = h / h * g/day * g/g         #nolint: commented_code_linter

  sa$x_ai <- sa$rt / foraging_time * sa$dft_t # * X_food, which we do later
  # total nutrient input in kg
  sa$x_tot <- sa$x_ai * sa$n_individuals * sa$n_days * 10 ^ -3 # * X_food

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
      var_season = sa$var_season,
      n_tot_25 = sa$n_tot_25,
      n_tot_50 = sa$n_tot_50,
      n_tot_75 = sa$n_tot_75,
      p_tot_25 = sa$p_tot_25,
      p_tot_50 = sa$p_tot_50,
      p_tot_75 = sa$p_tot_75
    )
  )
}
