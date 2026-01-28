#' Calculate intake model for breeding carnivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by carnivorous waterbirds based on nutrient concentrations of ingested food
#' and the energy requirements for breeding birds (intake model for breeding
#' birds, \emph{Hahn et al., 2007}).
#' The model assumes that one adult was continuously present at the nest
#' during the breeding period and it also takes into account the total nutrient
#' deposition of chicks both reaching fledging and perishing during the growth
#' period (for the average clutch size of the nest).
#'
#' @param species_name (vector of) scientific name(s) of the carnivorous
#'   waterbird species (without author name)
#' @param n_nests number of nests or number of breeding pairs
#' @param breeding_carnivores table (data.frame) with waterbird data on
#'   breeding carnivores with at least columns `species` (scientific name),
#'   `egg_mass` (egg weight in g),
#'   `clutch_size` (species-specific mean clutch size) and
#'   `nesting_period` (duration of the nesting period in days).
#'   Defaults to the table provided in this package that can be accessed by
#'   `read.csv2(system.file("input_variables/breeding_carnivores.csv", package = "waterbirds1.1"))`.
#' @param beta correction factor for chicks perishing during growth period,
#'   defaults to 0.5 (referenced in the article \emph{Hahn et al., 2007})
#' @param n_perc_body average nitrogen content of a complete bird body,
#'   defaults to 2.82 % or 0.0282 fresh body mass (referenced in the article
#'   \emph{Hahn et al., 2007})
#' @param p_perc_body average phosphorus content of a complete bird body,
#'   defaults to 0.46 % or 0.0046 fresh body mass (referenced in the article
#'   \emph{Hahn et al., 2007})
#' @inheritParams give_intake_herbivores
#'
#' @return data.frame with columns `species_name` and `n_nests` as
#'   reference columns from the input,
#'   and columns `n_tot` and `p_tot` with the nitrogen and phosphorus input
#'   in kg by a breeding bird and its chicks in the breeding season.
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
#'
#' @examples
#' library(waterbirds1.1)
#' give_intake_breeding_carni(
#'   species_name = "Ardea cinerea",
#'   n_nests = 1
#' )

give_intake_breeding_carni <- function(
  species_name, n_nests,
  var_species = read.csv2(
    system.file("input_variables/var_species.csv", package = "waterbirds1.1")
  ),
  var_food = read.csv2(
    system.file("input_variables/var_food.csv", package = "waterbirds1.1")
  ),
  breeding_carnivores = read.csv2(
    system.file(
      "input_variables/breeding_carnivores.csv", package = "waterbirds1.1"
    )
  ),
  beta = 0.5,
  n_perc_body = 0.0282,
  p_perc_body = 0.0046
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

  assert_that(inherits(breeding_carnivores, "data.frame"))
  assert_that(has_name(breeding_carnivores, "species"))
  assert_that(inherits(breeding_carnivores$species, "character"))
  assert_that(all(breeding_carnivores$species %in% var_species$species))
  assert_that(has_name(breeding_carnivores, "egg_mass"))
  assert_that(inherits(breeding_carnivores$egg_mass, "integer"))
  assert_that(
    all(is.na(breeding_carnivores$egg_mass) | breeding_carnivores$egg_mass > 0)
  )
  assert_that(has_name(breeding_carnivores, "clutch_size"))
  assert_that(inherits(breeding_carnivores$clutch_size, "numeric"))
  assert_that(
    all(
      is.na(breeding_carnivores$clutch_size) |
        breeding_carnivores$clutch_size > 0
    )
  )
  assert_that(has_name(breeding_carnivores, "nesting_period"))
  assert_that(inherits(breeding_carnivores$nesting_period, "integer"))
  assert_that(
    all(
      is.na(breeding_carnivores$nesting_period) |
        breeding_carnivores$nesting_period > 0
    )
  )

  assert_that(inherits(species_name, "character"))
  assert_that(all(species_name %in% var_species$species))

  assert_that(inherits(n_nests, "numeric") | inherits(n_nests, "integer"))
  assert_that(length(n_nests) == length(species_name))

  assert_that(inherits(beta, "numeric"))
  assert_that(beta > 0)
  assert_that(beta <= 1)

  assert_that(inherits(n_perc_body, "numeric"))
  assert_that(n_perc_body > 0)
  assert_that(n_perc_body <= 1)
  assert_that(inherits(p_perc_body, "numeric"))
  assert_that(p_perc_body > 0)
  assert_that(p_perc_body <= 1)


  # ADULT BIRD ON NEST
  # portion of total nutrient release (A in article), 1 for breeding bird
  a <- 1

  # n_days = duration of the nesting period
  n_days <- breeding_carnivores[
    breeding_carnivores$species == species_name, "nesting_period"
  ]

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
  x_adult <- x_nb_intake * n_nests * n_days * 10 ^ -3 # * X_intake


  # CHICKS
  # X_offspring = amounts excreted by offspring
  # = total nutrient deposition of chicks both reaching fledging and perishing
  # during the growth period

  # TER_chick = total energy requirement over entire chick rearing period (kJ)
  ter_chick <- 28.43 * body_mass ^ 1.062
  # body_mass = body mass of adult birds in g
  # E and AM equal to 'adult' values
  # X_offspring

  # CS = species-specific mean clutch size (table 2)
  cs <- breeding_carnivores[
    breeding_carnivores$species == species_name, "clutch_size"
  ]

  # correction factor beta
  beta

  egg_mass  <- breeding_carnivores[
    breeding_carnivores$species == species_name, "egg_mass"
  ]

  # X_syn = total amount of N and P fixed in a chick's body (in %)
  n_syn <- n_perc_body * (body_mass - 0.72 * egg_mass)
  p_syn <- p_perc_body * (body_mass - 0.72 * egg_mass)

  # X_offspring_intake = beta * CS * (TER_chick / (AM * E) * X_intake - X_syn) #nolint
  # x_offspr = beta * CS * TER_chick / (AM * E) * x_intake - beta * CS * x_syn #nolint
  # units: g = kJ / (kJ / g) * g / g            #nolint: commented_code_linter
  x_offspring <- beta * cs * ter_chick / (am * energy) * 10 ^ -3
  # * x_intake - beta * CS * x_syn * 10 ^ -3


  # TOTAL NUTRIENT RELEASE FOR ADULT AND CHICK
  x_tot <- x_adult + x_offspring # * x_intake - beta * CS * x_syn

  n_tot_25 <- x_tot * var_food[var_food$food == type_food, "N25"] -
    beta * cs * n_syn * 10 ^ -3
  n_tot_50 <- x_tot * var_food[var_food$food == type_food, "N50"] -
    beta * cs * n_syn * 10 ^ -3
  n_tot_75 <- x_tot * var_food[var_food$food == type_food, "N75"] -
    beta * cs * n_syn * 10 ^ -3
  p_tot_25 <- x_tot * var_food[var_food$food == type_food, "P25"] -
    beta * cs * p_syn * 10 ^ -3
  p_tot_50 <- x_tot * var_food[var_food$food == type_food, "P50"] -
    beta * cs * p_syn * 10 ^ -3
  p_tot_75 <- x_tot * var_food[var_food$food == type_food, "P75"] -
    beta * cs * p_syn * 10 ^ -3

  return(
    data.frame(
      species_name = species_name,
      n_nests = n_nests,
      n_tot_25 = n_tot_25,
      n_tot_50 = n_tot_50,
      n_tot_75 = n_tot_75,
      p_tot_25 = p_tot_25,
      p_tot_50 = p_tot_50,
      p_tot_75 = p_tot_75
    )
  )
}
