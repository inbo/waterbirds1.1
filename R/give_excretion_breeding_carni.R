#' Calculate excretion model for breeding carnivores
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by carnivorous waterbirds based on nutrient concentrations in the daily
#' excrement productions directly (excretion model, \emph{Hahn et al., 2007}).
#' The model assumes that one adult was continuously present at the nest
#' during the breeding period and it also takes into account the total nutrient
#' deposition of chicks both reaching fledging and perishing during the growth
#' period (for the average clutch size of the nest).
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
#' @inheritParams give_intake_breeding_carni
#'
#' @return data.frame with columns `species_name` and `n_nests` as
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
#' @family carnivores
#'
#' @examples
#' library(waterbirds1.1)
#' give_excretion_breeding_carni(
#'   species_name = c("Ardea cinerea", "Phalacrocorax carbo"),
#'   n_nests = c(1, 1)
#' )

give_excretion_breeding_carni <- function(
  species_name, n_nests,
  var_species = read.csv2(
    system.file("input_variables/var_species.csv", package = "waterbirds1.1")
  ),
  var_food = read.csv2(
    system.file("input_variables/var_food.csv", package = "waterbirds1.1")
  ),
  alpha = 0.395,
  n_excr = 0.103,
  p_excr = 0.04713,
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

  # X_excr = nutrient concentration (g/g)
  # n_excr and P_excr

  # nutrient input of non-breeding birds (g/day)
  # X_nb-excr = A * alpha * DER / (E * AM) * X_excr  #nolint: line_length_linter
  # units: g/day = g/g * kJ/day / (kJ/g) * g/g       #nolint: line_length_linter
  n_nb_excr <- a * alpha * der / (energy * am) * n_excr
  p_nb_excr <- a * alpha * der / (energy * am) * p_excr

  # total nutrient input in kg
  n_adult <- n_nb_excr * n_nests * n_days * 10 ^ -3
  p_adult <- p_nb_excr * n_nests * n_days * 10 ^ -3


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

  # X_offspr_excr = beta * CS * (alpha * TER_chick / (AM * E) * X_excr - X_syn)  #nolint
  # units: g = kJ / (kJ / g) * g / g     #nolint: commented_code_linter
  n_offspring <-
    beta * cs * (alpha * ter_chick / (am * energy) * n_excr - n_syn) * 10 ^ -3
  p_offspring <-
    beta * cs * (alpha * ter_chick / (am * energy) * p_excr - p_syn) * 10 ^ -3


  # TOTAL NUTRIENT RELEASE FOR ADULT AND CHICK
  n_tot <- n_adult + n_offspring
  p_tot <- p_adult + p_offspring


  return(
    data.frame(
      species_name = species_name,
      n_nests = n_nests,
      n_tot = n_tot,
      p_tot = p_tot
    )
  )
}
