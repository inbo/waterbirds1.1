#' Calculate intake model for given non-breeding bird species
#'
#' This function quantifies the allochthonous nitrogen and phosphorus input
#' by waterbirds based on mass-specific energy requirement and
#' daily food intake (intake model).
#' It takes into account the diet of the bird species (herbivore or carnivore)
#' and the season.
#' This function needs data on the average daily number of individuals present
#' per species per month to calculate how many nutrients are dropped each day
#' of that month,
#' so a modelled or derived average per month is expected and not just raw
#' observations which might have missing data or multiple data per month.
#'
#'
#' @param bird_occurrence data.frame giving the abundance of waterbirds per
#'   location (water body) in each month with at least columns
#'   - `species` (or `common_name`): the scientific (or common) name of the
#'   species,
#'   - `month`: the month as a number (e.g. 3 for March),
#'   - `year`: the year (if nutrient input should be given per year),
#'   - `n_individuals`: the average daily number of individuals present in this
#'   month,
#'   - `location`: the place or water body where the individuals of the species
#'   are present (if nutrient input should be given per location)
#' @param season_def definition of season, given as a data.frame with columns
#'   `season` and `month` (as number) showing which months are included in
#'   winter, spring and summer.
#'   Defaults to the season definition in article \emph{Hahn et al. (2008)}.
#' @inheritParams give_intake_herbivores
#' @inheritParams give_intake_carnivores
#'
#' @return
#'
#' @references
#' \itemize{
#' \item Hahn S., Bauwer S., Klaassen M. (2007). Estimating the contribution of
#' carnivorous waterbirds to nutrient loading in freshwater habitats.
#' Freshwater Biology 52: 2421-2433.
#' \doi{10.1111/j.1365-2427.2007.01838.x}
#' \item Hahn S., Bauwer S., Klaassen M. (2008). Quantification of allochthonous
#' nutrient input into freshwater bodies by herbivorous waterbirds.
#' Freshwater Biology 53: 181-193.
#' \doi{10.1111/j.1365-2427.2007.01881.x}
#' }
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr across bind_rows count filter group_by left_join mutate
#'   select summarise ungroup
#' @importFrom lubridate days_in_month
#'
#' @export
#'
#' @examples
#' library(waterbirds1.1)
#' dataset <- data.frame(
#'   species = c("Anas crecca", "Ardea cinerea"),
#'   month = 3,
#'   year = 2026,
#'   n_individuals = 1,
#'   location = "ZwartWater"
#' )
#' give_intake_model(bird_occurrence = dataset)

give_intake_model <- function(
  bird_occurrence,
  season_def = data.frame(
    season =
      c(rep("winter", 2), rep("spring", 2), rep("summer", 5), rep("winter", 3)),
    month = 1:12
  ),
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
  foraging_time = 12,
  prop_nutr_rel = c(internal = 1, external = 0.6)
) {
  assert_that(inherits(var_species, "data.frame"))
  assert_that(has_name(var_species, "species"))
  assert_that(inherits(var_species$species, "character"))
  assert_that(has_name(var_species, "common_name"))
  assert_that(inherits(var_species$common_name, "character"))

  assert_that(inherits(bird_occurrence, "data.frame"))
  if (
    !has_name(bird_occurrence, "species") &&
      has_name(bird_occurrence, "common_name")
  ) {
    bird_occurrence$species <-
      var_species[
        var_species$common_name == bird_occurrence$common_name, "species"
      ]
  }
  assert_that(has_name(bird_occurrence, "species"))
  assert_that(inherits(bird_occurrence$species, "character"))
  assert_that(all(bird_occurrence$species %in% var_species$species))
  assert_that(has_name(bird_occurrence, "month"))
  assert_that(is.numeric(bird_occurrence$month))
  assert_that(
    all(bird_occurrence$month == floor(bird_occurrence$month)),
    msg = "bird_occurence$month must be an integer"
  )
  assert_that(all(bird_occurrence$month > 0))
  assert_that(all(bird_occurrence$month <= 12))
  assert_that(has_name(bird_occurrence, "year"))
  assert_that(is.numeric(bird_occurrence$year))
  assert_that(
    all(bird_occurrence$year == floor(bird_occurrence$year)),
    msg = "bird_occurence$year must be an integer"
  )
  assert_that(has_name(bird_occurrence, "n_individuals"))
  assert_that(is.numeric(bird_occurrence$n_individuals))

  if (!has_name(bird_occurrence, "location")) {
    bird_occurrence$location <- "no location added"
  }
  assert_that(has_name(bird_occurrence, "location"))
  assert_that(is.character(bird_occurrence$location))

  doubles <- bird_occurrence |>
    count(.data$species, .data$month, .data$year, .data$location) |>
    filter(.data$n > 1)
  if (nrow(doubles) > 0) {
    stop("For some combinations of species, location and year, more than 1 record is given per month") #nolint: line_length_linter
  }

  assert_that(inherits(season_def, "data.frame"))
  assert_that(has_name(season_def, "season"))
  assert_that(all(season_def$season %in% c("winter", "spring", "summer")))
  assert_that(has_name(season_def, "month"))
  assert_that(is.numeric(season_def$month))
  assert_that(
    all(season_def$month == floor(season_def$month)),
    msg = "bird_occurence$month must be an integer"
  )
  assert_that(all(season_def$month > 0))
  assert_that(all(season_def$month <= 12))

  bird_occurrence$n_days <- days_in_month(
    as.Date(paste(bird_occurrence$year, bird_occurrence$month, "01", sep = "-"))
  )
  bird_occurrence <- bird_occurrence |>
    left_join(
      var_species |>
        select("species", "diet"),
      by = "species"
    ) |>
    left_join(season_def, by = "month")
  result <- bird_occurrence |>
    filter(.data$diet == "herbivore") |>
    mutate(
      give_intake_herbivores(
        species_name = .data$species,
        n_individuals = .data$n_individuals,
        n_days = .data$n_days,
        var_season = .data$season,
        type_food = type_food,
        var_species = var_species,
        terr_food_herbivores = terr_food_herbivores,
        var_food = var_food,
        foraging_time = foraging_time
      )
    ) |>
    bind_rows(
      bird_occurrence |>
        filter(.data$diet != "herbivore") |>
        mutate(
          give_intake_carnivores(
            species_name = .data$species,
            n_individuals = .data$n_individuals,
            n_days = .data$n_days,
            var_species = var_species,
            var_food = var_food,
            prop_nutr_rel = prop_nutr_rel
          )
        )
    ) |>
    group_by(.data$location, .data$species, .data$year) |>
    summarise(
      across(contains("_tot_"), sum), .groups = "keep"
    ) |>
    ungroup()
  if (all(result$location == "no location added")) {
    result$location <- NULL
  }

  return(result)
}
