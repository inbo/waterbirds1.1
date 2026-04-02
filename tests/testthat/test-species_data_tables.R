library(dplyr)

var_species <- read.csv2(
  system.file("input_variables/var_species.csv", package = "waterbirds1.1")
)
var_species_herbivores <- var_species[var_species$diet == "herbivore", ]
var_species_carnivores <- var_species[var_species$diet != "herbivore", ]
terr_food_herbivores <- read.csv2(
  system.file(
    "input_variables/terr_food_herbivores.csv", package = "waterbirds1.1"
  )
)
breeding_carnivores <- read.csv2(
  system.file(
    "input_variables/breeding_carnivores.csv", package = "waterbirds1.1"
  )
)
var_food <- read.csv2(
  system.file("input_variables/var_food.csv", package = "waterbirds1.1")
)

test_that("test table var_species", {
  expect_equal(length(var_species$species), length(unique(var_species$species)))
  expect_equal(
    sum(var_species_herbivores$species %in% var_species_carnivores$species),
    0
  )
  expect_equal(
    sum(var_species_carnivores$species %in% var_species_herbivores$species),
    0
  )
  expect_true(
    all(var_species_herbivores$species %in% terr_food_herbivores$species)
  )
  expect_true(
    all(var_species_carnivores$species %in% breeding_carnivores$species)
  )
  expect_true(all(inherits(var_species$body_mass, "integer")))
  expect_true(all(var_species$body_mass > 0))
  expect_true(
    all(
      var_species_carnivores$diet %in% c("vertebrates", "mix (in)vertebrates")
    )
  )
  expect_true(all(var_species_carnivores$loader %in% c("internal", "external")))
})

test_that("test table terr_food_herbivores", {
  expect_true(
    all(terr_food_herbivores$species %in% var_species_herbivores$species)
  )
  expect_true(
    all(terr_food_herbivores$season %in% c("winter", "spring", "summer"))
  )
  expect_true(all(inherits(terr_food_herbivores$f_t, "numeric")))
  expect_true(all(terr_food_herbivores$f_t >= 0))
  expect_true(all(terr_food_herbivores$f_t <= 1))
  expect_equal(
    terr_food_herbivores |>
      reframe(
        spring = sum(.data$season == "spring"),
        summer = sum(.data$season == "summer"),
        winter = sum(.data$season == "winter"),
        .by = "species"
      ) |>
      filter(.data$spring != 1 | .data$summer != 1 | .data$winter != 1) |>
      nrow(),
    0
  )
})

test_that("test table breeding_carnivores", {
  expect_true(
    all(breeding_carnivores$species %in% var_species_carnivores$species)
  )
  expect_true(all(inherits(breeding_carnivores$body_mass, "integer")))
  expect_true(all(breeding_carnivores$body_mass > 0))
  expect_true(all(inherits(breeding_carnivores$egg_mass, "integer")))
  expect_true(all(breeding_carnivores$egg_mass > 0))
  expect_true(all(inherits(breeding_carnivores$clutch_size, "numeric")))
  expect_true(all(breeding_carnivores$clutch_size > 0))
  expect_true(all(inherits(breeding_carnivores$breeding_success, "numeric")))
  expect_true(all(breeding_carnivores$breeding_success > 0))
  expect_true(all(inherits(breeding_carnivores$nesting_period, "integer")))
  expect_true(all(breeding_carnivores$nesting_period > 0))
})
