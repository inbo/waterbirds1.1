library(dplyr)
library(tidyr)

abundancies_1location <- data.frame(
  year = c(rep(2020, 5), rep(2021, 5), rep(2022, 5)),
  month = rep(1:5, 3),
  species = "Anas acuta",
  location = "Lake",
  n_individuals = 10:24
)

abundancies_2locations <- abundancies_1location |>
  bind_rows(
    data.frame(
      year = c(rep(2020, 5), rep(2021, 5), rep(2022, 5)),
      month = rep(1:5, 3),
      species = "Anas acuta",
      location = "Lake2",
      n_individuals = 10:24
    )
  )


test_that("results are consistent", {
  expect_equal(
    give_intake_model(abundancies_1location),
    give_intake_model(abundancies_2locations) |>
      filter(location == "Lake")
  )
  expect_equal(
    give_dropping_model(abundancies_1location),
    give_dropping_model(abundancies_2locations) |>
      filter(location == "Lake")
  )
})

test_that("give_dropping_model() aggregates correct", {
  expect_equal(
    give_dropping_model(abundancies_1location),
    abundancies_1location |>
      as_tibble() |>
      right_join(
        give_dropping_herbivores(
          abundancies_1location |>
            mutate(
              n_days = lubridate::days_in_month(
                as.Date(paste(year, month, "01", sep = "-"))
              ),
              season =
                rep(c("winter", "winter", "spring", "spring", "summer"), 3)
            ) |>
            transmute(
              species_name = species,
              n_individuals,
              n_days,
              var_season = season
            )
        ),
        by = c("species" = "species_name", "n_individuals")
      ) |>
      reframe(
        n_tot = sum(n_tot),
        p_tot = sum(p_tot),
        .by = c("location", "species", "year")
      )
  )
  expect_equal(
    give_dropping_model(
      abundancies_1location |>
        mutate(species = "Chroicocephalus ridibundus")
    ),
    abundancies_1location |>
      as_tibble() |>
      mutate(species = "Chroicocephalus ridibundus") |>
      right_join(
        give_excretion_carnivores(
          abundancies_1location |>
            mutate(
              days_month = lubridate::days_in_month(
                as.Date(paste(year, month, "01", sep = "-"))
              ),
              season =
                rep(c("winter", "winter", "spring", "spring", "summer"), 3)
            ) |>
            transmute(
              species_name = "Chroicocephalus ridibundus",
              n_individuals,
              n_days = days_month,
              var_season = season
            )
        ),
        by = c("species" = "species_name", "n_individuals")
      ) |>
      reframe(
        n_tot = sum(n_tot),
        p_tot = sum(p_tot),
        .by = c("location", "species", "year")
      )
  )
})

test_that("give_intake_model() aggregates correct", {
  expect_equal(
    give_intake_model(abundancies_1location),
    abundancies_1location |>
      as_tibble() |>
      right_join(
        give_intake_herbivores(
          abundancies_1location |>
            mutate(
              days_month = lubridate::days_in_month(
                as.Date(paste(year, month, "01", sep = "-"))
              ),
              season =
                rep(c("winter", "winter", "spring", "spring", "summer"), 3)
            ) |>
            transmute(
              species_name = species,
              n_individuals,
              n_days = days_month,
              var_season = season
            )
        ),
        by = c("species" = "species_name", "n_individuals")
      ) |>
      reframe(
        n_tot_25 = sum(n_tot_25),
        n_tot_50 = sum(n_tot_50),
        n_tot_75 = sum(n_tot_75),
        p_tot_25 = sum(p_tot_25),
        p_tot_50 = sum(p_tot_50),
        p_tot_75 = sum(p_tot_75),
        .by = c("location", "species", "year")
      )
  )
  expect_equal(
    give_intake_model(
      abundancies_1location |>
        mutate(species = "Chroicocephalus ridibundus")
    ),
    abundancies_1location |>
      as_tibble() |>
      mutate(species = "Chroicocephalus ridibundus") |>
      right_join(
        give_intake_carnivores(
          abundancies_1location |>
            mutate(
              days_month = lubridate::days_in_month(
                as.Date(paste(year, month, "01", sep = "-"))
              ),
              season =
                rep(c("winter", "winter", "spring", "spring", "summer"), 3)
            ) |>
            transmute(
              species_name = "Chroicocephalus ridibundus",
              n_individuals,
              n_days = days_month,
              var_season = season
            )
        ),
        by = c("species" = "species_name", "n_individuals")
      ) |>
      reframe(
        n_tot_25 = sum(n_tot_25),
        n_tot_50 = sum(n_tot_50),
        n_tot_75 = sum(n_tot_75),
        p_tot_25 = sum(p_tot_25),
        p_tot_50 = sum(p_tot_50),
        p_tot_75 = sum(p_tot_75),
        .by = c("location", "species", "year")
      )
  )
})

test_that("results are similar to tool", {
  expect_equal(
    give_intake_herbivores(
      data.frame(
        species_name = "Anas acuta",
        n_individuals = 10,
        n_days = 20,
        var_season = c("winter", "spring")
      )
    ),
    data.frame(
      species_name = "Anas acuta", n_individuals = 10,
      var_season = c("spring", "winter"),
      n_tot_25 = c(0, 0.0318),
      n_tot_50 = c(0, 0.0386),
      n_tot_75 = c(0, 0.0454),
      p_tot_25 = c(0, 0.00262),
      p_tot_50 = c(0, 0.00315),
      p_tot_75 = c(0, 0.00361)
    ),
    tolerance = 0.002
  )
  expect_equal(
    give_dropping_herbivores(
      data.frame(
        species_name = "Anas acuta",
        n_individuals = 10,
        n_days = 20,
        var_season = c("winter", "spring")
      )
    ),
    data.frame(
      species_name = "Anas acuta", n_individuals = 10,
      var_season = c("spring", "winter"),
      n_tot = c(0, 0.0237),
      p_tot = c(0, 0.00326)
    ),
    tolerance = 0.002
  )
  expect_equal(
    give_intake_carnivores(
      data.frame(
        species_name = "Chroicocephalus ridibundus",
        n_individuals = 10,
        n_days = 20
      )
    ),
    data.frame(
      species_name = "Chroicocephalus ridibundus",
      n_individuals = 10,
      n_tot_25 = 0.28,
      n_tot_50 = 0.305,
      n_tot_75 = 0.329,
      p_tot_25 = 0.0288,
      p_tot_50 = 0.0601,
      p_tot_75 = 0.0912
    ),
    tolerance = 0.001
  )
  expect_equal(
    give_excretion_carnivores(
      data.frame(
        species_name = "Chroicocephalus ridibundus",
        n_individuals = 10,
        n_days = 20
      )
    ),
    data.frame(
      species_name = "Chroicocephalus ridibundus",
      n_individuals = 10,
      n_tot = 0.129,
      p_tot = 0.059
    ),
    tolerance = 0.003
  )
  expect_equal(
    give_intake_model(
      data.frame(
        species = c(
          rep("Chroicocephalus ridibundus", 24),
          rep("Branta canadensis", 24),
          rep("Anser anser", 24),
          rep("Anas platyrhynchos", 24),
          rep("Vanellus vanellus", 24)
        ),
        year = c(rep(2020, 12), rep(2021, 12)),
        month = 1:12,
        location = "A",
        n_individuals = c(rep(10, 12), rep(30, 12))
      )
    ) |>
      as.data.frame(),
    data.frame(
      location = "A",
      species = c(
        rep("Branta canadensis", 2),
        rep("Anser anser", 2),
        rep("Anas platyrhynchos", 2),
        rep("Chroicocephalus ridibundus", 2),
        rep("Vanellus vanellus", 2)
      ),
      year = 2020:2021,
      n_tot_25 = c(
        11.052755, 33.09, 7.656, 22.89, 1.204, 3.592, 5.124, 15.32,
        7.293404, 21.820431
      ),
      n_tot_50 = c(
        13.44, 40.15, 9.291, 27.8, 1.462, 4.36, 5.574, 16.7,
        7.934275, 23.737790
      ),
      n_tot_75 = c(
        15.83, 47.33, 10.964, 32.78, 1.722, 5.139, 6.024, 18.01,
        8.575146, 25.655149
      ),
      p_tot_25 = c(
        0.9126, 2.73, 0.6325, 1.891, 0.0995, 0.2969, 0.5267, 1.579,
        0.749605, 2.242669
      ),
      p_tot_50 = c(
        1.0956, 3.273, 0.7589, 2.265, 0.1194, 0.3561, 1.1004, 3.285,
        1.565108, 4.682496
      ),
      p_tot_75 = c(
        1.2536, 3.76, 0.871, 2.602, 0.1368, 0.4087, 1.667, 4.995,
        2.375670, 7.107536
      )
    ),
    tolerance = 0.003
  )
  expect_equal(
    give_dropping_model(
      data.frame(
        species = c(
          rep("Chroicocephalus ridibundus", 24),
          rep("Branta canadensis", 24),
          rep("Anser anser", 24),
          rep("Anas platyrhynchos", 24),
          rep("Vanellus vanellus", 24)
        ),
        year = c(rep(2020, 12), rep(2021, 12)),
        month = 1:12,
        location = "A",
        n_individuals = c(rep(10, 12), rep(30, 12))
      )
    ) |>
      as.data.frame(),
    data.frame(
      location = "A",
      species = c(
        rep("Branta canadensis", 2),
        rep("Anser anser", 2),
        rep("Anas platyrhynchos", 2),
        rep("Chroicocephalus ridibundus", 2),
        rep("Vanellus vanellus", 2)
      ),
      year = 2020:2021,
      n_tot = c(
        6.954, 20.83, 4.959, 14.82, 0.8743, 2.608, 2.352, 7.043,
        3.351391, 10.0267025
      ),
      p_tot = c(
        0.9518, 2.86, 0.6807, 2.035, 0.1199, 0.3579, 1.0793, 3.226,
        1.5335054, 4.5879465
      )
    ),
    tolerance = 0.003
  )
})
