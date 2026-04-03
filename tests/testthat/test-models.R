library(dplyr)

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


test_that("results should be consistent", {
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
