context("Value and class")

test_that("date manipulation", {
  result <- eq_date("2001-01-01")
  expect_that(result, is_a("Date"))
  expect_that(as.character(result), equals("2001-01-01"))
  result <- eq_date("-2001-01-01")
  expect_that(result, is_a("Date"))
  expect_that(as.character(result), equals("-2001-01-01"))
})

test_that("data Cleaning", {
  data(raw_df)
  clean_df <- eq_clean_data(raw_df)
  expect_that(clean_df, is_a("data.frame"))
  expect_that(clean_df$DATE, is_a("Date"))
})

test_that("location name cleaning", {
  a <- c(
    "JORDAN:BAB-A-DARAA,AL-KARAK" ,
    "SYRIA:UGARIT" ,
    "TURKMENISTAN:W" ,
    "GREECE:THERA ISLAND (SANTORINI)",
    "ISRAEL:ARIHA (JERICHO)",
    "ITALY:LACUS CIMINI"
  )
  b <- c(
    "Bab-A-Daraa,Al-Karak",
    "Ugarit",
    "W",
    "Thera Island (Santorini)",
    "Ariha (Jericho)",
    "Lacus Cimini"
  )
  expect_that(eq_location_clean(a), equals(b))
})
