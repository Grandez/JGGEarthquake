library(JGGEarthquake)

## eq_get_data tests

test_data_raw <- eq_get_data()

test_that("eq_get_data access to NOAA server", {
    expect_true(nrow(test_data_raw) > 1000)
})

test_data_raw <- eq_get_data("../extdata/signif.txt")

test_that("RAW data is in package", {
    expect_true(nrow(test_data_raw) > 1000)
})

## eq_clean_data test

test_data_clean <- eq_clean_data(test_data_raw)
test_that("eq_clean_data has correct classes", {
      expect_is(test_data_clean,"data.frame")
      expect_is(test_data_clean$DATE,"Date")
      expect_is(test_data_clean$LONGITUDE,"numeric")
      expect_is(test_data_clean$LATITUDE,"numeric")
})

## eq_load_data test

test_data_clean <- eq_load_data("../extdata/signif.txt")
test_that("eq_load_data has correct classes", {
    expect_is(test_data_clean,"data.frame")
    expect_is(test_data_clean$DATE,"Date")
    expect_is(test_data_clean$LONGITUDE,"numeric")
    expect_is(test_data_clean$LATITUDE,"numeric")
})

