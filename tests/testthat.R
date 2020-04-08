library(testthat)
library(pipeR)

test_check("pipeR")

context("hydraulics")

test_that("mannings_properties_are_correct", {
  expect_equal(round(calc_hyd_radius(8,8),4), 0.1667)
  expect_equal(round(calc_hyd_radius(2,8),4), 0.0978)
  expect_equal(round(calc_hyd_radius(4,8),4), 0.1667)
  expect_equal(calc_hyd_radius(4,8), calc_hyd_radius_surcharged(4,8,0))
})