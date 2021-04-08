source("connection_to_datasets/init_all_datasets.R")
connection   <- connect.all.datasets(ds.test_env)

context("dstr.check.param::expt::incorrect")
test_that("dstr.check.param::expt::incorrect",
{
  expect_error(dstr.check.param())
  expect_error(dstr.check.param(1))
  expect_error(ddstr.check.param("hi",3))
  expect_error(dstr.check.param("hi","hi",3))
  expect_error(dstr.check.param("hi","hi","hi",NULL))
})

connection   <- connect.all.datasets(ds.test_env)
context("dstr.check.param::expt::correct")
test_that("dstr.check.param::expt::correct",
{
  expect_true(dstr.check.param(data.server = "datashield.mtcars.data",
                                data.encrypted = "datashield.encrypted.data",
                                data.held.in.server = "D",
                                datasources = connection))
})

log.out.data.server()
