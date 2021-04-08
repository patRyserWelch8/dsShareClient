source("connection_to_datasets/init_all_datasets.R")
connection   <- connect.all.datasets(ds.test_env)

context("dsce.check.data.encrypted::expt::multiple")
test_that("dsce.check.data.encrypted::expt::multiple",
{
  expect_true(ds.assign.sharing.settings(connection))
  expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),  datasources = connection)))
  outcome <- dsce.check.data.encrypted("datashield.mtcars.data","datashield.mtcars.data","D", connection)
  expect_true(FALSE %in% unlist(outcome))

})

test_that("dsce.check.data.encrypted::param::incorrect",
{
  expect_false(dsce.check.data.encrypted())
  expect_false(dsce.check.data.encrypted(1))
  expect_false(dsce.check.data.encrypted("hi",3))
  expect_false(dsce.check.data.encrypted("hi","hi",3))
  expect_false(dsce.check.data.encrypted("hi","hi","hi",NULL))

})
log.out.data.server()


connection   <- connect.dataset.2(ds.test_env)

context("dsce.check.data.encrypted::expt::single")
test_that("dsce.check.data.encrypted::expt::single",
{
  expect_true(ds.assign.sharing.settings(connection))
  expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),  datasources = connection)))
  outcome <- dsce.check.data.encrypted("datashield.mtcars.data","datashield.mtcars.data","D", connection)
  expect_true(FALSE %in% unlist(outcome))

})

test_that("dsce.check.data.encrypted::param::incorrect",
{
  expect_false(dsce.check.data.encrypted())
  expect_false(dsce.check.data.encrypted(1))
  expect_false(dsce.check.data.encrypted("hi",3))
  expect_false(dsce.check.data.encrypted("hi","hi",3))
  expect_false(dsce.check.data.encrypted("hi","hi","hi",NULL))
})
log.out.data.server()

connection = NULL
context("dsce.check.data.encrypted::expt::no_connection")
test_that("ds.check.data.encrypted::expt::no_connection",
{
  expect_false(ds.assign.sharing.settings(connection))
  expect_false(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),  datasources = connection)))
  expect_error(outcome <- dsce.check.data.encrypted("datashield.mtcars.data","datashield.mtcars.data","D", connection))
})

test_that("dsce.check.data.encrypted::param::no_connection",
{
  expect_false(dsce.check.data.encrypted())
  expect_false(dsce.check.data.encrypted(1))
  expect_false(dsce.check.data.encrypted("hi",3))
  expect_false(dsce.check.data.encrypted("hi","hi",3))
  expect_false(dsce.check.data.encrypted("hi","hi","hi",NULL))
})
