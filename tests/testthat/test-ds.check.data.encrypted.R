connection   <- connect.all.datasets(ds.test_env)

source("connection_to_datasets/init_all_datasets.R")
context("dsce.server.call::expt")
test_that("dsce.server.call::expt",
{
  expect_true(ds.assign.sharing.settings(connection))
  expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),  datasources = connection)))
  #print(dsConnectClient::ds.aggregate(expression = call("lsDS", NULL, '.GlobalEnv'),  datasources = connection))
  outcome     <- dsce.server.call("datashield.mtcars.data","datashield.mtcars.data","D", connection)
  expect_false(outcome)
})


context("ds.check.data.encrypted::expt::single")
test_that("ds.check.data.encrypted::expt::single",
{
  expect_true(ds.assign.sharing.settings(connection))
  expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),  datasources = connection)))
  outcome <- ds.check.data.encrypted("datashield.mtcars.data","datashield.mtcars.data","D", connection)
  expect_true(FALSE %in% unlist(outcome))

})
log.out.data.server()


if(FALSE)
{
test_that("ds.check.data.encrypted::param::incorrect",
{
  expect_error(dsce.check.data.encrypted())
  expect_error(dsce.check.data.encrypted(1))
  expect_error(dsce.check.data.encrypted("hi",3))
  expect_error(dsce.check.data.encrypted("hi","hi",3))
  expect_error(dsce.check.data.encrypted("hi","hi","hi",NULL))
  expect_false(ds.check.data.encrypted())
  expect_false(ds.check.data.encrypted(1))
  expect_false(ds.check.data.encrypted("hi",3))
  expect_false(ds.check.data.encrypted("hi","hi",3))
  expect_false(ds.check.data.encrypted("hi","hi","hi",NULL))
})
}
