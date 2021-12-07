context("dstr.is.eof::expt::no_connection")
test_that("dstr.check.param::expt::no_connecction",
{
  expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = NULL))
})

connection   <- connect.dataset.1(ds.test_env)
context("dstr.is.eof::expt::single")
test_that("dstr.check.param::expt::single::incorrect",
{
  expect_error(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
})


test_that("dstr.check.param::expt::single::incorrect",
{
   # complete steps before testing. is.oef
   expect_true(ds.assign.sharing.settings(connection))
   print(dsConnectClient::ds.aggregate(expression = call("lsDS",NULL, ".GlobalEnv"),  datasources = connection))
   #expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),  datasources = connection)))
   #print(dsConnectClient::ds.aggregate(expression = call("lsDS",NULL, ".GlobalEnv"),  datasources = connection))
   #expect_true(dsce.check.data.encrypted("datashield.mtcars.data","datashield.encrypted.data","D", connection))

   expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
})

log.out.data.server()
