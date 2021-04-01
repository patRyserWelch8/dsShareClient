context("dstr.is.eof::expt::no_connection")
test_that("dstr.check.param::expt::no_connecction",
{
  expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = NULL))
})

context("dstr.next::expt::no_connection")
test_that("dstr.check.param::expt::no_connecction",
{
   expect_equal(dstr.next(data.encrypted = "datashield.encrypted.data", datasources = NULL), "NR")
})


connection   <- connect.dataset.1(ds.test_env)
dsShareClient::ds.assign.sharing.settings(datasources = connection)
context("dstr.is.eof::expt:::incorrect::single")

test_that("dstr.check.param::expt::single::incorrect",
{
  expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
})

context("dstr.nextf::expt:::incorrect::single")

test_that("dstr.check.param::expt::single::incorrect",
{
   expect_equal(dstr.next(data.encrypted = "datashield.encrypted.data", datasources = connection), "NR")
})


context("dstr.is.eof::expt::correct::single")
test_that("dstr.check.param::expt::single::correct",
{
   # complete steps before testing. is.oef
   expect_true(ds.assign.sharing.settings(connection))
   expect_true(dsConnectClient::ds.exists.on.server(variable.name = "settings_ds_share",
                                                    class.type = "list",
                                                    error.stop = TRUE,
                                                    datasources = connection))
   expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),
                                                                               datasources = connection)))

   expect_true(dstr.check.data.encrypted("datashield.mtcars.data","datashield.encrypted.data","D", connection))
   #print(dsConnectClient::ds.aggregate(expression = call("danger_settings"),
   #                                    datasources = connection))


   expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
   print(dstr.next(data.encrypted = "datashield.encrypted.data", no.rows = 10, datasources = connection))

})

log.out.data.server()
