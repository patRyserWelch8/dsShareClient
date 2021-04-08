
context("dstr.is.eof::expt::no_connection")
test_that("dstr.is.oef::expt::no_connecction",
{
  expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = NULL))
})

context("dstr.next::expt::no_connection")
test_that("dstr.next::expt::no_connection",
{
   expect_equal(dstr.next(data.encrypted = "datashield.encrypted.data", datasources = NULL), "NR")
})


context("dstr.transfer::expt::no_connection")
test_that("dstr.transfer::expt::no_connection",
{
   expect_error(dstr.transfer(data.from.server = "datashield.mtcars.data",
                data.encrypted = "datashield.encrypted.data",
               data.held.in.server = "D",
               no.rows = 10,
               client.side.variable = "received.data",
               datasources = NULL))
})

context("ds.read::expt::no_connection")
test_that("ds.read::expt::no_connection",
{
   expect_false(ds.read(data.from.server = "datashield.mtcars.data",
                        data.encrypted = "datashield.encrypted.data",
                        data.held.in.server = "D",
                        no.rows = 10,
                        client.side.variable = "received.data",
                        datasources = NULL))
   expect_false(exists("received.data", where = 1))
})

connection   <- connect.dataset.1(ds.test_env)
dsShareClient::ds.assign.sharing.settings(datasources = connection)

context("dstr.transfer::expt::single")
test_that("dstr.transfer::expt::single",
{

   expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),
                                                                               datasources = connection)))
   expect_equal(dstr.transfer(data.from.server = "datashield.mtcars.data",
                              data.encrypted = "datashield.encrypted.data",
                              data.held.in.server = "D",
                              no.rows = 10,
                              client.side.variable = "received.data",
                              datasources = connection),
                              TRUE)
})

log.out.data.server()

connection   <- connect.dataset.1(ds.test_env)
context("ds.read::expt::single")
test_that("ds.read::expt::single",
{

   expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),
                                                                                         datasources = connection)))
   expect_equal(ds.read(data.from.server = "datashield.mtcars.data",
                                        data.encrypted = "datashield.encrypted.data",
                                        data.held.in.server = "D",
                                        no.rows = 10,
                                        client.side.variable = "received.data",
                                        datasources = connection),
                          TRUE)
   expect_true(exists("received.data", where = 1))
})
log.out.data.server()

connection   <- connect.dataset.1(ds.test_env)

context("dstr.is.eof::expt:::incorrect::single")
test_that("dstr.check.param::expt::single::incorrect",
{
  expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
})

context("dstr.next::expt:::incorrect::single")

test_that("dstr.next::expt:::incorrect::single::incorrect",
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
   ####print(dsConnectClient::ds.aggregate(expression = call("danger_settings"),
   #      datasources = connection))


   expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
})

log.out.data.server()


connection   <- connect.all.datasets(ds.test_env)

context("dstr.transfer::expt::multiple")
test_that("dstr.transfer::expt::multiple",
{

   expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),
                                                                               datasources = connection)))
   expect_equal(dstr.transfer(data.from.server = "datashield.mtcars.data",
                              data.encrypted = "datashield.encrypted.data",
                              data.held.in.server = "D",
                              no.rows = 10,
                              client.side.variable = "received.data",
                              datasources = connection),
      TRUE)
})

log.out.data.server()

connection   <- connect.all.datasets(ds.test_env)
context("ds.read::expt::multiple")
test_that("ds.read::expt::multiple",
{

             expect_true(dssp.transform.outcome.to.logical(dsConnectClient::ds.aggregate(expression = call("assignDemoDataDS"),
                                                                                         datasources = connection)))
             expect_equal(ds.read(data.from.server = "datashield.mtcars.data",
                                  data.encrypted = "datashield.encrypted.data",
                                  data.held.in.server = "D",
                                  no.rows = 10,
                                  client.side.variable = "received.data",
                                  datasources = connection),
                          TRUE)
             expect_true(exists("received.data", where = 1))
})
log.out.data.server()

connection   <- connect.all.datasets(ds.test_env)
dsShareClient::ds.assign.sharing.settings(datasources = connection)
context("dstr.is.eof::expt:::incorrect::multiple")

test_that("dstr.check.param::expt::multiple::incorrect",
{
   expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
})

context("dstr.nextf::expt:::incorrect::multiple")

test_that("dstr.check.param::expt::multiple::incorrect",
{
   expect_equal(dstr.next(data.encrypted = "datashield.encrypted.data", datasources = connection), "NR")
})


context("dstr.is.eof::expt::correct::multiple")
test_that("dstr.check.param::expt::multiple::correct",
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
   ####print(dsConnectClient::ds.aggregate(expression = call("danger_settings"),
   #      datasources = connection))


   expect_false(dstr.is.eof(data.encrypted = "datashield.encrypted.data", datasources = connection))
   ###print(dstr.next(data.encrypted = "datashield.encrypted.data", no.rows = 10, datasources = connection))
   ###print(dstr.next(data.encrypted = "datashield.encrypted.data", no.rows = 10, datasources = connection))
   ###print(dstr.next(data.encrypted = "datashield.encrypted.data", no.rows = 10, datasources = connection))
   ###print(dstr.next(data.encrypted = "datashield.encrypted.data", no.rows = 10, datasources = connection))
   ###print(dstr.next(data.encrypted = "datashield.encrypted.data", no.rows = 10, datasources = connection))

})

log.out.data.server()

