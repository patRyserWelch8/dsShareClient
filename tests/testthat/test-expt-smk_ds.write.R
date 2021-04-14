
context("ds.write::expt::incorrect_param")
test_that("ds.write::expt::incorrect_param",
{
  expect_false(ds.write())
  expect_false(ds.write(data.to.server = pi))
  expect_false(ds.write(data.to.server = "var.on.server"))
  expect_false(ds.write(data.to.server = "var.on.server",
                        client.side.variable = "myVar"))
  expect_false(ds.write(data.to.server = "var.on.server",
                        no.rows        = "albert"))
  expect_false(ds.write(data.to.server = "var.on.server",
                        no.rows        = 10,
                        client.side.variable = "myVar"))
  expect_false(ds.write(data.to.server = "var.on.server",
                        no.rows        = 10,
                        client.side.variable = "myVar",
                        column.server = "mycolumn"))
  expect_false(ds.write(data.to.server = "var.on.server",
                        no.rows        = 10,
                        client.side.variable = "myVar",
                        column.server = "mycolumn",
                        datasources   = "My lovely ..."))
  expect_false(ds.write(data.to.server = "var.on.server",
                        no.rows        = 10,
                        client.side.variable = "myVar",
                        column.server = "mycolumn",
                        datasources   = list()))

})

connection   <- connect.all.datasets(ds.test_env)
context("ds.write::expt::correct_param")
test_that("ds.write::expt:correct_param",
{
  data  <- data.frame(a = c(1:60000), b = c(1:60000), c = c(1:60000), x = 1:3)
  assign("myVar",data,  pos = 1)
  expect_true(exists("myVar", where = 1))

  expect_true(ds.assign.sharing.settings(connection))
  expect_true(dsConnectClient::ds.exists.on.server(variable.name = "settings_ds_share",
                                                   class.type = "list",
                                                   error.stop = TRUE,
                                                   datasources = connection))
  expect_true(ds.write(data.to.server = "var.on.server",
                        no.rows        = 10000,
                        client.side.variable = "myVar",
                        column.server = "x",
                        datasources   = connection))

})


context("dswr.check.param::expt::incorrect_param")
test_that("dswr.check.param::expt::incorrect_param",
{
            expect_error(dswr.check.param())
            expect_error(dswr.check.param(data.to.server = pi))
            expect_error(dswr.check.param(data.to.server = "var.on.server"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                  client.side.variable = "myVar"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                  no.rows        = "albert"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                  no.rows        = 10,
                                  client.side.variable = "myVar"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                  no.rows        = 10,
                                  client.side.variable = "myVar",
                                  column.server = "mycolumn"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                  no.rows        = 10,
                                  client.side.variable = "myVar",
                                  column  = "mycolumn",
                                  datasources   = "My lovely ..."))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                  no.rows        = 10,
                                  client.side.variable = "myVar",
                                  column = "mycolumn",
                                  datasources   = list()))

})

connection   <- connect.all.datasets(ds.test_env)
context("dswr.check.param::expt::correct_param")
test_that("dswr.check.param::expt:correct_param",
{
            data  <- data.frame(a = c(1:60000), b = c(1:60000), c = c(1:60000), x = 1:3)
            assign("myVar",data,  pos = 1)
            expect_true(exists("myVar", where = 1))
            expect_true(dswr.check.param(data.to.server = "var.on.server",
                                 no.rows        = 1000,
                                 client.side.variable = "myVar",
                                 column = "x",
                                 client.side.split = "myVar_split",
                                 datasources   = connection))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                         no.rows        = 10,
                                         client.side.variable = "myVar",
                                         column = "z", #incorrect column
                                         client.side.split = "myVar_split",
                                         datasources   = connection))

})


log.out.data.server()


connection   <- connect.dataset.2(ds.test_env)
context("dswr.check.param::expt::incorrect_param")
test_that("dswr.check.param::expt::incorrect_param",
{
            expect_error(dswr.check.param())
            expect_error(dswr.check.param(data.to.server = pi))
            expect_error(dswr.check.param(data.to.server = "var.on.server"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          client.side.variable = "myVar"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = "albert"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = 10,
                                          client.side.variable = "myVar"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = 10,
                                          client.side.variable = "myVar",
                                          column.server = "mycolumn"))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = 10,
                                          client.side.variable = "myVar",
                                          column  = "mycolumn",
                                          datasources   = "My lovely ..."))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = 10,
                                          client.side.variable = "myVar",
                                          column = "mycolumn",
                                          datasources   = connection))

})


context("dswr.check.param::expt::correct_param")
test_that("dswr.check.param::expt:correct_param",
{
            data  <- data.frame(a = c(1:3000), b = c(1:3000), c = c(1:3000), x = 1:3)
            assign("myVar",data,  pos = 1)
            expect_true(exists("myVar", where = 1))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                         no.rows        = 10,
                                         client.side.variable = "myVar",
                                         column = "x",
                                         client.side.split = "myVar_split",
                                         datasources   = connection))
            expect_error(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = 10,
                                          client.side.variable = "myVar",
                                          column = "z", #incorrect column
                                          client.side.split = "myVar_split",
                                          datasources   = connection))
            #incorrect number of servers
            data  <- data.frame(a = c(1:3000), b = c(1:3000), c = c(1:3000), x = 1)
            assign("myVar",data,  pos = 1)
            expect_true(dswr.check.param(data.to.server = "var.on.server",
                                          no.rows        = 10,
                                          client.side.variable = "myVar",
                                          column = "x",
                                          client.side.split = "myVar_split",
                                          datasources   = connection))

})


context("ds.write::expt::correct_param")
test_that("ds.write::expt:correct_param",
{
  data  <- data.frame(a = c(1:60000), b = c(1:60000), c = c(1:60000), x = 1)
  assign("myVar",data,  pos = 1)
  expect_true(exists("myVar", where = 1))

  expect_true(ds.assign.sharing.settings(connection))
  expect_true(dsConnectClient::ds.exists.on.server(variable.name = "settings_ds_share",
                                                   class.type = "list",
                                                   error.stop = TRUE,
                                                   datasources = connection))


  expect_true(ds.write(data.to.server = "var.on.server",
                       no.rows        = 10000,
                       client.side.variable = "myVar",
                       column.server = "x",
                       datasources   = connection))

  # the number of servers is incorrect for each label
  data  <- data.frame(a = c(1:60000), b = c(1:60000), c = c(1:60000), x = 1:3)
  assign("myVar",data,  pos = 1)
  expect_true(exists("myVar", where = 1))

  expect_false(ds.write(data.to.server = "var.on.server",
                       no.rows        = 30000,
                       client.side.variable = "myVar",
                       column.server = "x",
                       datasources   = connection))
})

log.out.data.server()

