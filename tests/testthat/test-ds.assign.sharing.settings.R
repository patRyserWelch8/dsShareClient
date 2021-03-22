source("definition_tests/def-ds.share_param.R")

connection <- connect.dataset.3(ds.test_env)
context('ds.assign.sharing.settings::smk::single')
test_that('single connections',
{
  expect_true(ds.assign.sharing.settings(connection))
})


context('ds.assign.sharing.settings::expt::assignSettings::single')
test_that('.assignSettings',
{
  .create.server.var(connection)
  expect_true(ds.assign.sharing.settings(connection))
})

log.out.data.server()


connections <- connect.all.datasets(ds.test_env)
context('ds.assign.sharing.settings::smk::multiple')
test_that('single connections',
{
  expect_true(ds.assign.sharing.settings(connections))
})



context('ds.assign.sharing.settings::expt::assignSettings::multiple')
test_that('.assignSettings',
{
  .create.server.var(connection)
  expect_true(ds.assign.sharing.settings(connections))
})

log.out.data.server()
