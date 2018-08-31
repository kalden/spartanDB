library(spartanDB)
context("Test of spartanDB Table Deletion")

test_that("delete_database_structure", {

  skip_on_travis()
  skip_on_cran()

  # Tables created, then deleted here, assume no current database tables
  dblink<-setup_db_link()
  create_database_structure(dblink, c("parameter1","parameter2"), c("Velocity","Displacement"))
  # Creation has been tested elsewhere, here we check deletion
  delete_database_structure(dblink)

  # No tables should exist
  expect_equal(DBI::dbGetQuery(dblink, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'spartan_ppsim'")[[1]],0)
  close_db_link(dblink)
})

test_that("delete_experiment_table", {

  skip_on_travis()
  skip_on_cran()

  # Here we create the table and test we can delete it. Assumes no current database tables
  dblink<-setup_db_link()
  create_experiments_table(dblink)
  delete_experiment_table(dblink)

  # No tables should exist
  expect_equal(DBI::dbGetQuery(dblink, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'spartan_ppsim'")[[1]],0)
  close_db_link(dblink)
})

test_that("delete_parameters_table", {

  skip_on_travis()
  skip_on_cran()

  # Here we create the table and test we can delete it. Assumes no current database tables
  dblink<-setup_db_link()
  create_parameter_values_table(dblink,c("parameter1","parameter2"))

  delete_parameters_table(dblink)

  # No tables should exist
  expect_equal(DBI::dbGetQuery(dblink, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'spartan_ppsim'")[[1]],0)
  close_db_link(dblink)
})


test_that("delete_analysed_results_table", {

  skip_on_travis()
  skip_on_cran()

  # Here we create the table and test we can delete it. Assumes no current database tables
  dblink<-setup_db_link()
  create_analysed_results_table(dblink,c("Velocity","Displacement"))

  delete_analysed_results_table(dblink)
  # No tables should exist
  expect_equal(DBI::dbGetQuery(dblink, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'spartan_ppsim'")[[1]],0)
  close_db_link(dblink)
})

test_that("delete_results_table", {

  skip_on_travis()
  skip_on_cran()

  # Here we create the table and test we can delete it. Assumes no current database tables
  dblink<-setup_db_link()
  create_simulation_results_table(dblink,c("Velocity","Displacement"))

  delete_results_table(dblink)
  # No tables should exist
  expect_equal(DBI::dbGetQuery(dblink, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'spartan_ppsim'")[[1]],0)
  close_db_link(dblink)
})
