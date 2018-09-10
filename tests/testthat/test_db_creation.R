library(spartanDB)
context("Test of spartanDB Table Creation")

test_that("create_field_string", {

  # Function to create a string for a MySQL search - check the string is constructed correctly
  expect_equal(create_field_string(c("Velocity","Displacement")),"Velocity VARCHAR(45) NOT NULL,Displacement VARCHAR(45) NOT NULL,paramOfInterest VARCHAR(45),curve INT,")
})

test_that("create_experiments_table", {

  skip_on_travis()

  dblink<-setup_db_link()
  delete_database_structure(dblink)
  create_experiments_table(dblink)

  res<-DBI::dbGetQuery(dblink, "SELECT nullif(count(1),0) tableexists FROM information_schema.tables WHERE table_name='spartan_experiment'")
  expect_true(!is.na(res[[1]]))
  #RMySQL::dbClearResult(res)
  close_db_link(dblink)
})

test_that("create_parameter_values_table", {

  skip_on_travis()
  skip_on_cran()

  dblink<-setup_db_link()
  create_parameter_values_table(dblink,c("parameter1","parameter2"))
  res<-DBI::dbGetQuery(dblink, "SELECT nullif(count(1),0) tableexists FROM information_schema.tables WHERE table_name='spartan_parameters'")
  expect_true(!is.na(res[[1]]))
  #DBI::dbClearResult(dblink)
  close_db_link(dblink)
})

test_that("create_simulation_results_table", {

  skip_on_travis()
  skip_on_cran()

  dblink<-setup_db_link()
  create_simulation_results_table(dblink,c("Velocity","Displacement"))
  res<-DBI::dbGetQuery(dblink, "SELECT nullif(count(1),0) tableexists FROM information_schema.tables WHERE table_name='spartan_results'")
  expect_true(!is.na(res[[1]]))
  #DBI::dbClearResult(dblink)
  close_db_link(dblink)
})

test_that("create_analysed_results_table", {

  skip_on_travis()
  skip_on_cran()

  # This test assumes the database has no tables in to start with

  dblink<-setup_db_link()
  create_analysed_results_table(dblink,c("Velocity","Displacement"))
  res<-DBI::dbGetQuery(dblink, "SELECT nullif(count(1),0) tableexists FROM information_schema.tables WHERE table_name='spartan_analysed_results'")
  expect_true(!is.na(res[[1]]))
  #DBI::dbClearResult(dblink)
  close_db_link(dblink)
})

test_that("create_database_structure", {

  skip_on_travis()
  skip_on_cran()

  # This test assumes the database has no tables in to start with
  ## Prior to the first test, we clear the database tables, so we can test that these are created correctly
  dblink<-setup_db_link()
  delete_database_structure(dblink)

  create_database_structure(dblink, c("parameter1","parameter2"), c("Velocity","Displacement"))
  expect_equal(DBI::dbGetQuery(dblink, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'spartan_ppsim'")[[1]],5)

  close_db_link(dblink)

})

test_that("setup_experiment", {

  skip_on_travis()
  skip_on_cran()

  # This assumes the data-table was empty to start off with, so should return experiment ID's of 1 and 2
  dblink<-setup_db_link()
  expect_equal(setup_experiment(dblink,"LHC","2018-08-21", "Test LHC"),1)
  expect_equal(setup_experiment(dblink,"eFAST","2018-08-21", "Test eFAST"),2)
  expect_message(setup_experiment(dblink,"eFAST","2018-08-21", "Test eFAST"),"Experiment description and date already in the database")
  expect_equal(setup_experiment(dblink,"eFAST","2018-08-21", "Test eFAST"),-1)
  #RMySQL::dbClearResult(dblink)
  close_db_link(dblink)
})

