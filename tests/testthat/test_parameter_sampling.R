library(spartanDB)
context("Test of spartanDB Parameter Sampling")

test_that("set_new_experiment_flag", {

  # Should return TRUE if NULL, FALSE otherwise
  expect_true(set_new_experiment_flag(NULL))
  expect_false(set_new_experiment_flag(2))
})

test_that("check_experiment_id", {

  skip_on_travis()
  skip_on_cran()

  # First, if experiment_id is NULL, an experiment should be created.
  # Here we are assuming an empty database, so this should be created as 1

  dblink<-setup_db_link()
  delete_database_structure(dblink)
  create_database_structure(dblink, c("parameter1","parameter2"), c("Velocity","Displacement"))

  expect_equal(check_experiment_id(dblink, experiment_id=NULL, "LHC", Sys.Date(), "ID Test"),1)

  # Specify an ID, and see if it is already in the DB. We can do that as we created experiment ID 1 above
  # In this case, the experiment ID should be returned as there will be no parameter sets for that experiment
  expect_equal(check_experiment_id(dblink, experiment_id=1, "LHC", Sys.Date(), "ID Test"),1)

  # However we need to test the case where there are parameter sets assigned to that experiment, so -1 should
  # be returned
  # Need to add a row to the parameters table
  r<-cbind(0.1,0.1,1)
  colnames(r)<-c("parameter1","parameter2","experiment_id")
  RMySQL::dbWriteTable(dblink, value = as.data.frame(r), row.names = FALSE, name = "spartan_parameters", append = TRUE )

  # Now we can test -1 is returned
  expect_equal(check_experiment_id(dblink, experiment_id=1, "LHC", Sys.Date(), "ID Test"),-1)

  # And test whether an experiment id that does not exist is shown as not in the database
  expect_message(check_experiment_id(dblink, experiment_id=2, "LHC", Sys.Date(), "ID Test") ,"Specified experiment ID 2 is not in the database. Set not added to the Database")
  close_db_link(dblink)
})

test_that("generate_lhc_set_in_db", {

  skip_on_travis()
  skip_on_cran()

  dblink<-setup_db_link()
  delete_database_structure(dblink)
  create_database_structure(dblink, c("parameter1","parameter2"), c("Velocity","Displacement"))

  # Testing generation of new LHC set for a new experiment
  expect_message(generate_lhc_set_in_db(dblink, c("parameter1","parameter2"),10 , c(0.1,0.1), c(1,1), "normal", experiment_description="Test LHC", experiment_date = Sys.Date()),"Parameter Set Added to Database, with Experiment ID 1")

  # Should have 10 rows in the parameter database for this experiment
  expect_equal(nrow(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")),10)

  # Here we can also test that paramOfInterest and curve columns are all blank, not used for LHC analysis
  expect_true(all(is.na(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")[,5])))
  expect_true(all(is.na(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")[,6])))

  # Can check error generation here, if say there was a typo in max values. Error should be returned where experiment ID 2 is rolled back
  expect_message(generate_lhc_set_in_db(dblink, c("parameter1","parameter2"),10 , c(A,0.1), c(1,1), "normal", experiment_description="Test LHC2", experiment_date = Sys.Date()), "Experiment ID 2, created in failed attempt at sample generation, now deleted from DB")

  close_db_link(dblink)

})

test_that("generate_robustness_set_in_db", {

  skip_on_travis()
  skip_on_cran()

  dblink<-setup_db_link()
  delete_database_structure(dblink)
  create_database_structure(dblink, c("parameter1","parameter2"), c("Velocity","Displacement"))
  parameters<-c("parameter1","parameter2")

  # Much of the utility functions have been tested - here we check parameter set generation, record structure, and error
  expect_message(generate_robustness_set_in_db(dblink,parameters,c(0.3, 0.2), c(0.10, 0.10), c(0.9, 0.50), c(0.1, 0.05), experiment_description="Robustness Test", experiment_date=Sys.Date()),"Parameter Set Added to Database, with Experiment ID 1")

  # Check the correct number of records
  expect_equal(nrow(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")),18)

  # Now we can check the structure - robustness analysis should complete the paramOfInterest column
  expected_result <- c(rep(parameters[1],9),rep(parameters[2],9))
  expect_equal(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")[,5],expected_result)

  expect_true(all(is.na(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")[,6])))

  close_db_link(dblink)
})

test_that("generate_efast_set_in_db", {

  skip_on_travis()
  skip_on_cran()

  dblink<-setup_db_link()
  delete_database_structure(dblink)
  create_database_structure(dblink, c("parameter1","parameter2"), c("Velocity","Displacement"))
  parameters<-c("parameter1","parameter2")
  numcurves<-3

  # Generate the sample
  generate_efast_set_in_db(dblink, parameters, 65, c(0.10, 0.10), c(0.9, 0.50), numcurves, experiment_description="eFAST Test", experiment_date=Sys.Date())

  # Should be 390 rows
  db_result<-DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")
  expect_equal(nrow(db_result),585)

  # Now check the structure
  expected_result <- c(rep(parameters[1],65),rep(parameters[2],65),rep("Dummy",65),rep(parameters[1],65),rep(parameters[2],65),rep("Dummy",65),rep(parameters[1],65),rep(parameters[2],65),rep("Dummy",65))
  expect_equal(db_result[,5],expected_result)

  # eFAST also uses the curve column - so check the structure of that
  expected_result <- c(rep(1,195),rep(2,195),rep(3,195))
  expect_equal(db_result[,6],expected_result)

  close_db_link(dblink)

})

test_that("add_existing_lhc_sample_to_database", {

  skip_on_travis()
  skip_on_cran()

  # Test addition of a new sample
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  create_database_structure(dblink, c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope"), c("Velocity","Displacement"))

  expect_message(add_existing_lhc_sample_to_database(dblink, read.csv("~/Dropbox/RoboCalc/LHC_Params.csv",header=T), experiment_description="Test Existing LHC Set", experiment_date = Sys.Date()),
                                                    "Parameter Set Added to Database, with Experiment ID 1")

  # Check correct number of records
  db_result<-DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")
  expect_equal(nrow(db_result),500)

  # Check paramOfInterest and curve are null
  expect_true(all(is.na(db_result[,8])))
  expect_true(all(is.na(db_result[,9])))

  close_db_link(dblink)

})

test_that("add_existing_efast_sample_to_database", {

  skip_on_travis()
  skip_on_cran()

  # Test addition of a new sample
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue","maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
  measures<-c("Velocity","Displacement")
  create_database_structure(dblink, parameters, measures)
  parameter_set_path<-"~/Downloads/Spartan_Tutorial_Data/eFAST_Spartan2"
  num_curves<-3

  expect_message(add_existing_efast_sample_to_database(dblink, parameters, num_curves, parameter_set_path=parameter_set_path, experiment_id=NULL),"Parameter Set Added to Database, with Experiment ID 1")

  # Can now check structure as we did previously
  # Should be 1365
  db_result<-DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id=1;")
  expect_equal(nrow(db_result),1365)

  # eFAST also uses the curve column - so check the structure of that
  expected_result <- c(rep(1,455),rep(2,455),rep(3,455))
  expect_equal(db_result[,10],expected_result)

  close_db_link(dblink)
})

test_that("add_existing_robustness_sample_to_database", {

  skip_on_travis()
  skip_on_cran()

  # Test addition of a new sample
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  parameter_set_path<-"~/Documents/spartanDB/test_data"
  parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue","maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
  measures<-c("Velocity","Displacement")
  create_database_structure(dblink, parameters, measures)

  expect_message(add_existing_robustness_sample_to_database(dblink, parameters, parameter_set_path=parameter_set_path,experiment_description="Original Robustness"),"Parameter Sets Added to Database, with Experiment ID 1")

  # Check the correct number of records
  expect_equal(nrow(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")),82)

  # Now we can check the structure - robustness analysis should complete the paramOfInterest column
  expected_result <- c(rep(parameters[1],11),rep(parameters[2],9),rep(parameters[3],9),rep(parameters[4],14),rep(parameters[5],19),rep(parameters[6],20))
  expect_equal(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")[,9],expected_result)

  expect_true(all(is.na(DBI::dbGetQuery(dblink, "SELECT * FROM spartan_parameters WHERE experiment_id= '1'")[,8])))

  close_db_link(dblink)
})

test_that("download_sample_as_csvfile", {

  skip_on_travis()
  skip_on_cran()

  # Test addition of a new sample
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  parameter_set_path<-"~/Documents/spartanDB/test_data"
  parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue","maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
  measures<-c("Velocity","Displacement")
  create_database_structure(dblink, parameters, measures)

  add_existing_robustness_sample_to_database(dblink, parameters, parameter_set_path=parameter_set_path)

  # Check can download this sample
  expect_message(download_sample_as_csvfile(getwd(), dblink, experiment_type="Robustness",experiment_id=1),paste("Sample exported as CSV file to ",getwd(),"/generated_sample.csv",sep=""))

  # Open the CSV file and check structure
  r<-read.csv(file.path(getwd(),"generated_sample.csv"),header=T)

  # For a robustness analysis, this should contain the two parameters and parameter of interest
  expect_equal(ncol(r),8)
  expect_equal(nrow(r),82)

  # Column 3 should only contain the parameters
  expect_true(all(r[,8] %in% parameters))

  # Delete the csv file
  file.remove(file.path(getwd(),"generated_sample.csv"))

  close_db_link(dblink)
})
