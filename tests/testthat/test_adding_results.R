library(spartanDB)
context("Test of spartanDB Adding Results to Database")

test_that("add_lhc_and_robustness_sim_results_from_csv_file", {

  skip_on_travis()
  skip_on_cran()

  # Setup:
  # Test addition of a new sample
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  # Only two parameters in this test
  parameters<-c("chemoUpperLinearAdjust","chemoLowerLinearAdjust")
  measures<-c("Velocity","Displacement")
  create_database_structure(dblink, parameters, measures)
  parameter_set_path<-"~/Downloads/Spartan_Tutorial_Data/OAT_Spartan2/CSV_Structured/"
  add_existing_robustness_sample_to_database(dblink, parameter_set_path, parameters, experiment_description="Original PPSim Robustness")

  # Now test adding results for this experiment
  experiment_id<-1
  expect_message(add_lhc_and_robustness_sim_results_from_csv_file(dblink,paste(parameter_set_path,"/OAT_Medians.csv",sep=""), parameters, measures, "Robustness", experiment_id),
                 "6551 added to results database")

  # Now we can test the structure of the results table
  db_result<-DBI::dbGetQuery(dblink, "SELECT * FROM spartan_results WHERE experiment_set_id=1;")
  expect_equal(nrow(db_result),6555)

  # There should be no entries in the curve column
  expect_true(all(is.na(db_result[,5])))

  # All experiment ID should be 1
  expect_true(all(db_result[,7]==1))

  # Check parameter of interest column
  counts<-table(db_result[,4])
  expect_equal(as.numeric(counts[1]),3875)
  expect_equal(as.numeric(counts[2]),2680)

  close_db_link(dblink)
})

test_that("add_efast_sim_results_from_csv_files", {

  skip_on_travis()
  skip_on_cran()

  # Setup:
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  parameters<-c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy")
  measures<-c("Velocity","Displacement")
  create_database_structure(dblink, parameters, measures)

  parameter_set_path<-"~/Downloads/Spartan_Tutorial_Data/eFAST_Spartan2"
  num_curves<-3
  add_existing_efast_sample_to_database(dblink, parameter_set_path, parameters, num_curves,experiment_description="Original PPSim eFAST")

  # Now add the results for this experiment
  experiment_id<-1 # Could have also added by description and date - these removed as default to NULL if ID specified
  expect_message(add_efast_sim_results_from_csv_files(dblink, parameter_set_path, parameters, measures, num_curves, experiment_id),"Addition of eFAST results to database complete")

  # Examine database structure
  db_result<-DBI::dbGetQuery(dblink, "SELECT * FROM spartan_results WHERE experiment_set_id=1;")
  expect_equal(nrow(db_result),2799951)

  # Check curve column is correct (as used in this case)
  counts<-table(db_result[,5])
  expect_equal(as.numeric(counts[1]),934885)
  expect_equal(as.numeric(counts[2]),934605)
  expect_equal(as.numeric(counts[3]),930461)

  # Check experiment ID is all 1
  expect_true(all(db_result[,7]==1))

  close_db_link(dblink)

})

test_that("add_lhc_and_robustness_sim_results_from_csv_file", {

  skip_on_travis()
  skip_on_cran()

  # Setup:
  dblink<-setup_db_link()
  delete_database_structure(dblink)
  parameters<-c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
  measures<-c("Velocity","Displacement")
  create_database_structure(dblink, parameters, measures)

  add_existing_lhc_sample_to_database(dblink, read.csv("~/Downloads/Spartan_Tutorial_Data/LHC_Spartan2/Tutorial_Parameters_for_Runs.csv",header=T), experiment_description="original ppsim lhc dataset")

  # Now add the results for that experiment
  experiment_id<-1 # Could have also added by description and date - these removed as default to NULL if ID specified

  expect_message(add_lhc_and_robustness_sim_results_from_csv_file(dblink, "~/Downloads/Spartan_Tutorial_Data/LHC_Spartan2/LHC_AllResults.csv", parameters, measures, "LHC", experiment_id),"147507 added to results database")

  # Check structure
  db_result<-DBI::dbGetQuery(dblink, "SELECT * FROM spartan_results WHERE experiment_set_id=1;")
  expect_equal(nrow(db_result),147208)

  # In this case all of paramOfInterest and curve columns should be NA
  expect_true(all(is.na(db_result[,4])))
  expect_true(all(is.na(db_result[,5])))
  # All experiment ID should be 1
  expect_true(all(db_result[,7]==1))

  # Check correct number of parameter samples
  expect_equal(nrow(DBI::dbGetQuery(dblink, "SELECT DISTINCT parameter_set_id FROM spartan_results WHERE experiment_set_id=1;")),494)

  close_db_link(dblink)
})
