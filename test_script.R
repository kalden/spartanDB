# R & MySQL tutorial
# https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#open-mysql-workbench

## Main Script here:

library(RMySQL)
library(spartan)
library(EasyABC)
# R needs a full path to find the settings file
#rmysql.settingsfile<-"~/Documents/sql_settings/spartanDB.cnf"
rmysql.settingsfile<-"/home/kja505/Dropbox/Sarcoid/spartanDB.cnf"
#rmysql.db<-"spartan_ppsim"
rmysql.db<-"spartan_sarcoid"
dblink<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)

parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue","maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
measures<-c("Velocity","Displacement")
baseline<- c(50,0.3, 0.2, 0.04, 0.60, 1.0)
minvals <- c(10, 0.10, 0.10, 0.015, 0.1, 0.25)
maxvals <- c(100, 0.9, 0.50, 0.08, 0.95, 5.0)
incvals <- c(10, 0.1, 0.05, 0.005, 0.05, 0.25)
measure_scale<-c("Velocity","Displacement")

# Delete the current database structure if there already
delete_database_structure(dblink)

# Set up the database:
create_database_structure(dblink, parameters, measures)

#### 1: LHC Sampling
## Route 1: Generate a sample and store in the database
#parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
generate_lhc_set_in_db(dblink, parameters, 500, minvals, maxvals, "normal", experiment_description="generated_lhc_set")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_id=1)
## Note the above has an optional date argument if you don't want to use today's date
## Route 2: Already have an existing sample and want to add it to the database
add_existing_lhc_sample_to_database(dblink, read.csv("~Documents/spartanDB/test_data/LHC_Params.csv",header=T), experiment_description="original ppsim lhc dataset")

#### 2: Robustness Sampling
#parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
generate_robustness_set_in_db(dblink,parameters, baseline, minvals, maxvals, incvals, experiment_id=NULL, experiment_description="PPSim Robustness")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_type="Robustness",experiment_id=5)

#### 3: eFAST Sampling
num_samples<-65
num_curves<-3
generate_efast_set_in_db(dblink, parameters, num_samples, minvals, maxvals, num_curves, experiment_id=NULL, experiment_description="PPSim eFAST2")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_type="eFAST",experiment_id=3)

## Or can add an existing generated set to the database, as is shown below

############ RECREATING THE ORIGINAL PPSIM ANALYSES:

#### 4: Adding LHC Results to Database
## Route 1: From Spartan 2, all results can be provided in a single CSV file - this method processes that file and puts all results in the DB
## In this case, we add the parameters from the tutorial set, don't generate them, such that the parameters can tie up with the results
data(pregenerated_lhc)
add_existing_lhc_sample_to_database(dblink, pregenerated_lhc, experiment_description="original ppsim lhc dataset")
# Now add the results for that experiment
experiment_id<-1 # Could have also added by description and date - these removed as default to NULL if ID specified
add_lhc_and_robustness_sim_results(dblink, parameters, measures, experiment_id, results_csv="~/Documents/spartanDB/test_data/LHC_AllResults.csv")
# Or could have used the object
data(ppsim_lhc_results)
add_lhc_and_robustness_sim_results(dblink, parameters, measures, experiment_id, results_obj=ppsim_lhc_results)

# Now analyse the replicates to create a summary result
summarise_replicate_lhc_runs(dblink, measures, experiment_id)
# Now we have the data in a format that spartan can process - so we'll do the analysis
generate_lhc_analysis(dblink, parameters, measures, experiment_id=1)
# Graph an experiment - the graphs are never stored in the database, but we provide methods to graph for an experiment_id

output_directory<-"~/Desktop/"
graph_lhc_analysis(dblink, parameters, measures, measure_scale, output_directory, experiment_id=1, output_type=c("PDF"))

# Or we could generate an R object (as robospartan will) and add that to the database
lhc_set<-spartan::lhc_generate_lhc_sample(FILEPATH=NULL, parameters, 500, minvals, maxvals, "normal", write_csv = FALSE)
add_existing_lhc_sample_to_database(dblink, lhc_set, experiment_description="original ppsim lhc dataset")

#### 5: Adding eFAST Results to Database
## CSV file:
# In this case, we add the parameters from the tutorial set, don't generate them, such that the parameters can tie up with the results
# Pregenerated eFAST sample now part of the package
# Note eFAST need
dir.create(file.path(getwd(), "efast"), showWarnings = FALSE)
unzip(system.file("extdata","pregenerated_efast_sample.zip",package="spartanDB"),exdir=file.path(getwd(), "efast"))
num_curves<-3
add_existing_efast_sample_to_database(dblink, parameters, num_curves, parameter_set_path=file.path(getwd(), "efast"), experiment_description="Original PPSim eFAST")

# Now add the results for this experiment - file available online, we're going to extract into the same folder as created for the samples
sample_results<-"~/Documents/spartanDB/test_data/eFAST_Sample_Outputs.zip"
unzip(sample_results,exdir=file.path(getwd(), "efast"))
experiment_id<-2 # Could have also added by description and date - these removed as default to NULL if ID specified
add_efast_sim_results_from_csv_files(dblink, file.path(getwd(), "efast"), parameters, measures, num_curves, experiment_id)
# Now we can create summary stats from the replicates:
summarise_replicate_efast_runs(dblink, parameters, measures, experiment_id=2)
# Now do the eFAST Analysis
generate_efast_analysis(dblink, parameters, measures, experiment_id=2, graph_results=TRUE, output_directory=output_directory)

# Or we could generate an R object (as robospartan will) and add that to the DB
# Current spartan does not generate the dummy - needs specifying
efast_set<-spartan::efast_generate_sample(FILEPATH=NULL, 3, 65, c(parameters,"Dummy"), c(minvals,1), c(maxvals,2), write_csv = FALSE, return_sample = TRUE)
add_existing_efast_sample_to_database(dblink, parameters, num_curves=3, parameters_r_object=efast_set, experiment_description="Original PPSim eFAST")

# Delete the extracted files
unlink(file.path(getwd(), "efast"), recursive=TRUE)


#### 6: Adding Robustness Results to Database
# In this case, we add the parameters from the tutorial set, don't generate them, such that the parameters can tie up with the results
data(ppsim_robustness_set)
# Read these into the database:
add_existing_robustness_sample_to_database(dblink, parameters, ppsim_robustness_set, experiment_description="Original PPSim Robustness")

# Now add the results for this experiment:
experiment_id<-3
data(ppsim_robustness_results)
add_lhc_and_robustness_sim_results(dblink, parameters, measures, "Robustness", experiment_id, results_obj=ppsim_robustness_results)

# Now create summary stats from these replicates
# Replicate responses not analysed for OAT
generate_robustness_analysis(dblink, parameters, measures, baseline, experiment_id=3)
graph_robustness_analysis(dblink, "/home/kja505/Desktop/",parameters, measures, experiment_id=3)


####### MACHINE LEARNING SECTION
#### Now mine the database for experiments to use to create emulations and ensembles, using spartan
emulators<-create_emulators_from_database_experiments(dblink, parameters, measures, emulator_list=c("RF"),normalise_set=TRUE,experiment_id=1)

# Or can regenerate the emulators from previous emulator data in the database
emulators<-regenerate_emulators_from_db_data(dblink, parameters, measures, emulator_list, normalise_set=TRUE, experiment_id=5)

# Now to generate some ensembles from the emulators
validation_set<-retrieve_validation_set_from_db_for_emulator(dblink, parameters, measures, experiment_id=4)
# Try to make some predictions
use_emulators_to_make_and_store_predictions(dblink, emulators, parameters, measures, validation_set, normalise=FALSE, normalise_result=TRUE, experiment_description="Predict Validation Set")

# Generate emulators and an ensemble
ensemble<-generate_emulators_and_ensemble_using_db(dblink, parameters, measures, emulator_list=c("RF","SVM"), normalise_set=TRUE, experiment_id=2)
# Use ensemble to generate predictions
validation_set<-retrieve_validation_set_from_db_for_emulator(dblink, parameters, measures, experiment_id=5)
use_ensemble_to_make_and_store_predictions(dblink, ensemble, parameters, measures, validation_set, normalise=FALSE, normalise_result=TRUE, experiment_description="Predict Validation Set2")


##### Demonstration of using the Ensemble to perform SA and add straight to the DB
#1: LHC
emulated_lhc_values<-spartan::lhc_generate_lhc_sample(NULL,parameters,500,minvals,maxvals, "normal",write_csv=FALSE)
analyse_and_add_emulated_lhc_to_db(dblink, emulated_lhc_values, ensemble, parameters, measures, experiment_description="Emulated LHC Analysis", output_directory="/home/kja505/Desktop", normalise_sample=TRUE)

#2:eFAST
emulated_efast_values<-efast_generate_sample(NULL, 3,65,c(parameters,"Dummy"), c(minvals,0), c(maxvals,1), write_csv=FALSE, return_sample=TRUE)
analyse_and_add_emulated_efast_to_db(dblink, emulated_efast_values, ensemble, parameters, measures, experiment_description="Emulated eFAST Analysis2",
                                               graph_results=TRUE, output_directory="/home/kja505/Desktop", normalise_sample=TRUE, normalise_result=TRUE)


all_curve_results<-emulate_efast_sampled_parameters(NULL, ensemble, c(parameters,"Dummy"), measures, 3, normalise = TRUE, csv_file_input=FALSE,
                                                    spartan_sample_obj=emulated_efast_values,write_csv_file_out=FALSE, normalise_result=TRUE)
analyse_and_add_emulated_efast_to_db(dblink, emulated_efast_values, all_curve_results, c(parameters,"Dummy"), measures, experiment_id=NULL, experiment_description="Test emulated eFAST3",
                                     graph_results=TRUE, output_directory="/home/kja505/Desktop")


#### Can we store ABC data in the database too?
normalise_values = TRUE
normalise_result = TRUE
prior=list(c("unif",0,100),c("unif",0.1,0.9),c("unif",0.1,0.5),
           c("unif",0.015,0.08),c("unif",0.1,1.0),c("unif",0.25,5.0))
sum_stat_obs=c(4.4677342593,28.5051144444)
abc_set<-create_abc_settings_object(parameters, measures, ensemble, normalise_values,
                           normalise_result, file_out = FALSE)
numRunsUnderThreshold=100
tolerance=c(20,15,10.00,7,5.00)
abc_resultSet<-ABC_sequential(method="Beaumont",
                              model=ensemble_abc_wrapper, prior=prior,
                              nb_simul=numRunsUnderThreshold,
                              summary_stat_target=sum_stat_obs,
                              tolerance_tab=tolerance, verbose=FALSE)
store_abc_experiment_in_db(dblink, abc_set, abc_resultSet, parameters, measures, experiment_id=NULL, experiment_description="ABC Test",
                           graph_results=TRUE, output_directory="/home/kja505/Desktop")

# Retrieve stored results for plotting
retrieve_abc_experiment_for_plotting(dblink, experiment_description="ABC Test", experiment_date = Sys.Date())

dbDisconnect(dblink)
