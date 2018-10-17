# R & MySQL tutorial
# https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#open-mysql-workbench

## Main Script here:

library(RMySQL)
library(spartan)
# R needs a full path to find the settings file
rmysql.settingsfile<-"~/Documents/sql_settings/spartanDB.cnf"
rmysql.db<-"spartan_ppsim"
dblink<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)

parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue","maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
measures<-c("Velocity","Displacement")
baseline<- c(50,0.3, 0.2, 0.04, 0.60, 1.0)
minvals <- c(10, 0.10, 0.10, 0.015, 0.1, 0.25)
maxvals <- c(100, 0.9, 0.50, 0.08, 1.0, 5.0)
incvals <- c(10, 0.1, 0.05, 0.005, 0.05, 0.25)

# Delete the current database structure if there already
delete_database_structure(dblink)

# Set up the database:
create_database_structure(dblink, parameters, measures)

#### 1: LHC Sampling
## Route 1: Generate a sample and store in the database
#parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
generate_lhc_set_in_db(dblink, parameters, 500, minvals, maxvals, "normal", experiment_description="generated_lhc_set")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_type="LHC",experiment_id=1)
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
add_lhc_and_robustness_sim_results_from_csv_file(dblink, parameters, measures, "LHC", experiment_id, results_csv="~/Documents/spartanDB/test_data/LHC_AllResults.csv")
# Or could have used the object
data(ppsim_lhc_results)
add_lhc_and_robustness_sim_results_from_csv_file(dblink, parameters, measures, "LHC", experiment_id, results_obj=ppsim_lhc_results)

# Now analyse the replicates to create a summary result
summarise_replicate_lhc_runs(dblink, measures, experiment_id)
# Now we have the data in a format that spartan can process - so we'll do the analysis
generate_lhc_analysis(dblink, parameters, measures, experiment_id=1)
# Graph an experiment - the graphs are never stored in the database, but we provide methods to graph for an experiment_id
measure_scale<-c("Velocity","Displacement")
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
summarise_replicate_efast_runs(dblink, parameters, measures, experiment_id)
# Now do the eFAST Analysis
generate_efast_analysis(dblink, parameters, measures, experiment_id=2)
graph_efast_analysis(dblink, parameters, measures, output_directory, experiment_id=2)

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
add_existing_robustness_sample_to_database(dblink, parameters, ppsim_robustness_set, experiment_description="Original PPSim Robustness3")

# Now add the results for this experiment:
experiment_id<-8
data(ppsim_robustness_results)
add_lhc_and_robustness_sim_results_from_csv_file(dblink, parameters, measures, "Robustness", experiment_id, results_obj=ppsim_robustness_results)

# Now create summary stats from these replicates
# Replicate responses not analysed for OAT
generate_robustness_analysis(dblink, parameters, measures, baseline, experiment_id=8)
graph_robustness_analysis(dblink, "/home/kja505/Desktop/",parameters, measures, experiment_id=8)

dbDisconnect(dblink)
