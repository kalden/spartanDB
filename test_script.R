# R & MySQL tutorial
# https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#open-mysql-workbench

## Main Script here:

library(RMySQL)
library(spartan)
# R needs a full path to find the settings file
rmysql.settingsfile<-"~/Documents/sql_settings/spartanDB.cnf"
rmysql.db<-"spartan_ppsim"
dblink<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)

# Simulation Settings
#parameters<-c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
# Only two parameters in the robustness analysis test
#parameters<-c("chemoUpperLinearAdjust","chemoLowerLinearAdjust")
# Slightly different names in eFAST set
#parameters<-c("BindProbability","ChemoThreshold","ChemoUpperLinearAdjust","ChemoLowerLinearAdjust","VCAMProbabilityThreshold","VCAMSlope","Dummy")
measures<-c("Velocity","Displacement")

# Delete the current database structure if there already
delete_database_structure(dblink)

# Set up the database:
create_database_structure(dblink, parameters, measures)

#### 1: LHC Sampling
## Route 1: Generate a sample and store in the database
parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
generate_lhc_set_in_db(dblink, parameters, 500, c(0.10, 0.10, 0.015, 0.1, 0.25), c(0.9, 0.50, 0.08, 1.0, 5.0), "normal", experiment_description="generated_lhc_set")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_type="LHC",experiment_id=1)
## Note the above has an optional date argument if you don't want to use today's date
## Route 2: Already have an existing sample and want to add it to the database
add_existing_lhc_sample_to_database(dblink, read.csv("/home/kja505/Dropbox/RoboCalc/LHC_Params.csv",header=T), experiment_description="original ppsim lhc dataset")

#### 2: Robustness Sampling
parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
baseline<- c(0.3, 0.2, 0.04, 0.60, 1.0)
minvals <- c(0.10, 0.10, 0.015, 0.1, 0.25)
maxvals <- c(0.9, 0.50, 0.08, 1.0, 5.0)
incvals <- c(0.1, 0.05, 0.005, 0.05, 0.25)
generate_robustness_set_in_db(dblink,parameters, baseline, minvals, maxvals, incvals, experiment_id=NULL, experiment_description="PPSim Robustness")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_type="Robustness",experiment_id=1)


#### 3: eFAST Sampling
parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
num_samples<-65
num_curves<-3
minvals <- c(0.10, 0.10, 0.015, 0.1, 0.25,1)
maxvals <- c(0.9, 0.50, 0.08, 1.0, 5.0,2)
generate_efast_set_in_db(dblink, parameters, num_samples, minvals, maxvals, num_curves, experiment_id=NULL, experiment_description="PPSim eFAST2")
download_sample_as_csvfile("/home/kja505/Desktop/", dblink, experiment_type="eFAST",experiment_id=3)

## Or can add an existing generated set to the database
parameter_set_path<-"/home/kja505/Downloads/Spartan_Tutorial_Data/eFAST_Spartan2"
num_curves<-3
add_existing_efast_sample_to_database(dblink, parameter_set_path, parameters, num_curves,experiment_description="Original PPSim eFAST")


#### 4: Adding LHC Results to Database
## Route 1: From Spartan 2, all results can be provided in a single CSV file - this method processes that file and puts all results in the DB
## In this case, we add the parameters from the tutorial set, don't generate them, such that the parameters can tie up with the results
parameters<-c("thresholdBindProbability","chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
add_existing_lhc_sample_to_database(dblink, read.csv("~/Downloads/Spartan_Tutorial_Data/LHC_Spartan2/Tutorial_Parameters_for_Runs.csv",header=T), experiment_description="original ppsim lhc dataset")
# Now add the results for that experiment
experiment_id<-1 # Could have also added by description and date - these removed as default to NULL if ID specified
add_lhc_and_robustness_sim_results_from_csv_file(dblink, "~/Downloads/Spartan_Tutorial_Data/LHC_Spartan2/LHC_AllResults.csv", parameters, measures, "LHC", experiment_id)
# Now analyse the replicates to create a summary result
summarise_replicate_lhc_runs(dblink, measures, experiment_id)
# Now we have the data in a format that spartan can process - so we'll do the analysis
generate_lhc_analysis(dblink, parameters, measures, experiment_id=1)
# Graph an experiment - the graphs are never stored in the database, but we provide methods to graph for an experiment_id
measure_scale<-c("Velocity","Displacement")
output_directory<-"~/Desktop/"
graph_lhc_analysis(dblink, parameters, measures, measure_scale, output_directory, experiment_id=1, output_type=c("PDF"))


#### 5: Adding eFAST Results to Database
## CSV file:
# In this case, we add the parameters from the tutorial set, don't generate them, such that the parameters can tie up with the results
parameter_set_path<-"/home/kja505/Downloads/Spartan_Tutorial_Data/eFAST_Spartan2"
num_curves<-3
add_existing_efast_sample_to_database(dblink, parameter_set_path, parameters, num_curves,experiment_description="Original PPSim eFAST")
# Now add the results for this experiment
experiment_id<-1 # Could have also added by description and date - these removed as default to NULL if ID specified
add_efast_sim_results_from_csv_files(dblink, parameter_set_path, parameters, measures, num_curves, experiment_id)
# Now we can create summary stats from the replicates:
summarise_replicate_efast_runs(dblink, parameters, measures, experiment_id)
# Now do the eFAST Analysis


#### 6: Adding Robustness Results to Database
## CSV File:
# In this case, we add the parameters from the tutorial set, don't generate them, such that the parameters can tie up with the results
parameter_set_path<-"/home/kja505/Downloads/Spartan_Tutorial_Data/OAT_Spartan2/CSV_Structured/OAT_Medians.csv"
# Only two parameters in this test
parameters<-c("chemoUpperLinearAdjust","chemoLowerLinearAdjust")
add_existing_robustness_sample_to_database(dblink, parameter_set_path, parameters, experiment_description="Original PPSim Robustness")
# Now add the results for this experiment:
experiment_id<-1
add_lhc_and_robustness_sim_results_from_csv_file(dblink,parameter_set_path, parameters, measures, "Robustness", experiment_id)
# Now create summary stats from these replicates
summarise_replicate_robustness_runs(dblink,parameters,measures,experiment_id)


dbDisconnect(dblink)


# Analysis settings
FILEPATH<-"/home/kja505/Documents/ppsim_robochart_refactor_runs/"
MEASURE_SCALE <- c("microns/min", "microns")
NUMRUNSPERSAMPLE <- 500
NUMSAMPLES<-500
RESULTSFILENAME <- "trackedCells_Close_12.0to15.0.csv"
ALTERNATIVEFILENAME <- NULL
OUTPUTCOLSTART <- 10
OUTPUTCOLEND <- 12
LHC_ALL_SIM_RESULTS_FILE <- "Hour12_Close/LHC_AllResults_12.csv"
LHC_PARAM_CSV_LOCATION <- "LHC_Params.csv"
LHCSUMMARYFILENAME <- "Hour12_Close/LHC_Summary_12.csv"
CORCOEFFSOUTPUTFILE <- "Hour12_Close/LHC_corCoeffs_12.csv"
TIMEPOINTS<-NULL; TIMEPOINTSCALE<-NULL

# Now run the analysis
run_results <- lhc_process_sample_run_subsets(FILEPATH, LHC_PARAM_CSV_LOCATION, parameters, NUMSAMPLES,
                                              NUMRUNSPERSAMPLE, measures, RESULTSFILENAME, ALTERNATIVEFILENAME, OUTPUTCOLSTART,
                                              OUTPUTCOLEND, LHC_ALL_SIM_RESULTS_FILE,write_csv_file=FALSE)















#### TUTORIAL SCRIPTS


# optional - confirms we connected to the database
dbListTables(storiesDb)

# Assign variables
entryTitle <- "THE LOST LUSITANIA."
entryPublished <- "21 MAY 1916"
#convert the string value to a date to store it into the database
entryPublishedDate <- as.Date(entryPublished, "%d %B %Y")
entryUrl <- "http://newspapers.library.wales/view/4121281/4121288/94/"
searchTermsSimple <- "German+Submarine"

# Create the query statement
query<-paste(
  "INSERT INTO tbl_newspaper_search_results (
  story_title,
  story_date_published,
  story_url,
  search_term_used)
  VALUES('",entryTitle,"',
  '",entryPublishedDate,"',
  LEFT(RTRIM('",entryUrl,"'),99),
  '",searchTermsSimple,"')",
  sep = ''
  )

# optional - prints out the query in case you need to troubleshoot it
print(query)

#execute the query on the storiesDb that we connected to above.
rsInsert <- dbSendQuery(storiesDb, query)

#disconnect to clean up the connection to the database
dbDisconnect(storiesDb)


# Populate database with a CSV file
#R needs a full path to find the settings file
rmysql.settingsfile<-"/home/kja505/Documents/sql_settings/newspaper_search_results.cnf"

rmysql.db<-"newspaper_search_results"
storiesDb<-dbConnect(RMySQL::MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)
sampleGardenData <- read.csv(file="/home/kja505/Documents/spartan-db/sample-data-allotment-garden.csv", header=TRUE, sep=",")

dbWriteTable(storiesDb, value = sampleGardenData, row.names = FALSE, name = "tbl_newspaper_search_results", append = TRUE )

# Now do the same for submarine
sampleSubmarineData <- read.csv(file="/home/kja505/Documents/spartan-db/sample-data-submarine.csv", header=TRUE, sep=",")

dbWriteTable(storiesDb, value = sampleSubmarineData, row.names = FALSE, name = "tbl_newspaper_search_results", append = TRUE )

#disconnect to clean up the connection to the database
dbDisconnect(storiesDb)

### Querying the database and making a plot

# Query the database and make a plot
rmysql.settingsfile<-"/home/kja505/Documents/sql_settings/newspaper_search_results.cnf"

rmysql.db<-"newspaper_search_results"
storiesDb<-dbConnect(RMySQL::MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)

searchTermUsed="German+Submarine"
# Query a count of the number of stories matching searchTermUsed that were published each month
query<-paste("SELECT (
             COUNT(CONCAT(MONTH(story_date_published),' ',YEAR(story_date_published)))) as 'count'
             FROM tbl_newspaper_search_results
             WHERE search_term_used='",searchTermUsed,"'
             GROUP BY YEAR(story_date_published),MONTH(story_date_published)
             ORDER BY YEAR(story_date_published),MONTH(story_date_published);",sep="")

rs = dbSendQuery(storiesDb,query)
dbRows<-dbFetch(rs)
#Put the results of the query into a time series
qts1 = ts(dbRows$count, frequency = 12, start = c(1914, 8))
#Plot the qts1 time series data with line width of 3 in the color red.
plot(qts1, lwd=3,col = "red",
     xlab="Month of the war",
     ylab="Number of newspaper stories",
     main=paste("Number of stories in Welsh newspapers matching the search terms listed below.",sep=""),
     sub="Search term legend: Red = German+Submarine. Green = Allotment And Garden.")

searchTermUsed="AllotmentAndGarden"
#Query a count of the number of stories matching searchTermUsed that were published each month
query<-paste("SELECT (
             COUNT(CONCAT(MONTH(story_date_published),' ',YEAR(story_date_published)))) as 'count'
             FROM tbl_newspaper_search_results
             WHERE search_term_used='",searchTermUsed,"'
             GROUP BY YEAR(story_date_published),MONTH(story_date_published)
             ORDER BY YEAR(story_date_published),MONTH(story_date_published);",sep="")

rs = dbSendQuery(storiesDb,query)
dbRows<-dbFetch(rs)
#Put the results of the query into a time series
qts2 = ts(dbRows$count, frequency = 12, start = c(1914, 8))
#Add this line with the qts2 time series data to the the existing plot
lines(qts2, lwd=3,col="darkgreen")

dbDisconnect(storiesDb)
