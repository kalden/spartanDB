# R & MySQL tutorial
# https://programminghistorian.org/en/lessons/getting-started-with-mysql-using-r#open-mysql-workbench

## Main Script here:

library(RMySQL)
library(spartan)
# R needs a full path to find the settings file
rmysql.settingsfile<-"/home/kja505/Documents/sql_settings/newspaper_search_results.cnf"
rmysql.db<-"spartan_ppsim"
dblink<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)

# Simulation Settings
parameters<-c("chemoThreshold","chemoUpperLinearAdjust","chemoLowerLinearAdjust","maxVCAMeffectProbabilityCutoff","vcamSlope")
measures<-c("Velocity","Displacement")

# Set up the database:
create_experiments_table(dblink)
create_parameter_values_table(dblink,parameters)
create_simulation_results_table(dblink, measures)
create_analysed_results_table(dblink, measures)

# Two routes here: Can we add an existing parameter set to the database
experiment_id <- setup_experiment(dblink,"LHC","2018-08-07","original ppsim lhc dataset")
add_parameter_set_to_database(read.csv("/home/kja505/Dropbox/RoboCalc/LHC_Params.csv",header=T),experiment_id)
# Second, can we generate an lhc sample and add straight to the database. This time use the new function
experiment_date<-"2018-08-20"
generate_lhc_set_in_db(dblink, parameters, 500, c(0, 0.10, 0.10, 0.015, 0.1, 0.25), c(100, 0.9, 0.50, 0.08, 1.0, 5.0), "normal", experiment_description="generated_lhc_set")

# Robustness sampling
baseline<- c(0.3, 0.2, 0.04, 0.60, 1.0)
minvals <- c(0.10, 0.10, 0.015, 0.1, 0.25)
maxvals <- c(0.9, 0.50, 0.08, 1.0, 5.0)
incvals <- c(0.1, 0.05, 0.005, 0.05, 0.25)
generate_robustness_set_in_db(dblink,parameters, baseline, minvals, maxvals, incvals, experiment_id=NULL, experiment_description="PPSim Robustness")

#eFAST sampling
num_samples<-65
num_curves<-3
generate_efast_set_in_db(dblink, parameters, num_samples, minvals, maxvals, num_curves, experiment_id=NULL, experiment_description="PPSim eFAST")

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






dbDisconnect(dblink)








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
