setup_db_link <-function()
{
  rmysql.settingsfile<-"/home/kja505/Documents/sql_settings/newspaper_search_results.cnf"
  rmysql.db<-"spartan_ppsim"
  dblink<-RMySQL::dbConnect(RMySQL::MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)
}

close_db_link <- function(dblink)
{
  RMySQL::dbDisconnect(dblink)
}
