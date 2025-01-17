setup_db_link <-function()
{
  rmysql.settingsfile<-"~/Documents/sql_settings/spartanDB.cnf"
  rmysql.db<-"spartan_ppsim"
  dblink<-RMySQL::dbConnect(RMySQL::MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)
}

close_db_link <- function(dblink)
{
  tryCatch({
    RMySQL::dbDisconnect(dblink)
  }, error = function(e)
    {
    message(paste("Error closing database link. Error message generated: \n",e,sep=""))
  })
}
