library(rmarkdown)
library(dplyr)
library(lubridate)
#library(RPostgreSQL)
#library(jsonlite)

# # get a driver
# drv <- dbDriver("PostgreSQL")
# 
# # database parameters
# dbname = "ce_db"
# host = "---"
# port = ---
# user = "---"
# password = "---"
# 
# # creates a connection to the postgres database
# # note that "conn" will be used later in each connection to the database
# conn <- dbConnect(drv, dbname = dbname,
#                   host = host, port = port,
#                   user = user, password = password)

# get tables from the database
#property <- dbGetQuery(conn, "SELECT * FROM tblProperty")

# pulling in CSVs ------
field_data <- read.csv("MOCK_DATA.csv", header=TRUE, sep=",")

# data cleaning -----

#### DATA INTERPRETATION PROBLEM
## It appears that EventDate is the date a case was opened
## And ResolvedDate is when it's closed
## But there's missingness in the ResolvedDate
## And also does not appear to have a date for
## follow visits
## dataframe below is for figuring out differnce between
## ComplianceDate, DateResolved, and how they connect to EventDate
# testingDates <- muni.data %>%
#   select(EventID, EventDate, ComplianceDate, DateResolved, OverallStatus)

# # close the connection
# RPostgreSQL::postgresqlCloseConnection(conn)
# 
# # setup logging
# flog.appender(appender.file("logs/make_reports.log"), name="log")
# flog.threshold(DEBUG, name="log")


# Generate reports -------

generateReport <- function(disasterid, locationid, field_data){
  #' Generates a report and stores it as the location with .pdf
  #' appended in the /output directory for that disaster id.
  #' @param disasterid     code for particular disaster
  #' @param locationid     id for specific location
  #' @param field_data      data input from field via app
  require(lubridate)
  require(rmarkdown)
  require(dplyr)
  
  # only make the report if it doesn't already exist
  if(!file.exists(paste("output/", disasterid, locationid, ".html", sep=""))){
    rmarkdown::render("template_individual.rmd",
                      #output_format = pdf_document,
                      output_file = paste(disasterid, "_", locationid, ".html", sep=""),
                      #output_file = paste(disasterid, "_", locationid, ".pdf", sep=""),
                      output_dir = paste("ReportOutput/", disasterid, sep=""),
                      runtime = "static",
                      envir = new.env(),
                      intermediates_dir = "temp",
                      params=list(
                        disasterid = disasterid,
                        locationid = locationid,
                        field_data = field_data
                      )
    )
  }
}



makeAllReports <- function(disasterid_selected, field_data){
  #' Calls generateReport for each location
  #' @param disasterid     code for particular disaster
  #' @param locationid     id for specific location
  #' @param field_data      data input from field via app
  
  for(id_selected in field_data$id[field_data$disaster_id == disasterid_selected]){
    generateReport(disasterid = disasterid_selected, locationid = id_selected, field_data)
  }
}

# make all reports
###### NEED TO FIGURE OUT HOW TO RUN FOR THIS DISASTER, NOT PRIOR?
makeAllReports(disasterid, field_data)
