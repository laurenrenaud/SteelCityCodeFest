library(RPostgreSQL)
library(rmarkdown)
library(futile.logger)
library(dplyr)
library(jsonlite)
library(lubridate)

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
codeDetail <- read.csv("data/tblCodeEnforcement-Detail.csv", header=TRUE, sep=",")
codeEnforcement <- read.csv("data/tblCodeEnforcement.csv", header=TRUE, sep=",")
property <- read.csv("data/tblProperty.csv", header=TRUE, sep=",")
rentalPermit <- read.csv("data/tblRentalPermit.csv", header=TRUE, sep=",")
codeDescription <- read.csv("data/tblIssueCode.csv", header=TRUE, sep=",", fileEncoding="LATIN1")
muni.codes <- fromJSON("data/municode.json")
convert <- read.csv("data/parcels_xy_only.csv", header=TRUE)

# data cleaning -----


# generate necessary time variables -----
reportMonthDate <- floor_date(Sys.Date(), "month") - months(4)
yearMonthChar <- format(reportMonthDate, "%Y-%m")
monthName <- month.abb[month(reportMonthDate)]
yearChar <- as.character(year(reportMonthDate))


# Join dataframes ----
report.data 
# 
# # all closed cases
# closed <- muni.data %>%
#   group_by(EventID) %>%
#   filter(all(Resolved.[EventDate==max(EventDate)]))
# 
# # open cases
# open <- muni.data %>%
#   group_by(EventID) %>%
#   filter(any(!Resolved.[EventDate==max(EventDate)]))
# 
# # NA cases
# na.cases <- muni.data %>%
#   group_by(EventID) %>%
#   filter(any(is.na(Resolved.[EventDate==max(EventDate)])))
# 
# # this month's cases
# nextMonth <- reportMonthDate + months(1)
# monthCases <- muni.data %>%
#   group_by(EventID) %>%
#   filter(min(EventDate) > reportMonthDate, min(EventDate) < nextMonth)
# 
# monthOpen <- monthCases %>%
#   group_by(EventID) %>%
#   filter(any(!Resolved.[EventDate==max(EventDate)]))
# 
# monthClosed <- monthCases %>%
#   group_by(EventID) %>%
#   filter(all(Resolved.[EventDate==max(EventDate)]))
# 
# muni.year <- muni.data %>%
#   # first group by EventID and filter by the first (min) EventDate for a given 
#   # EventID is within the past year
#   group_by(EventID) %>%
#   filter(min(EventDate) > (reportMonthDate - months(12)))

#### DATA INTERPRETATION PROBLEM
## It appears that EventDate is the date a case was opened
## And ResolvedDate is when it's closed
## But there's missingness in the ResolvedDate
## And also does not appear to have a date for
## follow visits
## dataframe below is for figuring out differnce between
## ComplianceDate, DateResolved, and how they connect to EventDate
testingDates <- muni.data %>%
  select(EventID, EventDate, ComplianceDate, DateResolved, OverallStatus)

# # close the connection
# RPostgreSQL::postgresqlCloseConnection(conn)
# 
# # setup logging
# flog.appender(appender.file("logs/make_reports.log"), name="log")
# flog.threshold(DEBUG, name="log")


generateReport <- function(disasterid, locationid, property, codeEnforcement, codeDetail){
  #' Generates a family report and stores it as the family id with .docx
  #' appended in the /output directory.
  #' @param disasterid     code for particular disaster
  #' @param locationid     id for specific location
  #' @param property         Property dataframe
  #' @param codeEnforcement   codeEnforcement dataframe
  #' @param codeDetail         codeDetail dataframe
  require(lubridate)
  require(rmarkdown)
  require(dplyr)
  
  # only make the report if it doesn't already exist
  if(!file.exists(paste("output/", disasterid, locationid, ".pdf", sep=""))){
    rmarkdown::render("template_individual.rmd",
                      #output_format = pdf_document,
                      output_file = paste(disasterid, "_", locationid, ".pdf", sep=""),
                      output_dir = paste("ReportOutput/", disasterid, sep=""),
                      runtime = "static",
                      envir = new.env(),
                      intermediates_dir = "temp",
                      params=list(
                        disasterid = disasterid,
                        locationid = locationid
                        #codeEnforcement = codeEnforcement,
                        #codeDetail = codeDetail,
                        #month = month
                      )
    )
  }
}


makeAllReports <- function(month, muniname, property, codeEnforcement, codeDetail){
  #' Calls generateReport for each family that is eligable to recieve a report.
  #' @param month          string of month plus year of report
  #' @param municipality    municipal name
  #' @param property         Property dataframe
  #' @param codeEnforcement   codeEnforcement dataframe
  #' @param codeDetail         codeDetail dataframe
  
  for(municipality in muni.codes$muniname){
    generateReport(month, municipality, property, codeEnforcement, codeDetail)
  }
}

# make all reports
makeAllReports(yearMonthChar, muniname, property, codeEnforcement, codeDetail)
