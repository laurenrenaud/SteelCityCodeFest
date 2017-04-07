library(rmarkdown)
library(dplyr)
library(lubridate)
library(readr)
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
field_data <- readr::read_csv("Codefest Data.csv")

recent_disaster <- unique(field_data$disaster_id[field_data$date == max(field_data$date)])

# Generate reports -------

generateReportHTML <- function(disasterid, locationid, field_data){
  #' Generates a report and stores it as the location with .pdf
  #' appended in the /output directory for that disaster id.
  #' @param disasterid     code for particular disaster
  #' @param locationid     id for specific location
  #' @param field_data      data input from field via app
  require(rmarkdown)
  require(dplyr)
  
  # only make the report if it doesn't already exist
  if(!file.exists(paste("output/", disasterid, locationid, ".html", sep=""))){
    rmarkdown::render("template_individual_html.rmd",
                      #output_format = pdf_document,
                      output_file = paste("Disaster", disasterid, "_ReportID", locationid, ".html", sep=""),
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


generateReportPDF <- function(disasterid, locationid, field_data){
  #' Generates a report and stores it as the location with .pdf
  #' appended in the /output directory for that disaster id.
  #' @param disasterid     code for particular disaster
  #' @param locationid     id for specific location
  #' @param field_data      data input from field via app
  require(rmarkdown)
  require(dplyr)
  
  # only make the report if it doesn't already exist
  if(!file.exists(paste("output/", disasterid, locationid, ".pdf", sep=""))){
    rmarkdown::render("template_individual_pdf.rmd",
                      #output_format = pdf_document,
                      output_file = paste("Disaster", disasterid, "_ReportID", locationid, ".pdf", sep=""),
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



generateConsolidatedReportPDF <- function(disasterid, field_data){
  #' Generates a report and stores it as the location with .pdf
  #' appended in the /output directory for that disaster id.
  #' @param disasterid     code for particular disaster
  #' @param field_data      data input from field via app
  require(rmarkdown)
  require(dplyr)
  
  # only make the report if it doesn't already exist
  if(!file.exists(paste("output/Consolidated_", disasterid, ".pdf", sep=""))){
    rmarkdown::render("template_individual_pdf.rmd",
                      #output_format = pdf_document,
                      output_file = paste("Consolidated_Disaster", disasterid, ".pdf", sep=""),
                      output_dir = paste("ReportOutput/", disasterid, sep=""),
                      runtime = "static",
                      envir = new.env(),
                      intermediates_dir = "temp",
                      params=list(
                        disasterid = disasterid,
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
    generateReportHTML(disasterid = disasterid_selected, locationid = id_selected, field_data)
    generateReportPDF(disasterid = disasterid_selected, locationid = id_selected, field_data)
  }
}

# make all reports
makeAllReports(recent_disaster, field_data)

# make consolidated report

