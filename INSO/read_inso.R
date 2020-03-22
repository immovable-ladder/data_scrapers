library(dplyr)
library(pdftools)
library(reshape2)
library(stringr)
library(magrittr)
library(openssl)
library(httr)
library(googledrive)
library(googlesheets4)

# setwd("~/Documents/RWorkDir/ebola_pdfs")

# authenticate yourself with google sheets
# drive_auth(
#   email = gargle::gargle_oauth_email(),
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/drive",
#   cache = TRUE
# )

#get INSO Master sheet
  insoDataMaster <- as.data.frame(read_sheet(drive_get(id = "1v-ckh_dyAfkIssFn4UM9OMgJsLLDqjLx803eD7QGczo" )))
  insoDataMaster$Heure <- format(insoDataMaster$Heure, "%I:%M %p")
  insoDataMaster$Latitude <- as.numeric(insoDataMaster$Latitude)
  insoDataMaster$Longitude <- as.numeric(insoDataMaster$Longitude)

#load locations data
#locations <- as.data.frame(read_sheet(drive_get(id = "1o7uex23bB_1LhfdcAs4zzYudKNk_ahbRkTlJO0olx3U")))
  locations <- readRDS("locationsDRC.RDS") %>% 
    select(PROVINCE, TERRITOIRE, LOCALITE, "Latitude" = LATITUDE, "Longitude" = LONGITUDE)

# Find unique titles of documents that have already been processed
  nowFiles <- unique(insoDataMaster$scraped_file_name)

# Locate new files that haven't been processed
  driveFiles <- drive_ls(path = "https://drive.google.com/drive/folders/1ypOS_cCe-6G1lJh9nh23gGCJucqA_YHW")
  driveFiles <- driveFiles[grep("INSO ALERT", driveFiles$name),]
  newFiles <- driveFiles[!(driveFiles$name %in% nowFiles),]

#For population time of day column
  # create breaks for time of day
  breaks <- c(0, 6, 12, 18, 23, 24)
  # labels for the breaks
  labels <- c("Night", "Morning", "Afternoon", "Evening", "Night")
  
# create empty df for temp data
  df <- insoDataMaster[0,1:12]

##############################################################################
##########################################DATA PROCESSING

# if there are new files, loop over all files in list and read data
if(nrow(newFiles) > 0) {
  for (i in 1:nrow(newFiles)) {
    # read a new pdf file from drive to temp file
      drive_download(drive_get(id = newFiles$id[i], 
                                team_drive = "Technology for Development"),
                                path = "/Users/hannacamp/Documents/RWorkDir/ebola_pdfs/tempFile",
                                overwrite = T)
    
    # read text
      text <- read.delim("/Users/hannacamp/Documents/RWorkDir/ebola_pdfs/tempFile", stringsAsFactors = F)
      names(text) <- "text"
    
    # find report date, time, and filename
      x <- text[grep("DATE & HORAIRE|DATE & TIME", text$text)+1,]
      #order columns day month year
      date <- unlist(strsplit(x, split = " "))[1]
      annee <- lubridate::year(as.Date(date, format = "%d/%m/%Y"))
      mois <- lubridate::month(as.Date(date, format = "%d/%m/%Y"))
      jour <- lubridate::day(as.Date(date, format = "%d/%m/%Y"))
      
      #convert military time to standard
      heure <- as.POSIXct(paste(paste(annee, mois, jour, sep = "-"), unlist(strsplit(x, split = " "))[2]))
      heure <- format(heure, "%I:%M %p")
      
      #calculate time of day based on military time
      time_of_day <- cut(x = lubridate::hour(lubridate::hm(unlist(strsplit(x, split = " "))[2])), 
                         breaks = breaks, 
                         labels = labels, 
                         include.lowest = TRUE)
    
    # find province, territoire, village, road
      full_lieu <- paste(text[(grep("LIEU|LOCATION", text$text)+1):(grep("TYPE D'INCIDENT|INCIDENT TYPE", text$text)-1),], collapse = " ")
      full_lieu <- gsub("<https:\\/\\/www.google.*","", full_lieu)
      x <- trimws(strsplit(full_lieu, split = ",")[[1]])
      
      province <- unlist(x)[2]
      territoire <- x[grep("Territoire|territoire", x)]
        territoire <- ifelse(length(territoire) > 0, territoire, NA)
        territoire <- trimws(gsub("Territoire|territoire| d'| de ", "", territoire))
      village <- x[grep("Village|village|ville|localite", x)]
        village <- ifelse(length(village) > 0, village, NA)
        village <- trimws(gsub("Village|village|ville| de | d'", "", village))
      axe <- x[grep("axe|av ", x)]
        axe <- ifelse(length(axe) > 0, axe, NA)
    
    # find file name
      file_name <- newFiles$name[i]
    
    # find description of event
    ##INFORMATION
      start <- grep("INFORMATION", text$text)
      stop <- grep("RECOMMANDATION ACTUALISÉE|ACTION RECOMMANDÉE|UPDATED ADVICE", text$text) - 1
      description <- paste(text[(start[1]):(stop[1]),], collapse = " ")
    
    ##check if RAPPORTS INITIAUX exists and scrape if it does
      x <- grep("RAPPORTS INITIAUX|INITIAL REPORT", text$text)
      
      if(length(x) > 0) {
        start <- grep("RAPPORTS INITIAUX|INITIAL REPORT", text$text)
        stop <- grep("PUBLIÉ PAR|ISSUED BY", text$text) - 1
        y <- paste(text[(start[1]):(stop[1]),], collapse = " ")
        description <- paste(description, y, sep = " ")
      }
      
    #convert to a df row
      row <- data.frame("Annee" = annee,
                       "Mois" = mois,
                       "Jour" = jour,
                       "Heure" = heure,
                       "Time_of_Day" = time_of_day,
                       "Province" = province,	
                       "Territoire" = territoire,	
                       "Village" = village,	
                       "Axe" = axe,	
                       "Lieu_Complete" = full_lieu,
                       "Description" = description,	
                       "scraped_file_name" = file_name,
                       stringsAsFactors = FALSE
                       )
      
      #bind to temp data frame
      df <- dplyr::bind_rows(df, row)
  }

  #left join new locations with locations file
  df <- left_join(df, locations, 
                  by = c("Province" = "PROVINCE", "Territoire" = "TERRITOIRE", "Village" = "LOCALITE"))
    
  #bind to master
  insoDataMaster <- dplyr::bind_rows(insoDataMaster, df)
  
  # write new updated dataset to file
  write.csv(insoDataMaster, "./INSO_Scraping_MASTER.csv", row.names = F)
  
  # publish updated file to Drive
  drive_update(file = "https://docs.google.com/spreadsheets/d/1v-ckh_dyAfkIssFn4UM9OMgJsLLDqjLx803eD7QGczo",
               media = "./INSO_Scraping_MASTER.csv"
               # path = as_id("https://drive.google.com/drive/u/0/folders/1cM2Cb_PBFPlzOvbl8Ki_lZt4vsYFfZWR"),
               # type = "spreadsheet",
               # overwrite = T
    )
}

##############################################################################