library(dplyr)
library(pdftools)
library(reshape2)
library(stringr)
library(magrittr)
library(openssl)
library(httr)
library(googledrive)
library(googlesheets4)

# authenticate yourself with google sheets
# drive_auth(
#   email = gargle::gargle_oauth_email(),
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/drive",
#   cache = TRUE
# )

#get INSO Master sheet
  insoDataMaster <- as.data.frame(read_sheet(drive_get(id = "EXAMPLE SHEET ID" )))
  insoDataMaster$Heure <- format(insoDataMaster$Heure, "%I:%M %p")
  insoDataMaster$Latitude <- as.numeric(insoDataMaster$Latitude)
  insoDataMaster$Longitude <- as.numeric(insoDataMaster$Longitude)

#load locations data
  locations <- readRDS("locationsDRC.RDS") %>% 
    select(PROVINCE, TERRITOIRE, LOCALITE, "Latitude" = LATITUDE, "Longitude" = LONGITUDE)

# Find unique titles of documents that have already been processed
  nowFiles <- unique(insoDataMaster$scraped_file_name)

# Locate new files that haven't been processed
  driveFiles <- drive_ls(path = "DRIVE FOLDER WHERE FILES ARE STORED")
  driveFiles <- driveFiles[grep("INSO ALERT|INSO RAPPORT|INSO UPDATE", driveFiles$name),]
  newFiles <- driveFiles[!(driveFiles$name %in% nowFiles),]
  
# Locate new Hebdomadaire files that haven't been processed
  newHebdo <- driveFiles[grep("Liste_hebdomadaire", driveFiles$name),]
  newHebdo <- newHebdo[!(newHebdo$name %in% nowFiles),]

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

if (nrow(newFiles) > 0 | nrow(newHebdo) > 0) {
  
  # if there are new alert files, read data and add to df if yes
  if(nrow(newFiles) > 0) {
    for (i in 1:nrow(newFiles)) {
      # read a new pdf file from drive to temp file
        drive_download(drive_get(id = newFiles$id[i], 
                                  team_drive = "TEAM DRIVE NAME"),
                                  path = "tempFile path",
                                  overwrite = T)
      
      # read text
        text <- read.delim("tempFile path", stringsAsFactors = F)
        names(text) <- "text"
      
      # find report date & time
        x <- text[grep("DATE & HORAIRE|DATE & TIME", text$text)+1,]
      
      # order columns day month year
        date <- unlist(strsplit(x, split = " "))[1]
        annee <- lubridate::year(as.Date(date, format = "%d/%m/%Y"))
        mois <- lubridate::month(as.Date(date, format = "%d/%m/%Y"))
        jour <- lubridate::day(as.Date(date, format = "%d/%m/%Y"))
        
      # convert military time to standard
        heure <- as.POSIXct(paste(paste(annee, mois, jour, sep = "-"), unlist(strsplit(x, split = " "))[2]))
        heure <- format(heure, "%I:%M %p")
        
      # calculate time of day based on military time
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
  }
  
  # if there are new Hebdomadaire files, read data and add to df if yes
  if(nrow(newHebdo) > 0) {
    for (i in 1:nrow(newHebdo)) {
      # read a new sheet file from drive to local df
      drive_download(drive_get(id = newHebdo$id[i]),
                     path = "tempFile path",
                     overwrite = T)
      
      # read excel file
      data <- readxl::read_excel("tempFile path", 
                                 sheet = 1,
                                 skip = 10)
      
      # remove crappy first row and select columns we need
      data <- data[-1,] %>% 
        select(Description = `DESCRIPTION DE L'INCIDENT`, Annee = ANNEE, Mois = MOIS, Jour = JOUR, Heure = HEURE,
               Province = PROVINCE, Territoire = TERRITOIRE, Village = `VILLE/VILLAGE`, Axe = `LIEU EXACT`)
      
      # add scraped_file_name
      data$scraped_file_name <- newFiles$name[i]
      
      # add Time_of_Day
      data$Time_of_Day <- cut(x = as.numeric(format(data$Heure, format='%H')), 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = TRUE)
      
      data$Heure <- format(data$Heure, "%I:%M %p")
      
      # correct from month name to month number
      data$Mois <- match(data$Mois, toupper(month.abb))
      
      #bind to temp data frame
      df <- dplyr::bind_rows(df, data)
    }
  }
  
    #left join df with locations file
    df <- left_join(df, locations, 
                    by = c("Province" = "PROVINCE", 
                           "Territoire" = "TERRITOIRE", 
                           "Village" = "LOCALITE"))
    
    #bind to master and sort
    insoDataMaster <- dplyr::bind_rows(insoDataMaster, df) %>% 
      arrange(Annee, Mois, Jour)
    
    # write new updated dataset to file
    write.csv(insoDataMaster, "./INSO_Scraping_MASTER.csv", row.names = F)
    
    # publish updated file to Drive
    drive_update(file = "EXAMPLE SHEET ID",
                 media = "./INSO_Scraping_MASTER.csv")
}

##############################################################################