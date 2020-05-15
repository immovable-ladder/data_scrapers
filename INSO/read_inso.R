library(dplyr); library(pdftools); library(reshape2); library(stringr); library(magrittr)
library(openssl); library(httr); library(googledrive); library(googlesheets4)

# authenticate yourself with google sheets
# drive_auth(
#   email = gargle::gargle_oauth_email(),
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/drive",
#   cache = TRUE
# )

#get INSO Master sheet
  insoDataMaster <- as.data.frame(read_sheet(drive_get(id = "EXAMPLE SHEET ID" ),
                                             na = "NA",
                                             col_types = "cnnncccccccnnc")) %>% 
    mutate_at(vars(Annee, Mois, Jour, Date), unlist)

#load locations data
  locations <- readRDS("locationsDRC.RDS") %>% 
    select(PROVINCE, TERRITOIRE, LOCALITE, "Latitude" = LATITUDE, "Longitude" = LONGITUDE)

# Locate all drive files
  driveFiles <- drive_ls(path = "DRIVE FOLDER WHERE FILES ARE STORED")
  driveFiles_detail <- as.data.frame(do.call(rbind, driveFiles$drive_resource)) %>% 
    select(name, id, createdTime) %>% 
    mutate_at(vars(name, id, createdTime), unlist) %>% 
    mutate(Date = as.Date(createdTime))
  
# Locate alert files within last 3 day window  
  newFiles <- driveFiles_detail[grep("INSO ALERT|INSO RAPPORT|INSO UPDATE", driveFiles_detail$name),] %>% 
    filter(Date > Sys.Date()-3)
  
# Locate new Hebdomadaire files uploaded within the last 3 days
  newHebdo <- driveFiles_detail[grep("Liste_hebdomadaire|Liste hebdomadaire", driveFiles_detail$name),] %>% 
    filter(Date > Sys.Date()-3)

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
      
    # check that the time/date formatting is recognizable
      if(length(unlist(strsplit(x, split = " "))) > 2) {
        
        # if not input NA, and a fake time for formatting reasons
        annee <- NA
        mois <- NA
        jour <- NA
        time_of_day <- NA
        heure <- "12:00 AM"
        
      } else {
        
        # find report date, time, and time of day
        date <- unlist(strsplit(x, split = " "))[1]
        annee <- lubridate::year(as.Date(date, format = "%d/%m/%Y"))
        mois <- lubridate::month(as.Date(date, format = "%d/%m/%Y"))
        jour <- lubridate::day(as.Date(date, format = "%d/%m/%Y"))
        
        # convert military time to standard
        heure <- as.POSIXct(paste(paste(annee, mois, jour, sep = "-"), unlist(strsplit(x, split = " "))[2]))
        heure <- format(heure, "%I:%M %p")
        heure <- gsub("^0", "", heure)
        
        # calculate time of day based on military time
        time_of_day <- cut(x = lubridate::hour(lubridate::hm(unlist(strsplit(x, split = " "))[2])), 
                           breaks = breaks, 
                           labels = labels, 
                           include.lowest = TRUE)
        
        # end date/time if statement
      }

      # find full location string
        full_lieu <- paste(text[(grep("LIEU|LOCATION", text$text)+1):(grep("TYPE D'INCIDENT|INCIDENT TYPE", text$text)-1),], collapse = " ")
        full_lieu <- gsub("<https:\\/\\/www.google.*","", full_lieu)
        x <- trimws(strsplit(full_lieu, split = ",")[[1]])
      
      # assign province, territoire, village, road
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
        stop <- grep("RECOMMANDATION ACTUALISÉE|ACTION RECOMMANDÉE|UPDATED ADVICE|ANALYSE|ANALYSISRAPPORTS INITIAUX", text$text) - 1
        
      if(length(start) > 0 && length(stop) > 0) {
        description <- paste(text[(start[1]):(stop[1]),], collapse = " ")
      } else {
        description <- "Error in scraping, please check with script maintainer"
      }
        
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
                         "Date" = paste(jour, mois, annee, sep = "-"),
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
        df <- bind_rows(df, row)
        
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
                                 skip = 10,
                                 col_types = c("numeric", "text", "numeric", "date", rep(text, 12)))
      
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
      data$Heure <- gsub("^0", "", data$Heure)
      
      # correct from month name to month number
      data$Mois <- match(data$Mois, toupper(month.abb))
      
      # add Date column
      data <- data %>% mutate(Date = paste(Jour, Mois, Annee, sep = "-"))
       
      # clean up Province column
      data$Province <- gsub("NORD_KIVU", "Nord Kivu", data$Province)
      data$Province <- gsub("SUD_KIVU", "Sud Kivu", data$Province)
      data$Province <- gsub("_", "-", data$Province)
      
      # convert Province, Territoire, and Village columns to Title Case
      data$Province <- stringr::str_to_title(data$Province)
      data$Territoire <- stringr::str_to_title(data$Territoire)
      data$Village <- stringr::str_to_title(data$Village)
      data$Axe <- stringr::str_to_title(data$Axe)
      
      #bind to temp data frame
      df <- dplyr::bind_rows(df, data)
    }
  }
  
    #left join df with locations file
    df <- left_join(df, dplyr::distinct(locations, PROVINCE, TERRITOIRE, LOCALITE, .keep_all = T), 
                    by = c("Province" = "PROVINCE", 
                           "Territoire" = "TERRITOIRE", 
                           "Village" = "LOCALITE"))
    
    #bind to master and sort
    insoDataMaster <- dplyr::bind_rows(insoDataMaster, df) %>% 
      distinct() %>% 
      arrange(Annee, Mois, Jour) %>% 
      mutate(Date = paste(Jour, Mois, Annee, sep = "-")) %>% 
      select(Description, Annee, Mois, Jour, Date, Time_of_Day, Heure, Province,
             Territoire, Village, Axe, Latitude, Longitude, scraped_file_name)
    
    # write new updated dataset to file
    write.csv(insoDataMaster, "./INSO_Scraping_MASTER.csv", row.names = F)
    
    # publish updated file to Drive
    drive_update(file = "EXAMPLE SHEET ID",
                 media = "./INSO_Scraping_MASTER.csv")
}

##############################################################################