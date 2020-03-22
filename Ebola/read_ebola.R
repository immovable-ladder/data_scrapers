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

ebolaDataMaster <- read_sheet(drive_get(id = "EXAMPLE SHEET ID" 
#                                team_drive = "TEAM DRIVE NAME"
                                )) %>% 
  mutate_at(vars(VARNAMES), unlist)

# Find unique titles of documents that have already been processed
nowFiles <- unique(ebolaDataMaster$FileName)

# Locate new files that haven't been processed
driveFiles <- drive_ls(path = "FOLDER WHERE FILES ARE STORED")
driveFiles <- driveFiles[which(driveFiles$name != "EbolaData_pdfScraped_MASTER"),]
newFiles <- driveFiles[!(driveFiles$name %in% nowFiles),]

##############################################################################
##########################################DATA PROCESSING

# if there are new files, loop over all files in list and read data
## the text strings being looked for are incredibly convoluted because the files are convoluted, don't @ me
if(nrow(newFiles) > 0) {
  for (i in 1:nrow(newFiles)) {
    # read a new pdf file from drive to temp file
    drive_download(drive_get(id = newFiles$id[i], 
                              team_drive = "TEAM DRIVE NAME"),
                              path = "LOCAL PATH",
                              overwrite = T)
    
    # parse text
    text <- pdf_text("TempFile location") %>%
      strsplit(., "\n", fixed = TRUE) %>%
      unlist()
    
    #find report date and filename
    date <- sub(".*Activités du ", "\\1", text[grep("Activités du", text)])[1]
    file_name <- newFiles$name[i]
    
    #isolate stop and start elements for target data
    start <- grep("Kalunguta", text)
    stop <- grep("onfirmés et probables MVE", text)
    data <- text[(start[1]):(stop[1] - 1)]
    
    #remove leading whitespace
    data <- trimws(data, "l")
    
    #clean up text strings that shouldn't be there
    data <- gsub("21 derniers|Sud Kivu|d'Isolement le Dernier Cas|Délai Moyen Jours Depuis|derniers|Confirmé", "", data)
    data <- gsub("s d'Isolement|le Dernier|Confirmé|Cumulatif|jours|Province Zone de Santé|Cas|Confirmés", "", data)
    data <- gsub("Délai|Moyen|Jours|Depuis|21 derniers|Nord Kivu|Ituri|Province|Zone de Santé", "", data)
    
    #pull out orphan numbers into a vector to deal with later
    orphans <- data[grep("^\\d", data)]
    data <- data[!data %in% orphans]
    
    #change delimiter from massive whitespace to _
    data <- gsub("  +", "_", data)
    
    #get rid of province names and elements in the vector
    data <- data[!data %in% c("", "_", "s",
                              "La partie de l'image avec l'ID de relation rId12 n'a pas été trouvé dans le fichier.")]
    
    #split multi-district strings into individual districts
    splits <- data[grep("_[a-zA-Z]", data)]
    new_splits <- unlist(strsplit(splits, split = "_+(?=[A-Z])", perl = TRUE))
    data <- c(data[!data %in% splits], new_splits)
    
    #delete Total rows
    totals <- data[grep("^Total", data)]
    data <- data[!data %in% totals]
      
    #convert to df by splitting along "_" characters
    df <- data.frame()
    df <- colsplit(data, pattern = "_", names = c("zone_de_sante", "cas_confirmes",
                                                  "var1", "var2"))
    
    #clean up data where data for "days to close case" went into the "time to isolate" column
    df <- df %>% 
      mutate(delai_moyen_dIsolement = ifelse(grepl(',',var1), var1, NA),
             jours_depuis_ledernier_cas_confirme = ifelse(!grepl(',',var1), var1, var2)) %>% 
      select(-var1, -var2)
  
    #add orphan data to the bottom in a new row
    df <- rbind(df, data.frame("zone_de_sante" = "Unknown", 
                               "cas_confirmes" = NA, 
                               "delai_moyen_dIsolement" = NA,
                               "jours_depuis_ledernier_cas_confirme" = orphans))
    
    # check if there are blank values and fill with NA
    df$jours_depuis_ledernier_cas_confirme <- trimws(df$jours_depuis_ledernier_cas_confirme)
    df <- df %>% mutate_if(is.character, funs(na_if(.,""))) 
      
    #add identifiers from above
    df$Date <- date
    df$FileName <- file_name
      
    #bind to existing data
    ebolaDataMaster <- rbind(ebolaDataMaster, df)
  }
  
  # write new updated dataset to local file
  write.csv(ebolaDataMaster, "./EbolaData_pdfScraped_MASTER.csv", row.names = F)
  
  # publish updated file to Drive
  drive_update(file = "SAME SHEET ID AS ABOVE",
               media = "./EbolaData_pdfScraped_MASTER.csv",
               type = "spreadsheet",
               overwrite = T
  )
}

##############################################################################