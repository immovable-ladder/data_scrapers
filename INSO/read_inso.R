library(dplyr); library(pdftools); library(reshape2)
library(stringr); library(magrittr); library(openssl)
library(httr); library(googledrive); library(googlesheets4)
library(lubridate)

# authenticate yourself with google sheets
drive_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/drive",
  cache = TRUE
)

#load locations data
  #locations <- readRDS("locationsDRC.RDS") %>%  
  #  select(PROVINCE, TERRITOIRE, LOCALITE, "Latitude" = LATITUDE, "Longitude" = LONGITUDE)

# Locate Drive folder files and relevant details 
  driveFiles <- drive_ls(path = "https://drive.google.com/drive/folders/1ypOS_cCe-6G1lJh9nh23gGCJucqA_YHW")
  driveFiles_detail <- as.data.frame(do.call(rbind, driveFiles$drive_resource)) %>% 
    select(name, id, createdTime) %>% 
    mutate_at(vars(name, id, createdTime), unlist) %>% 
    mutate(adjTime = substr(gsub("T", " ", createdTime), 1, 19)) %>% 
    mutate(Date = as.Date(createdTime, tz = "UTC"),
           DateTime = strptime(adjTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
    suppressWarnings()
  
# Locate Hebdomadaire files uploaded within the last 2 hours
  newHebdo <- driveFiles_detail[grep("Liste_hebdomadaire|Liste hebdomadaire|Liste_Hebdomadaire", driveFiles_detail$name),] %>% 
    filter(DateTime > as.POSIXlt(Sys.time(), tz = "UTC")-7200)
  
#For populating time of day column
  # create breaks for time of day
  breaks <- c(0, 6, 12, 18, 23, 24)
  # labels for the breaks
  labels <- c("Night", "Morning", "Afternoon", "Evening", "Night")
  
# create empty df for temp data
  df <- data.frame(Description=character(), Annee=numeric(), Mois=numeric(), Jour=numeric(),
                   Date=character(), Time_of_Day=character(), Heure=character(), Province=character(),
                   Territoire=character(), Village=character(), Axe=character(), Zone_de_Sante=character(),
                   Aire_de_Sante=character(), Latitude=numeric(), Longitude=numeric(), scraped_file_name=character())

##############################################################################
##########################################DATA PROCESSING

# if there are new files, loop over all files and update the master workbook
  
if (nrow(newHebdo) > 0) {
  
  # if there are new Hebdomadaire files, read data and add to temp df
    for (i in 1:nrow(newHebdo)) {
      # read a new sheet file from drive to local df
      drive_download(drive_get(id = newHebdo$id[i]),
                     path = "./tempFile.xlsx",
                     overwrite = T)
      
      # read excel file
      data <- readxl::read_excel("./tempFile.xlsx", 
                                 sheet = 1,
                                 skip = 12,
                                 col_types = c(rep("numeric", 3), "date", rep("text", 5), rep("numeric", 2), rep("text", 5)))
      
      #select columns we need
      data <-  select(data, Description = DESCRIPTION, Annee = ANNEE, Mois = MOIS, Jour = JOUR, Heure = HEURE,
               Province = PROVINCE, Territoire = TERRITOIRE, Village = `GROUPEMENT/VILLAGE/VILLE`, Axe = `LIEU EXACT`, 
               Latitude = LATITUDE, Longitude = LONGITUDE)
      
      # add scraped_file_name
      data$scraped_file_name <- newHebdo$name[i]
      
      # add Time_of_Day
      data$Time_of_Day <- cut(x = as.numeric(format(data$Heure, format='%H')), 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = TRUE)
      
      data$Heure <- format(data$Heure, "%I:%M %p")
      data$Heure <- gsub("^0", "", data$Heure)
      
      # check if time was not given and assign midnight if necessary to prevent errors
      data$Heure <- tidyr::replace_na(data$Heure, "12:00 AM")
      
      # correct from month name to month number
      #data$Mois <- match(data$Mois, toupper(month.abb))
      
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

  
  #merge df with locations file - NOTE: MAY NOT BE NEEDED ANYMORE
  #df <- dplyr::left_join(df, dplyr::distinct(locations, PROVINCE, TERRITOIRE, LOCALITE, .keep_all = T), 
  #                       by = c("Province" = "PROVINCE", 
  #                              "Territoire" = "TERRITOIRE", 
  #                              "Village" = "LOCALITE"))
  
  #remove any duplicates
  df <- distinct(df)
  
  #Update Master workbook by sheet, split out by province
  sheet_append("1Y_6CIwMjmTyCR9DxqJAyl9YeLRXTnx3OUooB6D9XKrs", filter(df, Province == "Ituri"), 
                "Ituri Province")
  sheet_append("1Y_6CIwMjmTyCR9DxqJAyl9YeLRXTnx3OUooB6D9XKrs", filter(df, Province == "Nord-Kivu"), 
               "North Kivu Province")
  sheet_append("1Y_6CIwMjmTyCR9DxqJAyl9YeLRXTnx3OUooB6D9XKrs", filter(df, Province == "Maniema"), 
               "Maniema Province")
  sheet_append("1Y_6CIwMjmTyCR9DxqJAyl9YeLRXTnx3OUooB6D9XKrs", filter(df, Province == "Sud-Kivu"), 
               "South Kivu Province")
  sheet_append("1Y_6CIwMjmTyCR9DxqJAyl9YeLRXTnx3OUooB6D9XKrs", filter(df, Province == "Tanganyika"), 
               "Tanganyika Province")
  sheet_append("1Y_6CIwMjmTyCR9DxqJAyl9YeLRXTnx3OUooB6D9XKrs", filter(df, Province == "Haut-Katanga"), 
               "Haut-Katanga Province")
}
  
##############################################################################