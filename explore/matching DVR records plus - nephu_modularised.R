#=========================================================================================================
# DVR not matched to contact records

rm(list=ls())
.libPaths(c("C:/Users/dwol3009/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.3/library" ))
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(lubridate)
library(fuzzyjoin)

# load in mobile cleaning function
LoadData_CRM_Location <- paste0("E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/DART/Data snapshots/RDS files/")
SitRepOrLatest <- "sitrep"
Files_required <- c("linelist", "Persons", "MonitoringLogging", "AllClusters", "ClusterSites")
source(paste0(LoadData_CRM_Location, "LoadData_CRM.R"))

linelist_used <- paste0(dataFolder, "linelist.txt")
DataUpdateAt <- file.info(linelist_used)$mtime

# Load in lists -----------------------------------------------------------

# input_location <- c("E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/5 PBI and STATA under development/Andrew E/PCC SCC LPHU Work/Input/")
output_location <- c("E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/5 PBI and STATA under development/Andrew E/PCC SCC LPHU Work/Output/")
date <- format(today(), "%Y%m%d")

#get managed clusters
nephu.managed <- get_clusters_for_phu( 'NEPHU', ClusterSites, AllClusters)

# get pp screened by these clusters
nephu.screening <- get_screening_from_clusters( clusters_managed, LineList, Persons )

# generate list of active contacts
our.list <- get_active_contacts( LineList, Persons)

nephu.screening.dedupe <- nephu.screening %>% distinct(CaseNumber, .keep_all = TRUE)

# (1) Match based on first name and mobile
firstname.mobile.join <- inner_join(our.list, nephu.screening.dedupe,
                                    by = c("FirstNameClean", "Aus_mobile"),
                                    suffix = c("_CONTACT", "_SCREEN"), na_matches = "never") %>%
#distinct(CaseNumber_SCREEN, .keep_all = TRUE) %>%
select(CaseNumber_CONTACT, CaseNumber_SCREEN, FirstName_CONTACT, FirstName_SCREEN, Surname_CONTACT, Surname_SCREEN, Mobile=Aus_mobile,
       DateOfBirth_CONTACT, DateOfBirth_SCREEN, ClusterName.x, Classification_CONTACT, dummy.id) %>%
mutate(join = "firstname.mobile.join")

nephu.screening.remaining <- nephu.screening.dedupe %>%
  anti_join(firstname.mobile.join %>%
              #distinct(CaseNumber_SCREEN, .keep_all = TRUE) %>%
              select(dummy.id), by = "dummy.id")

firstname.mobile.join <- firstname.mobile.join %>% distinct(CaseNumber_SCREEN, .keep_all = TRUE)


# (2) Match based on full name
fullname.join <- inner_join(our.list, nephu.screening.remaining,
                            by = c("FirstNameClean", "SurnameClean"),
                            suffix = c("_CONTACT", "_SCREEN"), na_matches = "never") %>%
  #distinct(CaseNumber_SCREEN, .keep_all = TRUE) %>%
  select(CaseNumber_CONTACT, CaseNumber_SCREEN, FirstNameClean, SurnameClean,
         Mobile_CONTACT, Mobile_SCREEN, DateOfBirth_CONTACT, DateOfBirth_SCREEN, ClusterName.x, Classification_CONTACT, dummy.id) %>%
  mutate(join = "fullname.join")

nephu.screening.remaining2 <- nephu.screening.remaining %>%
  anti_join(fullname.join %>%
              select(dummy.id), by = "dummy.id") %>%
  #distinct(CaseNumber, .keep_all = TRUE) %>%
  select(RecordID, CaseNumber, FirstName, Surname, Mobile, DateOfBirth, ClusterName.x, AddressLine1, AddressLine2)

fullname.join <- fullname.join %>% distinct(CaseNumber_SCREEN, .keep_all = TRUE)



# # (3) Match based on first name and mobile
# firstname.mobile.join <- inner_join(our.list, nephu.screening.remaining2,
#                                    by = c("FirstNameClean", "Aus_mobile"),
#                                    suffix = c("_CONTACT", "_SCREEN"), na_matches = "never") %>%
#   # distinct(CaseNumber_DH, .keep_all = TRUE) %>%
#   select(CaseNumber_CONTACT, CaseNumber_SCREEN, FirstName_CONTACT, FirstName_SCREEN, Surname_CONTACT, Surname_SCREEN, Mobile=Aus_mobile,
#          DateOfBirth_CONTACT, DateOfBirth_SCREEN, ClusterName.x, Classification_CONTACT, dummy.id) %>%
#   mutate(join = "firstname.mobile.join")
#
# nephu.screening.remaining3 <- nephu.screening.remaining2 %>%
#   anti_join(firstname.mobile.join %>%
#               select(dummy.id), by = "dummy.id") %>%
#   select(RecordID, CaseNumber, FirstName, Surname, Mobile, DateOfBirth, ClusterName.x, AddressLine1, AddressLine2)


#=========================================================================================================
# Create a blank workbook
output <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(output, "Matched - reject screening")
addWorksheet(output, "Name matched only - Call")
addWorksheet(output, "No match - Call")
#addWorksheet(output, "our.list")

# Write the data to the sheets
writeData(output, sheet = "Matched - reject screening", x = firstname.mobile.join)
writeData(output, sheet = "Name matched only - Call", x = fullname.join)
writeData(output, sheet = "No match - Call", x = nephu.screening.remaining2)
#writeData(output, sheet = "our.list", x = our.list)

# Export the file
saveWorkbook(output, paste0('output/', "nephu_dvr_", date, ".xlsx"), overwrite = T)
