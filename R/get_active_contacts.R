
get_active_contacts <- function( LineList, Persons) {


  LineList %>%
    left_join(MonLog, by = "RecordID") %>%
    filter( (Classification == "Confirmed" & ActiveFlag == "Active") |
              Classification == "Contact - active" |
              Classification == "Secondary contact - active" |
              #Classification == "Casual contact" & MonitoringStartDate >= as.Date("2021-05-13") |
              Classification == "Rejected after testing" & MonitoringStartDate >= as.Date("2021-05-13") |
              Classification == "Rejected - contact > 14 days" & MonitoringStartDate >= as.Date("2021-05-13")) %>%
    select(RecordID, AccountID, CaseNumber, Classification, RecordType, RecordOwner, Sex, Postcode, CaseFoundBy, EpiClassification, ClusterName,
           CalculatedOnsetDate, ContactWithRecordID, ContactWithPrimaryContact, LGA, PHU, MonitoringStartDate) %>%
    left_join(Persons %>%
                select(RecordID, DateOfBirth, FirstName, Surname, Mobile, HomePhone, AddressLine1, AddressLine2, Status), by = "RecordID") %>%
    # format case number, first/last names
    mutate(FirstName = tolower(FirstName),
           Surname = tolower(Surname),
           FirstNameClean = str_remove(FirstName, "[^a-zA-Z]"),
           SurnameClean = str_remove(Surname, "[^a-zA-Z]")) %>%
    MobileCleaning( "Mobile", "Mobile", format = "04") %>%
    mutate( Mobile_phone=NULL,
            Home_phone=NULL)

}
