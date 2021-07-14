get_screening_from_clusters <- function  ( clusters_managed, LineList, Persons ) {

  clusters_managed %>%
  left_join(LineList %>%
            select(RecordID, AccountID, CaseNumber, Classification, RecordType, RecordOwner, Sex, Postcode, CaseFoundBy, EpiClassification, ClusterName,
                   CalculatedOnsetDate, ContactWithRecordID, ContactWithPrimaryContact, LGA, PHU), by = "RecordID") %>%
filter(RecordType == "COVID-19 Case Screening") %>%
# select(RecordID, AccountID, CaseNumber, Classification, RecordType, RecordOwner, Sex, Postcode, CaseFoundBy, EpiClassification, ClusterName,
#        CalculatedOnsetDate, ContactWithRecordID, ContactWithPrimaryContact, LGA, PHU) %>%
left_join(Persons %>%
          select(RecordID, DateOfBirth, FirstName, Surname, Mobile, HomePhone, AddressLine1, AddressLine2, Status), by = "RecordID") %>%
# format case number, first/last names
mutate(FirstName = tolower(FirstName),
       Surname = tolower(Surname),
       DateOfBirth = if_else(DateOfBirth == "1800-01-01","", as.character(DateOfBirth)),
       FirstNameClean = str_remove(FirstName, "[^a-zA-Z]"),
       SurnameClean = str_remove(Surname, "[^a-zA-Z]")) %>%
MobileCleaning( "Mobile", "Mobile", format = "04") %>%
  mutate( Mobile_phone=NULL,
         Home_phone=NULL,
         dummy.id = row_number())  # generate a dummy id
}
