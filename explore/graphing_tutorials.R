# tutorial page for playing with graphics
library(tidyverse)
library(lubridate)


# 1) visit data visualisation at https://r4ds.had.co.nz/data-visualisation.html
# 2) get a feel for the ggplot package.
#     *Type* in some ggplot plot commands, get a feel for the syntax
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")


# 3) load in Covid datasets
LoadData_CRM_Location <- paste0("E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/DART/Data snapshots/RDS files/")
SitRepOrLatest <- "sitrep"
Files_required <- c("linelist", "Persons")
source(paste0(LoadData_CRM_Location, "LoadData_CRM.R"))


# 4) make up some covid plots, e.g.

LineList %>%
  as_tibble() %>%
  count( RecordType, Sex) %>%
  ggplot() +
  geom_col( aes( RecordType, n, fill=Sex ))


# 5)
