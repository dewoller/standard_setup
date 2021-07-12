wideScreen = function() {
  options(width=as.numeric(system("tput cols", intern=TRUE))-5)
}

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos=r)
})

library( conflicted)
library( dotenv )
conflicted::conflict_prefer("year", "lubridate")
conflicted::conflict_prefer("month", "lubridate")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("isoweek", "lubridate")
conflicted::conflict_prefer("week", "lubridate")
conflicted::conflict_prefer("wday", "lubridate")
conflicted::conflict_prefer( 'read_xlsx', 'readxl' )
conflicted::conflict_prefer("annotate", "ggplot2")
conflicted::conflict_prefer("lag", "dplyr")

