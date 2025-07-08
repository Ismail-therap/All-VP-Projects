# To create the FY

library(lubridate)
library(dplyr)
library(readxl)


get_fy <- function(date, fy_start_month = 7) {
  date <- as.Date(date)  # ensures POSIXct/t becomes Date
  ifelse(
    is.na(date),
    NA_character_,
    paste0("FY", year(date) + ifelse(month(date) >= fy_start_month, 1, 0))
  )
}



Hanlon <- read_excel("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/IC for Dr. Hanlon/Project Expenditure Details_Results_Hanlon.xlsx")

Hanlon$`Accounting Date` <- as.Date(Hanlon$`Accounting Date`)


Hanlon <- Hanlon %>%
  filter(!is.na(`Accounting Date`)) %>%               # Remove NA Created Dates
  mutate( Accounting_FY = get_fy(`Accounting Date`))              # Apply FY conversion

write.csv(Hanlon,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/IC for Dr. Hanlon/Hanlon_IC.csv",row.names = FALSE)
