FSU_joining_date_data <- read_excel(Sys.getenv("FACULTY_STARTUP_FUND_DATA_PATH_JAMI"))


pi_joining_date_and_funding <- FSU_joining_date_data %>%
  mutate(`Hire Date` = as.Date(`Hire Date`,format = "Y%"),
         `Joining FY` = if_else(month(`Hire Date`) >= 7,
                                paste0("FY", year(`Hire Date`) + 1),
                                paste0("FY", year(`Hire Date`))))

names(pi_joining_date_and_funding)[2] <- "PI"


pi_joining_date_and_funding <- pi_joining_date_and_funding %>%
  filter(`Tenure Status` %in% c("Tenured Faculty", "Tenured Track Faculty"),
         Startup > 0) %>%
  rename(Total_FSU = Startup)


# View(pi_joining_date_and_funding$`Joining FY`)
# 
# 


# table(pi_joining_date_and_funding$`Joining FY`)
# aggregate(Total_FSU ~ `Joining FY`, data = pi_joining_date_and_funding, FUN = sum)
# a <-  aggregate(Total_FSU ~ `Joining FY`, data = pi_joining_date_and_funding, FUN = sum)



pi_joining_date_and_funding$PI <- sapply(pi_joining_date_and_funding$PI, extract_first_last)