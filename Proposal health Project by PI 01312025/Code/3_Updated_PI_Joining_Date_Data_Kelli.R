# =================================================
# SCRIPT 3: MERGING JOINING DATE AND FUNDING DATA
# =================================================


# =====================================================
# STEP 1: Load and Clean Joining Date Data
# =====================================================

# Load Employee Assignment and Position Data
joining_date_data <- read_excel(Sys.getenv("JOINING_DATE_DATA_PATH"))


colnames(joining_date_data)[1] <- "Project Manager"

length(unique(joining_date_data$`Project Manager`))

# Total 584 PI data provided by Kelli.

# =====================================================
# STEP 2: Load and Filter Faculty Startup Fund Data
# =====================================================

# Load Faculty Startup Fund Data
faculty_startup_fund_data <- read_excel(Sys.getenv("FACULTY_STARTUP_FUND_DATA_PATH"))




# Filter Data for 'VP for Research & Economic Development Office'
fund_by_project_manager <- faculty_startup_fund_data %>%
  filter(`Project Organization` == "VP for Research & Economic Development Office") %>%
  group_by(`Project Manager`) %>%
  summarise(
    Total_FSU = sum(`Project Funding Amount`, na.rm = TRUE)  # Handle potential NA values
  ) 



#length(unique(fund_by_project_manager$`Project Manager`))

# But 126 PI data available in WyoCloud regarding the FSU.

# =====================================================
# STEP 3: Merge Joining Date and Funding Data
# =====================================================

# Merge Data Using 'Project Manager' as the Key
# pi_joining_date_and_funding <- left_join(joining_date_data, 
#                                          fund_by_project_manager, 
#                                          by = "Project Manager")


pi_joining_date_and_funding <- merge(joining_date_data, 
                                         fund_by_project_manager, 
                                         by = "Project Manager")



#names(pi_joining_date_and_funding)
# Among 126 Individual 119 names matched (Need to check why 7 individual name not matched).

########################################################################################################
#setdiff(unique(fund_by_project_manager$`Project Manager`),unique(pi_joining_date_and_funding_updated$`Project Manager`))
# Spot checking the names joining date from Other source (we are using kelli source here but need to check from WyoCloud)
##########################################################################################################




pi_joining_date_and_funding <- pi_joining_date_and_funding %>%
  mutate(`Hire Date` = as.Date(`Hire Date`,format = "Y%"),
         `Joining FY` = if_else(month(`Hire Date`) >= 7,
                                paste0("FY", year(`Hire Date`) + 1),
                                paste0("FY", year(`Hire Date`))))


names(pi_joining_date_and_funding)[2] <- "PI"







