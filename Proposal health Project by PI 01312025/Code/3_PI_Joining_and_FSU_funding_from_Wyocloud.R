# =================================================
# SCRIPT 3: MERGING JOINING DATE AND FUNDING DATA
# =================================================


# =====================================================
# STEP 1: Load and Clean Joining Date Data
# =====================================================

# Load Employee Assignment and Position Data
joining_date_data <- read_excel(Sys.getenv("JOINING_DATE_DATA_PATH_OLD"))


# Convert 'Rehire Date' to Date Format
joining_date_data <- joining_date_data %>%
  mutate(`Rehire Date` = ymd(`Rehire Date`))  # Ensures date format

# Create New Dataset with Latest "Rehire Date" as New "Hire_date"
latest_hire_date_data <- joining_date_data %>%
  group_by(`Employee Name`) %>%
  summarise(Hire_date = max(`Rehire Date`, na.rm = TRUE)) %>%
  ungroup()

# Extract Required Columns
joining_date_only <- latest_hire_date_data %>%
  select(`Employee Name`, Hire_date) %>%
  rename(`Project Manager` = `Employee Name`)  %>%
rename(`Assignment Start Date` = Hire_date)



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



# =====================================================
# STEP 3: Merge Joining Date and Funding Data
# =====================================================

# Merge Data Using 'Project Manager' as the Key
pi_joining_date_and_funding <- left_join(fund_by_project_manager, 
                                         joining_date_only, 
                                         by = "Project Manager")




pi_joining_date_and_funding <- pi_joining_date_and_funding %>%
  mutate(`Assignment Start Date` = as.Date(`Assignment Start Date`,format = "Y%"),
         `Joining FY` = if_else(month(`Assignment Start Date`) >= 7,
                                paste0("FY", year(`Assignment Start Date`) + 1),
                                paste0("FY", year(`Assignment Start Date`))))


names(pi_joining_date_and_funding)[1] <- "PI"


