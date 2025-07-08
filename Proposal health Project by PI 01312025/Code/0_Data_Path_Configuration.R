# ==============================================
# SCRIPT 0: PACKAGES AND DATA PATH CONFIGURATION 
# ==============================================

# 📌 Load Required Libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(lubridate)  # For handling date-time conversions
library(stringr)    # For string operations
library(tidyr)      # For reshaping data



# =====================================================
# STEP 1: Define Base Path for Data
# =====================================================

# 📌 Base Data Directory
base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Data/"

# 📌 Define File Paths Using Base Path
# Cayuse Data
proposal_data_path <- file.path(base_path, "Cayuse Data/Proposal_Data_All_04302025.csv")

# WyoCloud Data
wyocloud_path <- file.path(base_path, "WyoCloud Data/")
project_financial_summary_path <- file.path(wyocloud_path, "Project Financial Summary_Results_04302025.xlsx")
project_information_path <- file.path(wyocloud_path, "Project Information_Results 04302025.xlsx")
old_joining_date_data_path <- file.path(wyocloud_path, "Hire Date Employees.xlsx")
joining_date_data_path <- file.path(base_path, "Kelli/New Faculty Five Years.xlsx")
faculty_startup_fund_data_path <- file.path(wyocloud_path, "Faculty_Start_Up_Fund_Data_04302025.xlsx")

#Joining and FSU data from Jami

jami_path <- file.path(base_path, "Jami/New Faculty Five Years and start-up awarded.xlsx")



# Award External Funding Data
college_name_data_path <- file.path(base_path, "Award External Funding By Dept_Details.xlsx")

# =====================================================
# STEP 2: Define Output Paths
# =====================================================

# 📌 Define Base Output Directory
output_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Output/04302025/By FY/"

# # 📌 Define Output File Names
# output_merged_fsu_award_path <- file.path(output_path, "award_non_award_submission.csv")
# output_no_submission_path <- file.path(output_path, "pi_without_any_submission.csv")

# =====================================================
# STEP 3: Export Paths as Environment Variables
# =====================================================

Sys.setenv(PROPOSAL_DATA_PATH = proposal_data_path)
Sys.setenv(PROJECT_FINANCIAL_SUMMARY_PATH = project_financial_summary_path)
Sys.setenv(PROJECT_INFORMATION_PATH = project_information_path)
Sys.setenv(COLLEGE_NAME_DATA_PATH = college_name_data_path)
Sys.setenv(JOINING_DATE_DATA_PATH = joining_date_data_path)
Sys.setenv(JOINING_DATE_DATA_PATH_OLD = old_joining_date_data_path)
Sys.setenv(FACULTY_STARTUP_FUND_DATA_PATH = faculty_startup_fund_data_path)
Sys.setenv(FACULTY_STARTUP_FUND_DATA_PATH_JAMI = jami_path)


# Export Output Paths
Sys.setenv(OUTPUT_MERGED_FSU_AWARD_PATH = output_path)
Sys.setenv(OUTPUT_NO_SUBMISSION_PATH = output_path)


#========================================================
# STEP 4: Required Functions
#========================================================

# To create the FY

library(lubridate)

get_fy <- function(date, fy_start_month = 7) {
  date <- as.Date(date)  # ensures POSIXct/t becomes Date
  ifelse(
    is.na(date),
    NA_character_,
    paste0("FY", year(date) + ifelse(month(date) >= fy_start_month, 1, 0))
  )
}



# Function to extract only first and last names
extract_first_last <- function(name) {
  words <- str_split(name, "\\s+")[[1]]
  if (length(words) >= 2) {
    paste(words[1], words[length(words)])
  } else {
    name  # Return as-is if name has only one word
  }
}


summarize_pi_funding_by_FY <- function(FY, pi_joining_date_and_funding, award_non_award) {
  library(dplyr)
  library(lubridate)
  
  # Step 1: Filter PIs by Joining FY
  pi_joining_funding_selected_FY <- pi_joining_date_and_funding %>%
    filter(`Joining FY` == FY)
  
  All_PI_in_selected_FY <- unique(pi_joining_funding_selected_FY$PI)
  
  # Step 2: Merge with award/non-award data
  pi_joining_funding_final_selected_FY <- merge(
    award_non_award,
    pi_joining_funding_selected_FY,
    by = "PI"
  )
  
  # Step 3: Identify PIs with no submission
  Pi_name_without_anysubmission <- setdiff(
    unique(pi_joining_funding_selected_FY$PI),
    unique(pi_joining_funding_final_selected_FY$PI)
  )
  
  Pi_details_without_submission <- pi_joining_date_and_funding %>%
    filter(PI %in% Pi_name_without_anysubmission)
  
  # Step 4: Clean and calculate durations
  pi_joining_funding_final_selected_FY <- pi_joining_funding_final_selected_FY %>%
    mutate(
      `Project Funding Amount` = as.numeric(gsub("[^0-9.-]", "", `Project Funding Amount`)),
      `Total Project Cost` = as.numeric(gsub("[^0-9.-]", "", `Total Project Cost`)),
      `Project Funding Amount` = if_else(Status == "Funded" & is.na(`Project Funding Amount`),
                                         `Total Project Cost`, `Project Funding Amount`)
    ) %>%
    select(-any_of("NA")) %>%
    mutate(
      `Hire Date` = as.Date(`Hire Date`),
      `First_Proposal_Submission_Date` = as.Date(`First_Proposal_Submission_Date`, format = "%m/%d/%Y"),
      `First_Federal_Grant_Submission_Date` = as.Date(`First_Federal_Grant_Submission_Date`, format = "%m/%d/%Y"),
      Months_to_submit_first_proposal = round(as.numeric(difftime(`First_Proposal_Submission_Date`, `Hire Date`, units = "days")) / 30, 1),
      Months_to_submit_first_federal_grant = round(as.numeric(difftime(`First_Federal_Grant_Submission_Date`, `Hire Date`, units = "days")) / 30, 1)
    )
  
  # Step 5: Summary calculations
  fed_funds <- pi_joining_funding_final_selected_FY %>%
    filter(`sponsor type` == "U.S. Federal Government" & First_Proposal_Submission_Date == First_Federal_Grant_Submission_Date)
  
  pi_less_than_year <- pi_joining_funding_final_selected_FY %>%
    filter(Months_to_submit_first_proposal < 12) %>%
    distinct(PI) %>%
    nrow()
  
  pi_less_than_year_fed <- pi_joining_funding_final_selected_FY %>%
    filter(Months_to_submit_first_federal_grant < 12) %>%
    distinct(PI) %>%
    nrow()
  
  valid_month_taken <- pi_joining_funding_final_selected_FY %>%
    filter(Months_to_submit_first_proposal >= 0 & Months_to_submit_first_federal_grant >= 0)
  
  start_up_fund_by_pi <- pi_joining_funding_final_selected_FY %>%
    select(PI, Total_FSU) %>%
    distinct() %>%
    mutate(Total_FSU = as.numeric(gsub("[^0-9.]", "", Total_FSU)))
  
  FSU_total <- sum(pi_joining_funding_selected_FY$Total_FSU, na.rm = TRUE)
  
  Total_submitted <- sum(pi_joining_funding_final_selected_FY$Total_submitted_to_sponsor, na.rm = TRUE)
  Total_funded <- sum(pi_joining_funding_final_selected_FY$Funded, na.rm = TRUE)
  

  print
  # Step 6: Summary output
  
  
  cat("\n---------------------------------\nSummary Report for", FY, "\n---------------------------------\n")
  cat("Total PI considered: ", length(unique(pi_joining_funding_final_selected_FY$`PI`)) + length(unique(Pi_details_without_submission$`PI`)), "\n")
  cat("Total active PI (at least submitted one proposal): ", length(unique(pi_joining_funding_final_selected_FY$PI)), "\n")
  cat("PI who haven't submitted any proposal: ", length(unique(Pi_details_without_submission$`PI`)), "\n")
  
  cat("Total FSU allocated among ", nrow(pi_joining_funding_selected_FY), " PI's: $", FSU_total, "\n")
  
  cat("Total Proposal submitted: ", Total_submitted, "\n")
  cat("Total Proposal awarded: ", Total_funded, "\n")
  cat("Total Fund : $", sum(pi_joining_funding_final_selected_FY$`Project Funding Amount`, na.rm = TRUE), "\n")
  
  median_time_taken_after_joining <- median(valid_month_taken$Months_to_submit_first_proposal, na.rm = TRUE)
  first_prop_prcnt <- round((pi_less_than_year / length(unique(pi_joining_funding_final_selected_FY$PI))) * 100, 2)
  first_grant_is_fedral <- round((length(unique(fed_funds$PI)) / length(unique(pi_joining_funding_final_selected_FY$PI))) * 100, 2)
  
  cat("Median time needed after hire to submit the first proposal: ", median_time_taken_after_joining, " months.\n")
  cat("Among the PI who are submitting proposals, ", first_prop_prcnt, "% submitted within a year of hire.\n")
  cat("Among the PI who are submitting proposals, ", first_grant_is_fedral, "% submitted their first proposal to a federal agency.\n")
  
  # Step 7: Export CSVs
  write.csv(pi_joining_funding_final_selected_FY,
            file = file.path(Sys.getenv("OUTPUT_MERGED_FSU_AWARD_PATH"), paste0("Award_non_award_FY_", FY, ".csv")),
            row.names = FALSE)
  
  write.csv(Pi_details_without_submission,
            file = file.path(Sys.getenv("OUTPUT_NO_SUBMISSION_PATH"), paste0("PI_no_submission_FY_", FY, ".csv")),
            row.names = FALSE)
  
  # Step 8: Return outputs
  return(list(
    pi_joining_funding_final_selected_FY = pi_joining_funding_final_selected_FY,
    Pi_details_without_submission = Pi_details_without_submission
  ))
}
