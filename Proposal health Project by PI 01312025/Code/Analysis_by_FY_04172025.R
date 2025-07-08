# Load Data:
# Load Data Paths
# Load Data:
# Load Data Paths
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/1_Proposal_data_load_and_clean_from_Cayuse.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/2_Financial_data_load_and_clean_from_Wyocloud.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/3_PI_Joining_and_FSU_funding_from_Wyocloud.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/4_PI_list_input_and_filtering_award_and_fsu_and_joining_data.R")



# After running above 3 services, we are getting mainly 2 Mother data sets.

# Mother data 1: award_non_award
# Mother data 2: pi_joining_date_and_funding



# Function to get the analysis by FY
get_pi_funding_data_by_FY <- function(FY, filtered_pi_joining_funding, filtered_award_non_award) {
  library(dplyr)
  
  # Subset for selected FY
  pi_joining_funding_selected_FY <- filtered_pi_joining_funding %>%
    filter(`Joining FY` == FY)
  
  # Unique PIs in selected FY
  All_PI_in_selected_FY <- unique(pi_joining_funding_selected_FY$`PI`)
  
  # Merge with award/non-award
  pi_joining_funding_final_selected_FY <- merge(
    filtered_award_non_award,
    pi_joining_funding_selected_FY,
    by = "PI"
  )
  
  # PIs with no submission
  Pi_name_without_anysubmission <- setdiff(
    unique(pi_joining_funding_selected_FY$`PI`),
    unique(pi_joining_funding_final_selected_FY$`PI`)
  )
  
  Pi_details_without_submission <- filtered_pi_joining_funding %>%
    filter(`PI` %in% Pi_name_without_anysubmission)
  
  # Clean numeric columns
  pi_joining_funding_final_selected_FY <- pi_joining_funding_final_selected_FY %>%
    mutate(
      `Project Funding Amount` = as.numeric(gsub("[^0-9.-]", "", `Project Funding Amount`)),
      `Total Project Cost` = as.numeric(gsub("[^0-9.-]", "", `Total Project Cost`)),
      `Project Funding Amount` = if_else(Status == "Funded" & is.na(`Project Funding Amount`),
                                         `Total Project Cost`, `Project Funding Amount`)
    ) %>%
    select(-any_of("NA")) %>%
    mutate(
      `Joining Date` = as.Date(`Joining Date`, format = "%Y-%m-%d"),
      `First_Proposal_Submission_Date` = as.Date(`First_Proposal_Submission_Date`, format = "%m/%d/%Y"),
      `First_Federal_Grant_Submission_Date` = as.Date(`First_Federal_Grant_Submission_Date`, format = "%m/%d/%Y"),
      Months_to_submit_first_proposal = round(as.numeric(difftime(`First_Proposal_Submission_Date`, `Joining Date`, units = "days")) / 30, 1),
      Months_to_submit_first_federal_grant = round(as.numeric(difftime(`First_Federal_Grant_Submission_Date`, `Joining Date`, units = "days")) / 30, 1)
    )
  
  # Summary Calculations
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
    select(`PI`, `Total Start Up Funds`) %>%
    distinct() %>%
    mutate(`Total Start Up Funds` = as.numeric(gsub("[^0-9.]", "", `Total Start Up Funds`)))
  
  FSU_total <- sum(start_up_fund_by_pi$`Total Start Up Funds`, na.rm = TRUE)
  
  
  Total_submitted <- sum(pi_joining_funding_final_selected_FY$Total_submitted_to_sponsor)
  Total_funded <- sum(pi_joining_funding_final_selected_FY$Funded)
  
  # Summary Outputs
  cat("\n---------------------------------\nSummary Report for", FY, "\n---------------------------------\n")
  cat("Total PI considered: ", length(unique(pi_joining_funding_final_selected_FY$`PI`))+length(unique(Pi_details_without_submission$`PI`)), "\n")
  cat("Total active PI (at least submitted one proposal): ", length(unique(pi_joining_funding_final_selected_FY$PI)), "\n")
  cat("PI who haven't submitted any proposal: ", length(unique(Pi_details_without_submission$`PI`)), "\n")
  cat("Total FSU allocated among ", nrow(pi_joining_funding_selected_FY), " PI's: $", FSU_total, "\n")
  
  cat("Total Proposal submitted: ", Total_submitted,"\n")
  cat("Total Proposal awarded: ", Total_funded,"\n")
  cat("Total Fund : $", sum(pi_joining_funding_final_selected_FY$`Project Funding Amount`, na.rm = TRUE), "\n")
  
  median_time_taken_after_joining <- median(valid_month_taken$Months_to_submit_first_proposal, na.rm = TRUE)
  first_prop_prcnt <- round((pi_less_than_year / length(unique(pi_joining_funding_final_selected_FY$PI))) * 100, 2)
  first_grant_is_fedral <- round((length(unique(fed_funds$PI)) / length(unique(pi_joining_funding_final_selected_FY$PI))) * 100, 2)
  
  cat("Median time needed after joining to submit the first proposal: ", median_time_taken_after_joining, " months.\n")
  cat("Among the PI who are submitting proposals, ", first_prop_prcnt, "% need less than a year to submit their first proposal.\n")
  cat("Among the PI who are submitting proposals, ", first_grant_is_fedral, "% submitted their first proposal to a federal agency.\n")
  
  
  
  
  write.csv(pi_joining_funding_final_selected_FY,
            file = file.path(Sys.getenv("OUTPUT_MERGED_FSU_AWARD_PATH"), paste0("Award_non_award_FY_", FY, ".csv")),
            row.names = FALSE)
  
  write.csv(Pi_details_without_submission,
            file = file.path(Sys.getenv("OUTPUT_NO_SUBMISSION_PATH"), paste0("PI_no_submission_FY_", FY, ".csv")),
            row.names = FALSE)
  
  return(list(
    pi_joining_funding_final_selected_FY = pi_joining_funding_final_selected_FY,
    Pi_details_without_submission = Pi_details_without_submission
  ))
}





result <- get_pi_funding_data_by_FY("FY2025", filtered_pi_joining_funding, filtered_award_non_award)
result <- get_pi_funding_data_by_FY("FY2024", filtered_pi_joining_funding, filtered_award_non_award)
result <- get_pi_funding_data_by_FY("FY2023", filtered_pi_joining_funding, filtered_award_non_award)
result <- get_pi_funding_data_by_FY("FY2022", filtered_pi_joining_funding, filtered_award_non_award)









