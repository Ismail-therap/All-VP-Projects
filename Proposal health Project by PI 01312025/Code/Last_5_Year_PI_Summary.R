# Load Data:
# Load Data Paths
# Load Data:
# Load Data Paths
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/1_Proposal_data_load_and_clean_from_Cayuse.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/2_Financial_data_load_and_clean_from_Wyocloud.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/3_Updated_PI_Joining_Date_Data_Kelli.R")


# After running above 3 services, we are getting mainly 2 Mother data sets.

# Mother data 1: award_non_award
# Mother data 2: pi_joining_date_and_funding
# By PI we can join this two data.



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
  
  FSU_total <- sum(start_up_fund_by_pi$Total_FSU, na.rm = TRUE)
  
  Total_submitted <- sum(pi_joining_funding_final_selected_FY$Total_submitted_to_sponsor, na.rm = TRUE)
  Total_funded <- sum(pi_joining_funding_final_selected_FY$Funded, na.rm = TRUE)
  
  # Step 6: Summary output
  cat("\n---------------------------------\nSummary Report for", FY, "\n---------------------------------\n")
  cat("Total PI considered: ", length(unique(pi_joining_funding_final_selected_FY$`Project Manager`)) + length(unique(Pi_details_without_submission$`Project Manager`)), "\n")
  cat("Total active PI (at least submitted one proposal): ", length(unique(pi_joining_funding_final_selected_FY$PI)), "\n")
  cat("PI who haven't submitted any proposal: ", length(unique(Pi_details_without_submission$`Project Manager`)), "\n")
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

results <- summarize_pi_funding_by_FY("FY2020", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2021", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2022", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2023", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2024", pi_joining_date_and_funding, award_non_award)
results <- summarize_pi_funding_by_FY("FY2025", pi_joining_date_and_funding, award_non_award)



