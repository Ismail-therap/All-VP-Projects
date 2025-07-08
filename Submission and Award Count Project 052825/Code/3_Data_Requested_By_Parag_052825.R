# ================================================
# SCRIPT 2: PROJECT FINANCIAL DATA CLEANING & MERGING
# ================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Submission and Award Count Project 052825/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))

# dim(proposal_data)
# table(proposal_data$`epscor-idea`)
# table(proposal_data$`sponsor type`)
# table(proposal_data$Sponsor_Type_Grouped)


# Excluding the Internal proposals
proposal_data <- proposal_data %>%
  filter(`sponsor type` != "Institutional (UW)")



############## Calculation (Count the number of submission and award for each Quarter seperately) ########




library(dplyr)

# Prepare submitted summary with Quarter
submitted_summary <- proposal_data %>%
  filter(!is.na(Actual_Submission_Quarter)) %>%
  group_by( `College/Division`, Sponsor_Type_Grouped, Sponsor_Category, Quarter = Actual_Submission_Quarter) %>%
  summarise(
    Applications_Submitted = n(),
    Amount_Requested = sum(`Total Sponsor Costs`, na.rm = TRUE),
    .groups = "drop"
  )





# Prepare funded summary with Quarter
funded_summary <- proposal_data %>%
  filter(!is.na(Actual_Funding_Quarter)) %>%
  group_by( `College/Division`, Sponsor_Type_Grouped, Sponsor_Category, Quarter = Actual_Funding_Quarter) %>%
  summarise(
    Grants_Received = n(),
    Amount_Received = sum(`Total Sponsor Costs`, na.rm = TRUE),
    .groups = "drop"
  )



# Join on all 5 keys
summary_combined_version_2 <- submitted_summary %>%
  full_join(funded_summary,
            by = c("College/Division" = "College/Division",
                   "Sponsor_Type_Grouped" = "Sponsor_Type_Grouped",
                   "Sponsor_Category" = "Sponsor_Category",
                   "Quarter" = "Quarter")) %>%
  mutate(
    Applications_Submitted = replace_na(Applications_Submitted, 0),
    Grants_Received = replace_na(Grants_Received, 0),
    Amount_Requested = replace_na(Amount_Requested, 0),
    Amount_Received = replace_na(Amount_Received, 0),
    
    
    # ✅ Now calculate Funding Rate using adjusted Applications_Submitted
    Funding_Rate = ifelse(Applications_Submitted == 0, NA,
                          Grants_Received / Applications_Submitted * 100),
    
    # Format for clean display
    Funding_Rate = ifelse(is.na(Funding_Rate), NA, sprintf("%.1f", Funding_Rate)),
    Amount_Requested = as.character(round(Amount_Requested, 0)),
    Amount_Received = as.character(round(Amount_Received, 0))
  ) %>%
  filter(Quarter %in% c("Q1 FY2024", "Q2 FY2024", "Q3 FY2024", "Q4 FY2024",
                        "Q1 FY2025", "Q2 FY2025", "Q3 FY2025")) %>%
  select(`College/Division`, Quarter, Sponsor_Type_Grouped, Sponsor_Category,
         Applications_Submitted, Grants_Received, Funding_Rate,
         Amount_Requested, Amount_Received) %>%
  arrange(`College/Division`, factor(Quarter, levels = c("Q1 FY2024", "Q2 FY2024", "Q3 FY2024", "Q4 FY2024",
                                                         "Q1 FY2025", "Q2 FY2025", "Q3 FY2025")))







output_path_prop <- file.path(output_path, "Summarized_data_version_2.csv") # Mother data for Pivot Tables
write.csv(summary_combined_version_2,output_path_prop,row.names=F)





#######################

# ✅ Select columns from filtered data
selected_columns <- c(
  "Proposal #", "full proposal title", "College/Division", "Admin Unit", "Instrument Type", "activity type", "PI", 
  "Prime Sponsor", "Project End Date", "Project Start Date", "Project Title", "Proposal Type", "Sponsor",
  "History Action Day and Comment", "Actual_Submission_Date", "Actual_Udr_Consid_Date", 
  "Actual_Funding_Date", "Actual_Not_Funding_Date", "Actual_Submission_FY", "Actual_Udr_Consid_FY", 
  "Actual_Funding_FY", "Actual_Not_Funding_FY", "Created Date.y", "Proposal_Creation_FY", 
  "Days_to_Submission", "Days_to_Funding_From_Submission", "Days_to_Non_Funding_From_Submission", 
  "Actual_Submission_Month_Year", "College/Division", "Actual_Submission_Quarter", "Actual_Funding_Quarter", 
  "Sponsor_Type_Grouped", "Sponsor_Category", "Submission_Status", "Submission_Count", "Award_Count", 
  "Total_Submission_Count", "Received_Total_Sponsor_Costs", "Total Direct Costs", 
  "Total Indirect Costs", "Total Project Cost", "Total Sponsor Costs"
)

# Step 0: Define quarters of interest (if not already defined)
quarters_of_interest <- c("Q1 FY2024", "Q2 FY2024", "Q3 FY2024", "Q4 FY2024", 
                          "Q1 FY2025", "Q2 FY2025", "Q3 FY2025")


Submitted_data_sub <- proposal_data %>%
  filter(!is.na(Actual_Submission_Quarter),
         Actual_Submission_Quarter %in% quarters_of_interest) %>%
  select(all_of(selected_columns))


Funded_data_sub <- proposal_data %>%
  filter(!is.na(Actual_Funding_Quarter),
         Actual_Funding_Quarter %in% quarters_of_interest) %>%
  select(selected_columns)






# Processed Raw data for transparency 
output_path_raw_processed <- file.path(output_path, "Processed_Raw_Submitted_Data.csv")
write.csv(Submitted_data_sub,output_path_raw_processed,row.names=F)


output_path_raw_processed <- file.path(output_path, "Processed_Raw_Funding_Data.csv")
write.csv(Funded_data_sub,output_path_raw_processed,row.names=F)













########### Additional Analysis (Optional not reported) #########

##### Count number of submission per quater and see how many of them are funded ### (This is not used for reporitng)

# summary_combined <- proposal_data %>%
#   filter(!is.na(Submission_Status)) %>%
#   mutate(Quarter = coalesce(Actual_Submission_Quarter, Actual_Funding_Quarter)) %>%
#   filter(Quarter %in% c("Q1 FY2024", "Q2 FY2024", "Q3 FY2024", "Q4 FY2024",
#                         "Q1 FY2025", "Q2 FY2025", "Q3 FY2025")) %>%
#   group_by(`College/Division`, Quarter, Sponsor_Type_Grouped, Sponsor_Category) %>%
#   summarise(
#     Base_Submitted = sum(Submission_Status == "Submitted"),
#     Grants_Received = sum(Submission_Status == "Funded"),
#     Amount_Requested = sum(`Total Sponsor Costs`, na.rm = TRUE),
#     Amount_Received = sum(ifelse(Submission_Status == "Funded", `Total Sponsor Costs`, 0), na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     Applications_Submitted = Base_Submitted + Grants_Received,
#     Funding_Rate = ifelse(Applications_Submitted == 0, NA,
#                           Grants_Received / Applications_Submitted * 100),
#     Funding_Rate = ifelse(is.na(Funding_Rate), NA, sprintf("%.1f", Funding_Rate)),
#     Amount_Requested = as.character(round(Amount_Requested, 0)),
#     Amount_Received = as.character(round(Amount_Received, 0))
#   ) %>%
#   select(`College/Division`, Quarter, Sponsor_Type_Grouped, Sponsor_Category,
#          Applications_Submitted, Grants_Received, Funding_Rate,
#          Amount_Requested, Amount_Received) %>%
#   arrange(`College/Division`, factor(Quarter, levels = c("Q1 FY2024", "Q2 FY2024", "Q3 FY2024", "Q4 FY2024",
#                                                          "Q1 FY2025", "Q2 FY2025", "Q3 FY2025")))
# 
# 
