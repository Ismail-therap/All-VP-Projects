# More analysis

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Submission and Award Count Project 052825/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))

quarters_of_interest <- c("Q1 FY2024", "Q2 FY2024", "Q3 FY2024", 
                          "Q4 FY2024", "Q1 FY2025", "Q2 FY2025", "Q3 FY2025")


library(dplyr)


#  1. Average Amount Requested per Proposal
results1 <- proposal_data %>%
  filter(Actual_Submission_Quarter %in% quarters_of_interest) %>%
  group_by(`College/Division`) %>%
  summarise(
    Avg_Amount_Requested = mean(`Total Sponsor Costs`, na.rm = TRUE),
    Median_Amount_Requested = median(`Total Sponsor Costs`, na.rm = TRUE),
    Proposal_Count = n()
  ) %>%
  arrange(desc(Avg_Amount_Requested))


#sum(results1$Proposal_Count)


# 2. Average Award Amount for Funded Proposals

results2 <- proposal_data %>%
  filter(Submission_Status == "Funded",
         Actual_Funding_Quarter %in% quarters_of_interest) %>%
  group_by(`College/Division`) %>%
  summarise(
    Avg_Award_Amount = mean(`Total Sponsor Costs`, na.rm = TRUE),
    Median_Award_Amount = median(`Total Sponsor Costs`, na.rm = TRUE),
    Funded_Proposals = n()
  ) %>%
  arrange(desc(Avg_Award_Amount))


#sum(results2$Funded_Proposals)


#  3. Distribution of Sponsor Types Among Funded Proposals

#table(proposal_data$Submission_Status)


results3 <- proposal_data %>%
  filter(Submission_Status == "Funded",
         Actual_Funding_Quarter %in% quarters_of_interest) %>%
  group_by(`College/Division`, Sponsor_Type_Grouped) %>%
  summarise(Funded_Proposals = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Sponsor_Type_Grouped, 
    values_from = Funded_Proposals,
    values_fill = 0
  )
results3



# Submission and Award Frequency per PI


results4 <- proposal_data %>%
  filter(Actual_Submission_Quarter %in% quarters_of_interest | 
           Actual_Funding_Quarter %in% quarters_of_interest) %>%
  group_by(`College/Division`, PI) %>%
  summarise(
    Total_Proposals = n(),
    Funded = sum(Submission_Status == "Funded"),
    PI_Funding_Rate = Funded / Total_Proposals * 100,
    .groups = "drop"
  ) %>%
  group_by(`College/Division`) %>%
  summarise(
    Avg_Proposals_per_PI = mean(Total_Proposals),
    Avg_Funded_per_PI = mean(Funded),
    Avg_PI_Funding_Rate = mean(PI_Funding_Rate, na.rm = TRUE),
    Unique_PIs = n_distinct(PI)
  ) %>%
  arrange(desc(Avg_PI_Funding_Rate))

results4



# 1. Activity type

college_activity_share <- proposal_data %>%
  filter(Submission_Status == "Funded",
         Actual_Funding_Quarter %in% quarters_of_interest) %>%  # ✅ Only use funding quarter
  group_by(`College/Division`, `activity type`) %>%
  summarise(Funded = n(), .groups = "drop") %>%
  group_by(`College/Division`) %>%
  mutate(
    Total = sum(Funded),
    Percent_of_Funded = round(Funded / Total * 100, 1)
  ) %>%
  arrange(`College/Division`, desc(Percent_of_Funded))

View(college_activity_share)

# I should explain the report by activity type and show which college doing better by activity type.

activity_type <- file.path(output_path, "Activity_Type_Analysis.csv")
write.csv(college_activity_share,activity_type,row.names=F)









# proposal_data$EPSCOR_IDea <- ifelse(proposal_data$`epscor-idea` %in% c("EPSCoR","IDEA"),"Yes","No")
# 
# table(proposal_data$`College/Division`, proposal_data$EPSCOR_IDea)
# 
# # Group by College and EPSCoR/IDeA label
# epscor_by_college <- proposal_data %>%
#   filter(Submission_Status == "Funded",
#          Actual_Funding_Quarter %in% quarters_of_interest) %>%
#   group_by(`College/Division`, EPSCOR_IDea) %>%
#   summarise(Funded_Proposals = n(), .groups = "drop") %>%
#   group_by(`College/Division`) %>%
#   mutate(
#     Total = sum(Funded_Proposals),
#     EPSCoR_Percentage = round(Funded_Proposals / Total * 100, 1)
#   ) %>%
#   filter(EPSCOR_IDea == "Yes") %>%
#   select(`College/Division`, Funded_Proposals, EPSCoR_Percentage) %>%
#   arrange(desc(EPSCoR_Percentage))
