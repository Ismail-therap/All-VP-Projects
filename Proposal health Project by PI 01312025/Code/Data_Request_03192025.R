# Load Data:
# Load Data Paths
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/1_Proposal_data_load_and_clean_from_Cayuse.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/2_Financial_data_load_and_clean_from_Wyocloud.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/3_PI_Joining_and_FSU_funding_from_Wyocloud.R")


# After running above 3 services, we are getting mainly 2 Mother data sets.

# Mother data 1: award_non_award
# Mother data 2: pi_joining_date_and_funding


# =====================================================
# STEP 2: Preparing PI Data for Filtering
# =====================================================

# 📌 List of Project Managers
project_managers <- c(
  "Ahn, Juhyeon", "Aikens, Ellen", "Armstrong, Melanie", "Bailey, Ryan", 
  "Barrile, Gabriel", "Bedford, Nicole", "Beiermann, Clint", "Bell, Jennifer", 
  "Bernhardt, Natasha", "Bhattarai, Surendra", "Biasi, Joseph", 
  "Bock, Jarrod", "Borgholthaus, Cameron", "Brower, Alexandra", "Burgess, Matthew", 
  "Burton, Peter", "Davidson, Curt", "Davis, Robert", "De Mello Tavares Lima, Paulo", 
  "Demir, Aysegul", "Dittoe, Dana", "Dyett, Jordan", "Eidin, Emil", "Elokely, Khaled", 
  "Encinger, Amy", "Evans, Alecia", "Field, Sean", "Forzan, Maria", "French, Alexander", 
  "Gansauer, Grete", "Genoese, Francesca", "Germain, Sara", "Gilbert, Lauren", 
  "Grogan, Kelly", "Groot, Koen", "Grover, Abhay", "Gulick, Eleanor", "Harrington, Erin", 
  "Hawes, Jason", "Hayes, Lauren", "Holmes Fay, Irina", "Hunt, Tiffany", "Imhof, Michael", 
  "Iqbal, Hasan", "Irisarri, Jorge", "James, Alexander", "Joyce, Meridith", "Kane, Sarah", 
  "Koger, Benjamin", "Krause, Jennifer", "Kuper, Julie", "Leonard, Bryan", "Lewis, Madeline", 
  "Li, Xuesong", "Macy, Marisa", "Maia Sabino, Roberta", "Majeed, Yaqoob", "Mann, Allison", 
  "Matthews, Samuel", "McClure, Kenneth", "Moe, Maxwell", "Nowell, Stella", "Pascual, David", 
  "Payne, Anna", "Perry, Anne", "Petrovic, Alexander", "Phalen, Hannah", "Phillips, Brittney", 
  "Pinello, Jennifer", "Ragan, Izabela", "Rau, Daniel", "Saito, Masanori", "Sanders, Miriam", 
  "Saxena, Ankit", "Schlomer, Matthew", "Shane-Nichols, Amy", 
  "Sheshappanavar, Shivanand Venkanna", "Shukla, Rammohan", "Song, Yu", "Srednik, Mariela", 
  "Suyama, Takashi", "Tatum, Garrett", "Taylor, Dane", "Taylor, Nicolina", 
  "Tedmon-Jones, Tyler", "Tsai, Yu-Tsung", "Tuft, Alexander", "Vanderstichel, Raphael", 
  "Walker, Ian", "Woodward, Richard", "Wor, Magdalena", "Yang, Xuhao", "Zhang, Ling",
  "Nakka David, Sheba Rani","Elshehabi, Tawfik","Nguyen, Duong","McFarlin, Jamie","Zhou, Zejian"
  
  
)

# 📌 Format Names as "First Name Last Name"
formatted_names <- unname(sapply(project_managers, function(name) {
  parts <- strsplit(name, ", ")[[1]]
  paste(parts[2], parts[1])
}))

# 📌 Create Name Patterns for Filtering
first_names <- sapply(strsplit(formatted_names, " "), function(x) x[1])
other_part <- sapply(strsplit(formatted_names, " "), function(x) x[2])
name_patterns <- paste0(first_names, ".*", other_part)


# ===================================================================
  # STEP 2: Cleaning Joining date and FSU data based on spot checking
# ===================================================================

# After spot checking found unnecessary PI and also found someone very important missing. So, that's why deleting and adding 2 PI's in this part. 

# Load PI Joining Date and Funding Data
pi_joining_date_and_funding <- pi_joining_date_and_funding %>%
  filter(`Project Manager` != "Williams-Woodward, Jean")   # Remove unwanted PI




# ============================================
# STEP 3: Filtering Joining Date and FSU Data
# =============================================

# 📌 Filter Joining Date and FSU Data
filtered_pi_joining_funding <- pi_joining_date_and_funding %>%
  filter(str_detect(`Project Manager`, paste(other_part, collapse = "|")))

# 📌 Rename and Rearrange Columns
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  rename(
    `Total Start Up Funds` = `Total_FSU`,
    `Joining Date` = `Assignment Start Date`
  ) %>%
  select(`Project Manager`, `Joining Date`,`Joining FY`, `Total Start Up Funds`)

filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`Total Start Up Funds` = format(`Total Start Up Funds`, scientific = FALSE))

# 📌 Remove Unnecessary Entry
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  filter(`Project Manager` != "Pru, James")

# 📌 Correct PI Name Format
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`Project Manager` = str_replace(`Project Manager`, "^(.*),\\s*(.*)$", "\\2 \\1"))




# ==============================
# STEP 4: Filtering Award Data
# ===============================

# 📌 Filter Award Data Using PI Names
filtered_award_non_award <- award_non_award %>%
  mutate(
    Matched_Pattern = sapply(PI, function(x) {
      matched_pattern <- name_patterns[which(str_detect(x, name_patterns))]
      if (length(matched_pattern) > 0) matched_pattern else NA
    })
  ) %>%
  filter(!is.na(Matched_Pattern))  # Retain only matched entries


filtered_award_non_award <- filtered_award_non_award %>%
  mutate(`Project Manager` = str_replace(Matched_Pattern, "(.*)\\..*\\.(.*)", "\\2 \\1"))



# Convert "Juhyeon.*Ahn" to "Ahn Juhyeon"
filtered_award_non_award <- filtered_award_non_award %>%
  mutate(`Project Manager` = str_replace(Matched_Pattern, "^(.*?)\\..*\\.(.*)$", "\\2 \\1"))


# 📌 Correct Name Format
filtered_award_non_award <- filtered_award_non_award %>%
  mutate(`Project Manager` = str_replace_all(Matched_Pattern, "(.*)\\..*\\.(.*)", "\\2 \\1")) %>%
  mutate(`Project Manager` = str_replace_all(`Project Manager`, "\\*", "")) %>%
  mutate(`Project Manager` = str_replace_all(`Project Manager`, "\\.", " ")) 




# =====================================================
# STEP 5: Joining Filtered Award Data with FSU
# =====================================================

# 📌 Name Corrections for Specific PIs
filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`Project Manager` = case_when(
    `Project Manager` == "Shivanand Venkanna Sheshappanavar" ~ "Shivanand Venkanna",
    `Project Manager` == "Roberta Maia Sabino" ~ "Roberta Maia",
    `Project Manager` == "Paulo De Mello Tavares Lima" ~ "Paulo De",
    `Project Manager` == "Irina Holmes Fay" ~ "Irina Holmes",
    `Project Manager` == "Sheba Rani Nakka David" ~ "Sheba Rani",
    TRUE ~ `Project Manager`
  ))




# 📌 Final Data Merge
filtered_pi_joining_funding_final <- left_join(filtered_award_non_award, 
                                               filtered_pi_joining_funding, 
                                               by = "Project Manager") 




# 📌 Clean and Format Numerical Columns
filtered_pi_joining_funding_final <- filtered_pi_joining_funding_final %>%
  mutate(`Project Funding Amount` = as.numeric(gsub("[^0-9.-]", "", `Project Funding Amount`)),
         `Total Project Cost` = as.numeric(gsub("[^0-9.-]", "", `Total Project Cost`))) %>%
  mutate(`Project Funding Amount` = if_else(Status == "Funded" & is.na(`Project Funding Amount`),
                                            `Total Project Cost`, `Project Funding Amount`))


# 📌 Remove Columns Named "NA"
filtered_pi_joining_funding_final <- filtered_pi_joining_funding_final %>%
  select(-any_of("NA"))


# Ensure date columns are in Date format
filtered_pi_joining_funding_final <- filtered_pi_joining_funding_final %>%
  mutate(
    `Joining Date` = as.Date(`Joining Date`, format = "%Y-%m-%d"),
    `First_Proposal_Submission_Date` = as.Date(`First_Proposal_Submission_Date`, format = "%m/%d/%Y"),
    `First_Federal_Grant_Submission_Date` = as.Date(`First_Federal_Grant_Submission_Date`, format = "%m/%d/%Y")
  )


# Create new columns with month differences and keep negative values
filtered_pi_joining_funding_final <- filtered_pi_joining_funding_final %>%
  mutate(
    Months_to_submit_first_proposal = round(as.numeric(difftime(`First_Proposal_Submission_Date`, `Joining Date`, units = "days")) / 30, 1),
    Months_to_submit_first_federal_grant = round(as.numeric(difftime(`First_Federal_Grant_Submission_Date`, `Joining Date`, units = "days")) / 30, 1)
  )

# 📌 Save as CSV
write.csv(filtered_pi_joining_funding_final, 
          Sys.getenv("OUTPUT_MERGED_FSU_AWARD_PATH"), 
          row.names = FALSE)



# =====================================================
# STEP 6: Identifying PIs Without Any Submission
# =====================================================

# 📌 Extract Clean First and Last Names
extract_names <- function(name) {
  parts <- unlist(strsplit(name, " "))
  if (length(parts) >= 2) paste(parts[1], parts[length(parts)]) else parts[1]
}

# Extract Cleaned Names
pi_cleaned <- sapply(unique(filtered_pi_joining_funding_final$PI), extract_names)
formatted_cleaned <- sapply(formatted_names, extract_names)

# 📌 Identify Missing PIs

pi_without_any_submission <- setdiff(formatted_cleaned, pi_cleaned)

# 📌 Save Missing PI Data
names_df <- data.frame(`Project Manager` = pi_without_any_submission)
colnames(names_df)[1] <- "Project Manager"


pi_without_any_submission_data <- merge(filtered_pi_joining_funding, names_df, by = "Project Manager")

# pi_without_any_submission_data <- pi_without_any_submission_data %>%
#   filter(`Project Manager` != "Anne Perry")

# 📌 Save PI Without Submission Data
write.csv(pi_without_any_submission_data, 
          Sys.getenv("OUTPUT_NO_SUBMISSION_PATH"), 
          row.names = FALSE)


## Checking number of PI counts before analysis:

length(unique(filtered_pi_joining_funding_final$`Project Manager`))
length(unique(pi_without_any_submission_data$`Project Manager`))



table(filtered_pi_joining_funding_final$`Joining FY`)













########################################`Joining FY`######################
# For One page summary report
###############################

fed_funds <- filtered_pi_joining_funding_final %>%
  filter(`sponsor type` == "U.S. Federal Government" & First_Proposal_Submission_Date == First_Federal_Grant_Submission_Date)


pi_less_than_year <- filtered_pi_joining_funding_final %>%
  filter(Months_to_submit_first_proposal < 12) %>%
  distinct(`PI`) %>%
  nrow()

pi_less_than_year_fed <- filtered_pi_joining_funding_final %>%
  filter(Months_to_submit_first_federal_grant < 12) %>%
  distinct(`PI`) %>%
  nrow()


valid_month_taken <- filtered_pi_joining_funding_final %>%
  filter(Months_to_submit_first_proposal>=0 & Months_to_submit_first_federal_grant>=0)


# Amount transfered:
# transfer_amount <- filtered_pi_joining_funding_final %>%
#   filter(Status=="Transfer")



filtered_pi_joining_funding <- filtered_pi_joining_funding %>%
  mutate(`Total Start Up Funds` = gsub("[^0-9.]", "", `Total Start Up Funds`),
         `Total Start Up Funds` = as.numeric(`Total Start Up Funds`))

FSU_total <- sum(filtered_pi_joining_funding$`Total Start Up Funds`, na.rm = TRUE)



# Results:


print(paste0("Total PI considered in this report: ",dim(filtered_pi_joining_funding)[1]))
print(paste0("Total active PI (at least submitted one proposal): ",length(unique(filtered_pi_joining_funding_final$PI))))
print(paste0("PI who haven't submitted any proposal: ", length(unique(pi_without_any_submission_data$`Project Manager`))))
print(paste0("Total FSU allocated among ",dim(filtered_pi_joining_funding)[1], " PI's: $",FSU_total ))

print(paste0("Total Fund : $", sum(filtered_pi_joining_funding_final$`Project Funding Amount`,na.rm = TRUE)))
# transfer_percent <- (sum(transfer_amount$`Project Funding Amount`)/sum(filtered_pi_joining_funding_final$`Project Funding Amount`,na.rm = TRUE))*100
# print(paste0("Total % of fund comes through transfer: ", round(transfer_percent,2),"%($",sum(transfer_amount$`Project Funding Amount`),")"))

median_time_taken_after_joining <- median(valid_month_taken$Months_to_submit_first_proposal)
print(paste0("Median time needed after joining to submit the first proposal: ",median_time_taken_after_joining," months."))
first_prop_prcnt <- round((pi_less_than_year/length(unique(filtered_pi_joining_funding_final$PI)))*100,2)
print( paste0("Among the PI who are submitting proposals, ",first_prop_prcnt ,"% ", "need less than a year to submit their first proposal."))
first_grant_is_fedral <-  round((length(unique(fed_funds$PI))/length(unique(filtered_pi_joining_funding_final$PI)))*100,2)
print( paste0("Among the PI who are submitting proposals, ",first_grant_is_fedral ,"% ", " first proposal was a fedral grant."))

