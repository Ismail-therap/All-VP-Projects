# ================================================
# SCRIPT 1: PROPOSAL DATA CLEANING & DATE CREATION
# ================================================

source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Submission and Award Count Project 052825/Code/0_Data_Path_Configuration.R")

library(dplyr)
library(lubridate)
library(readr)

options(scipen = 999)
options(digits = 22)

# STEP 1: Load the data
award_data <- read_csv(Sys.getenv("AWARD_DATA_PATH"))


award_data <- award_data %>%
  mutate(`Award Start Date` = ifelse(`Modification Type` == "New Funding Increment",
                                      `Modification Date`,
                                      `Award Start Date`))




# Pre-Award Spending == ?
# Pre-Award Spending Removal == ? 
### Add Incremental Amount Column #

# =======================
# Calculate Increment Amount for Awards
# - Filter only relevant modification types (including Key Personnel Change)
# - Clean and convert Obligated Amount to numeric
# - Identify Base Awards and sort accordingly
# - Calculate incremental funding by subtracting previous obligated amount
# - If the increment is negative and not a De-Obligation or Sponsor Decrease, reset it to the full obligated amount
# - Round the results to 2 decimal places for consistency
# =======================


# valid_mod_types <- c("Original Award", "New Funding Increment","Pre-Award Spending","Pre-Award Spending Removal","Key Personnel Change")
# #valid_mod_types <- c("Original Award", "New Funding Increment", "Sponsor Decrease", "De-Obligation","Key Personnel Change")
# 
# 
# Increment_amount_data <- award_data %>%
#   filter(`Modification Type` %in% valid_mod_types) %>%
#   select(`Award #`, `Obligated Amount`, `Modification Type`) %>%
#   filter(!is.na(`Obligated Amount`)) %>%
#   mutate(`Obligated Amount` = gsub("[^0-9.]", "", `Obligated Amount`),
#          `Obligated Amount` = as.numeric(`Obligated Amount`)) %>%
#   mutate(Base_Award = str_remove(`Award #`, "-\\d+$")) %>%
#   arrange(Base_Award, `Award #`) %>%
#   group_by(Base_Award) %>%
#   mutate(Increment_Amount_Obliged = `Obligated Amount` - lag(`Obligated Amount`, default = 0)) %>%
#   ungroup() %>%
#   # Conditional adjustment for negative increments
#   mutate(Increment_Amount_Obliged = case_when(
#     Increment_Amount_Obliged < 0 & !(`Modification Type` %in% c("De-Obligation", "Sponsor Decrease")) ~ `Obligated Amount`,
#     TRUE ~ Increment_Amount_Obliged
#   )) %>%
#   # Round and format
#   mutate(Increment_Amount_Obliged = format(round(Increment_Amount_Obliged, 2), nsmall = 2)) %>%
#   select(`Award #`, Increment_Amount_Obliged)
# 


valid_mod_types <- c("Original Award", "New Funding Increment", "Pre-Award Spending", "Pre-Award Spending Removal", "Key Personnel Change")

Increment_amount_data <- award_data %>%
  filter(`Modification Type` %in% valid_mod_types) %>%
  select(`Award #`, `Obligated Amount`, `Modification Type`) %>%
  filter(!is.na(`Obligated Amount`)) %>%
  mutate(`Obligated Amount` = gsub("[^0-9.]", "", `Obligated Amount`),
         `Obligated Amount` = as.numeric(`Obligated Amount`)) %>%
  mutate(Base_Award = str_remove(`Award #`, "-\\d+$")) %>%
  arrange(Base_Award, `Award #`) %>%
  group_by(Base_Award) %>%
  # Conditional lag calculation: skip lag for Key Personnel Change
  mutate(Increment_Amount_Obliged = case_when(
    `Modification Type` == "Key Personnel Change" ~ `Obligated Amount`,
    TRUE ~ `Obligated Amount` - lag(`Obligated Amount`, default = 0)
  )) %>%
  ungroup() %>%
  # Adjust negative increments unless De-Obligation or Sponsor Decrease
  mutate(Increment_Amount_Obliged = case_when(
    Increment_Amount_Obliged < 0 & !(`Modification Type` %in% c("De-Obligation", "Sponsor Decrease")) ~ `Obligated Amount`,
    TRUE ~ Increment_Amount_Obliged
  )) %>%
  # Round and format
  mutate(Increment_Amount_Obliged = format(round(Increment_Amount_Obliged, 2), nsmall = 2)) %>%
  select(`Award #`, Increment_Amount_Obliged)




# Merge back to award data
award_data <- merge(award_data, Increment_amount_data, by = "Award #")

# Convert History Action Date to datetime first
award_data <- award_data %>%
  mutate(`History Action Date Parsed` = mdy_hms(`History Action Date`, quiet = TRUE))

# # Process the latest comment based on parsed datetime
# new_funding_increment_history_comment <- award_data %>%
#   filter(!is.na(`History Comment`) & !is.na(`History Action Date Parsed`)) %>%
#   group_by(`Award #`, `Modification Type`) %>%
#   slice_max(`History Action Date Parsed`, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   select(`Award #`, `History Comment`)
#   

  




# STEP 2: Select last 16 columns to move them forward (as in your original script)
award_data <- award_data %>%
  select(-(1:16), everything()[1:16])

# STEP 3: Remove rows where both Project Title and PI are missing
award_data <- award_data %>%
  filter(!(is.na(`Project Title`) & is.na(PI)))

# STEP 4: Separate versioned and non-versioned award rows
award_data <- award_data %>%
  mutate(
    is_versioned = grepl("-\\d+$", `Award #`)
  )

# ---- HANDLE VERSIONED AWARDS ---- #
# award_data_versioned <- award_data %>%
#   filter(is_versioned) %>%
#   mutate(
#     Award_Base = sub("-\\d+$", "", `Award #`),
#     Award_Version = as.numeric(sub(".*-(\\d+)$", "\\1", `Award #`))
#   ) %>%
#   group_by(Award_Base) %>%
#   filter(Award_Version == max(Award_Version, na.rm = TRUE)) %>%
#   ungroup() %>%
#   select(-Award_Base, -Award_Version, -is_versioned)


award_data_versioned <- award_data %>%
  filter(is_versioned) %>%
  mutate(
    Award_Base = sub("-\\d+$", "", `Award #`),
    Award_Version = as.numeric(sub(".*-(\\d+)$", "\\1", `Award #`))
  ) %>%
  select(-Award_Base, -Award_Version, -is_versioned)




# ---- KEEP NON-VERSIONED AWARDS AS-IS ---- #
award_data_nonversioned <- award_data %>%
  filter(!is_versioned) %>%
  select(-is_versioned)

# ---- COMBINE CLEANED DATA ---- #
award_data_cleaned <- bind_rows(award_data_versioned, award_data_nonversioned)

# STEP 5: Convert Award Start Date and add Month-Year column
award_data_cleaned <- award_data_cleaned %>%
  mutate(
    `Award Start Date` = mdy(`Award Start Date`),
    Award_Start_Month_Year = paste0("'", format(`Award Start Date`, "%b-%y"))  # To avoid Excel date conversion
  )


############# If College is missing how to fix? ################

# college_admin_unit <- award_data_cleaned %>%
#   select(`Award #`,college,`Admin Unit`) %>%
#   distinct()
# 
# View(college_admin_unit)


# =====================================================
# STEP 5: Load and Merge College Name Data
# =====================================================

# Load College Name Data
# college_name_data <- read_excel(Sys.getenv("COLLEGE_NAME_DATA_PATH"))
# 
# # Clean and Prepare College Data
# college_name_data <- college_name_data %>%
#   select(`College (Subdivision)`, `Department (Organization)`) %>%
#   na.omit() %>%
#   rename(`PI Unit` = `Department (Organization)`)
# 
# # Merge College Data with Award Data
# award_data_cleaned <- left_join(award_data_cleaned, 
#                              college_name_data, 
#                              by = "PI Unit") %>%
#   distinct()




college_name_data_Ashlee <- read_excel(Sys.getenv("COLLEGE_NAME_DATA_PATH_ASHLEE"))



college_name_data_Ashlee <- college_name_data_Ashlee %>%
  na.omit() %>%
  rename(`Admin Unit` = `Department`)



# Merge College Data with Award Data
award_data_cleaned <- left_join(award_data_cleaned, 
                                college_name_data_Ashlee, 
                                by = "Admin Unit") %>%
  distinct()

# Move "Award #" to be the first column
award_data_cleaned <- award_data_cleaned %>%
  select(`Award #`, everything())

# Final filtering


#valid_mod_types <- c("Original Award", "New Funding Increment", "Pre-Award Spending", "Pre-Award Spending Removal", "Key Personnel Change")
valid_mod_types <- c("Original Award", "New Funding Increment")

award_data_cleaned <- award_data_cleaned %>%
  filter(`Modification Type` %in% valid_mod_types)






award_data <- award_data_cleaned

# Extract Base and Version Information
award_data <- award_data %>%
  mutate(
    Award_Base = str_remove(`Award #`, "-\\d+$"),
    Award_Version = as.numeric(str_extract(`Award #`, "\\d+$"))
  )

# Process Based on Rules
award_data <- award_data %>%
  group_by(Award_Base) %>%
  # Keep the latest Key Personnel Change if multiple exist
  filter(!( 
    `Modification Type` == "Key Personnel Change" &
      Award_Version < max(Award_Version[`Modification Type` == "Key Personnel Change"], na.rm = TRUE)
  )) %>%
  # Remove Original Award if any Key Personnel Change exists
  filter(!(
    any(`Modification Type` == "Key Personnel Change") &
      `Modification Type` == "Original Award"
  )) %>%
  ungroup() %>%
  select(-Award_Base, -Award_Version)


output_path_prop <- file.path(output_path, "Processed_Award_Data.csv")

write.csv(award_data,output_path_prop,row.names=F)
