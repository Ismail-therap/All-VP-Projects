
# 📌 Load Required Libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(lubridate)  # For handling date-time conversions
library(stringr)    # For string operations
library(tidyr)      # For reshaping data

options(scipen = 999)
options(digits = 22)

# STEP 1: Load the data
award_data <- read_csv("Share NIFA data 06112025/All_selected_columns.csv")


### Fix admin Unit and PI unit changes:

# STEP 1: Extract base award ID and version number
award_data <- award_data %>%
  mutate(
    Award_Base = str_remove(`Award #`, "-\\d+$"),
    Award_Version = as.numeric(str_extract(`Award #`, "\\d+$"))
  )

# STEP 2: Fill latest PI and Admin Unit down to all related records per project
award_data <- award_data %>%
  group_by(Award_Base) %>%
  arrange(Award_Version, .by_group = TRUE) %>%
  mutate(
    Final_PI = coalesce(last(na.omit(PI)), first(PI)),
    Final_Admin_Unit = coalesce(last(na.omit(`Admin Unit`)), first(`Admin Unit`))
  ) %>%
  ungroup()

# STEP 3: If PI/Admin Unit differ from final value, update
award_data <- award_data %>%
  mutate(
    PI = ifelse(is.na(PI) | PI != Final_PI, Final_PI, PI),
    `Admin Unit` = ifelse(is.na(`Admin Unit`) | `Admin Unit` != Final_Admin_Unit, Final_Admin_Unit, `Admin Unit`)
  ) %>%
  select(-Award_Base, -Award_Version, -Final_PI, -Final_Admin_Unit)



All_NIFA <- award_data %>%
  filter(
    `Sponsor Primary Code` == "01-USDANIFA" | `Prime Sponsor Primary Code` == "01-USDANIFA",
    !is.na(PI),
    `Modification Type` %in% c("Original Award","New Funding Increment")
  )


All_NIFA_cleaned <- All_NIFA %>%
  select(where(~ !all(is.na(.))))

All_NIFA_cleaned <- All_NIFA_cleaned %>%
  select(
    `Award #`,
    PI,
    `PI Unit`,
    `Admin Unit`,
    everything()
  ) %>%
  select(all_of(unique(names(.))))  # Removes duplicated columns safely


write.csv(All_NIFA_cleaned,"Share NIFA data 06112025/All_NIFA_awards.csv",row.names = FALSE, na = "")

