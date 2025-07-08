


################################################################################################
# This function have bug: In date columns and applying logic with dates need fix and then use###
################################################################################################

All_Award_data <- read_csv("Other/RoamWyo Data/All_Award_data.csv")


convert_fy_date <- function(date_string) {
  # Convert "mm/dd/yyyy" to Date object, then format as "yyyy-mm-dd"
  as.Date(date_string, format = "%m/%d/%Y") %>% 
    format("%Y-%m-%d")
}



process_award_data <- function(data, sponsor_keyword, fy_start, fy_end) {
  library(dplyr)
  library(readr)
  library(stringr)
  
  # Filtering for FY
  All_Award_data_filtered <- data %>%
    mutate(`Award End Date` = as.Date(`Award End Date`, format = "%m/%d/%Y")) %>%
    mutate(`Award Start Date` = as.Date(`Award Start Date`, format = "%m/%d/%Y")) %>%
    filter(`Award End Date` >= as.Date(fy_start, format = "%m/%d/%Y") & `Award Start Date` <= as.Date(fy_end, format = "%m/%d/%Y"))
  
  # Creating a general column to select Prime Sponsor:
  All_Award_data_filtered <- All_Award_data_filtered %>%
    mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))
  
  All_Award_data_NASA <- All_Award_data_filtered %>%
    filter(grepl(sponsor_keyword, `Prime Sponsor`, ignore.case = TRUE))
  
  All_Award_data_NASA <- All_Award_data_NASA %>%
    filter(`Modification Type` %in% c("Original Award", "New Funding Increment"))
  
  All_Award_data_NASA$`Modification Date` <- as.Date(All_Award_data_NASA$`Modification Date`, format = "%m/%d/%Y")
  
  # Convert numeric Excel-style dates to Date format
  All_Award_data_NASA$`Award Start Date` <- as.Date(All_Award_data_NASA$`Award Start Date`, origin = "1899-12-30")
  All_Award_data_NASA$`Modification Date` <- as.Date(All_Award_data_NASA$`Modification Date`, origin = "1899-12-30")
  
  # Replace Award Start Date with Modification Date where available
  All_Award_data_NASA$`Award Start Date`[!is.na(All_Award_data_NASA$`Modification Date`)] <- 
    All_Award_data_NASA$`Modification Date`[!is.na(All_Award_data_NASA$`Modification Date`)]
  
  # Create base award ID (remove extension like -0, -1, -2)
  All_Award_data_NASA <- All_Award_data_NASA %>%
    mutate(
      Base_Award = str_replace(`Award #`, "-\\d+$", ""),
      Extension = as.integer(str_extract(`Award #`, "\\d+$"))
    )
  
  # Arrange and compute incremental amounts
  All_Award_data_NASA <- All_Award_data_NASA %>%
    arrange(Base_Award, Extension) %>%
    group_by(Base_Award) %>%
    mutate(
      Incremental_Amount = `Obligated Amount` - lag(`Obligated Amount`, default = 0)
    ) %>%
    ungroup()
  
  # Keep only the most recent row for each Base_Award based on Award Start Date
  All_Award_data_NASA_latest <- All_Award_data_NASA %>%
    group_by(Base_Award) %>%
    filter(`Award Start Date` == max(`Award Start Date`, na.rm = TRUE)) %>%
    ungroup()
  
  All_Award_data_NASA_latest_FY24 <- All_Award_data_NASA_latest %>%
    filter(`Award Start Date` <= as.Date(convert_fy_date(fy_end), format = "%Y-%m-%d"))

  
  All_Award_data_NASA_latest_FY24 <- All_Award_data_NASA_latest_FY24 %>%
    mutate(
      Duration_Years = round(as.numeric(difftime(`Award End Date`, `Award Start Date`, units = "days")) / 365.25, 0)
    )
  
  All_Award_data_NASA_latest_FY24 <- All_Award_data_NASA_latest_FY24 %>%
    mutate(Adjusted_Incremental_Amount = ifelse(
      `Modification Type` == "Original Award" & !is.na(Duration_Years) & Duration_Years != 0,
      Incremental_Amount / Duration_Years,
      Incremental_Amount
    ))
  
  
  # Return both the data and the summary
  list(
    data = All_Award_data_NASA_latest_FY24,
    total_funding = sum(All_Award_data_NASA_latest_FY24$Adjusted_Incremental_Amount, na.rm = TRUE),
    n_awards = n_distinct(All_Award_data_NASA_latest_FY24$Base_Award)
  )
 
}




# Using your exact parameters
results <- process_award_data(
  data = All_Award_data,
  fy_start = "07/01/2023",
  fy_end = "06/30/2024",  
  sponsor_keyword = "National Science Foundation"
)
results$total_funding



# Access results
final_data <- results$data
total_funding <- results$total_funding 
award_count <- results$n_awards



