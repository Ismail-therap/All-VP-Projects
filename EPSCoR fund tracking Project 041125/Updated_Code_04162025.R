

##################################### NSF Total funding: ##########################################

# https://tableau.external.nsf.gov/views/NSFbyNumbers/NumbersbyState?%3AisGuestRedirectFromVizportal=y&%3Aembed=y&%3Alinktarget=_blank&%3Atoolbar=top

# IN FY24 total was $18.37 million for University of Wyoming.75 Proposal Evaluated and 24 New Award Funded.


## NSF EPSCoR data: 

# I download the data from: https://www.nsf.gov/awardsearch/advancedSearch.jsp
# Filter I used: Organization == "University of Wyoming", State = "Wyoming", 
# Element Code = 916800,915100,915000,721700,321Y00,270Y00,269Y00,196Y00,195Y00,194Y00,193Y00,078P00 [This are the codes for EPSCoR]


library(readr)
Awards_NSF_EPSCoR_1 <- read_csv("Accurate Data 04162025/NSF EPSCoR/Awards_NSF_EPSCoR_1.csv")
Awards_NSF_EPSCoR_2 <- read_csv("Accurate Data 04162025/NSF EPSCoR/Awards_NSF_EPSCoR_2.csv")


Awards_NSF_EPSCoR <- rbind(Awards_NSF_EPSCoR_1,Awards_NSF_EPSCoR_2)
Awards_NSF_EPSCoR$StartDate <- as.Date(Awards_NSF_EPSCoR$StartDate, format = "%m/%d/%Y")


# Define FY24 range
fy24_start <- as.Date( "07/01/2023",format = "%m/%d/%Y")
fy24_end <- as.Date("06/30/2024",format = "%m/%d/%Y")

Awards_NSF_EPSCoR_FY24 <- Awards_NSF_EPSCoR %>%
  filter(StartDate>=fy24_start & StartDate<=fy24_end)

Awards_NSF_EPSCoR_FY24$AwardedAmountToDate <- as.numeric(gsub("[$,]", "", Awards_NSF_EPSCoR_FY24$AwardedAmountToDate))
sum(Awards_NSF_EPSCoR_FY24$AwardedAmountToDate)


####################################################### National Aeronautics and Space Administration (NASA) Funding amount ############################



#### Need to fix this code!!!


All_Award_data <- read_csv("Other/RoamWyo Data/All_Award_data.csv")



convert_fy_date <- function(date_string) {
  # Convert "mm/dd/yyyy" to Date object, then format as "yyyy-mm-dd"
  as.Date(date_string, format = "%m/%d/%Y") %>% 
    format("%Y-%m-%d")
}

fy_end = "06/30/2024"
convert_fy_date(fy_end)

library(dplyr)
library(stringr)

start_fy = "07/01/2023"
end_fy = "06/30/20224"

# Filtering for FY24
All_Award_data_filtered <- All_Award_data %>%
  mutate(`Award End Date` = as.Date(`Award End Date`, format = "%m/%d/%Y")) %>%
  mutate(`Award Start Date` = as.Date(`Award Start Date`, format = "%m/%d/%Y"))%>%
  filter(`Award End Date` >= as.Date(start_fy,format = "%m/%d/%Y") & `Award Start Date` <= as.Date(end_fy,format = "%m/%d/%Y"))


# Creating a general column to select Prime Sponsor:
All_Award_data_filtered <- All_Award_data_filtered %>%
  mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))


All_Award_data_NASA <- All_Award_data_filtered %>%
  filter(grepl("National Science Foundation", `Prime Sponsor`, ignore.case = TRUE))

dim(All_Award_data_NASA)

All_Award_data_NASA <- All_Award_data_NASA %>%
  filter(`Modification Type` %in% c("Original Award","New Funding Increment"))


All_Award_data_NASA$`Modification Date` <-  as.Date(All_Award_data_NASA$`Modification Date`, format = "%m/%d/%Y") 


# Convert the dates to Date type if they are not already
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

dim(All_Award_data_NASA)

# Keep only the most recent row for each Base_Award based on Award Start Date
All_Award_data_NASA_latest <- All_Award_data_NASA %>%
  group_by(Base_Award) %>%
  filter(`Award Start Date` == max(`Award Start Date`, na.rm = TRUE)) %>%
  ungroup()

print(dim(All_Award_data_NASA_latest))
All_Award_data_NASA_latest_FY24 <- All_Award_data_NASA_latest %>%
  filter(`Award Start Date` <= as.Date("2024-06-30",format = "%Y-%m-%d"))



All_Award_data_NASA_latest_FY24 <- All_Award_data_NASA_latest_FY24 %>%
  mutate(
    Duration_Years =  round(as.numeric(difftime(`Award End Date`, `Award Start Date`, units = "days")) / 365.25,0)
  )


All_Award_data_NASA_latest_FY24 <- All_Award_data_NASA_latest_FY24 %>%
  mutate(Adjusted_Incremental_Amount = ifelse(
    `Modification Type` == "Original Award" & !is.na(Duration_Years) & Duration_Years != 0,
    Incremental_Amount / Duration_Years,
    Incremental_Amount
  ))

sum(All_Award_data_NASA_latest_FY24$Adjusted_Incremental_Amount)



