library(readr)
library(dplyr)










library(readr)
All_Award_data <- read_csv("RoamWyo Data/All_Award_data.csv")

All_Award_data_sub <- All_Award_data %>%
  select(`Sponsor Award Number`,`Award Start Date`,`Award End Date`)

colnames(All_Award_data_sub)[1] <- "Grant Number" 



EPSCoR_IDEA_rows <- All_Award_data %>%
  filter(if_any(everything(), ~ grepl("EPSCoR|IDEA", ., ignore.case = TRUE)))

EPSCoR_IDEA_rows_filtered <- EPSCoR_IDEA_rows %>%
  mutate(`Award End Date` = as.Date(`Award End Date`, format = "%m/%d/%Y")) %>%
  filter(`Award End Date` > as.Date("07/01/2023",format = "%m/%d/%Y"))

EPSCoR_IDEA_rows_filtered <- EPSCoR_IDEA_rows_filtered %>%
  mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))


table(EPSCoR_IDEA_rows_filtered$`Prime Sponsor`)
NSF_EPSCoR_IDEA_FY24 <- EPSCoR_IDEA_rows_filtered %>%
  filter(grepl("National Science Foundation", `Prime Sponsor`, ignore.case = TRUE))

View(NSF_EPSCoR_IDEA_FY24)

#==============================================================================#



###### NSF (Did everything in excel)
# library(readxl)
# Awards_NSF <- read_excel("External Data Used in Output/Awards_NSF.xls")
library(readr)
library(dplyr)
NSF_Updated <- read_csv("Final Reference Data/NSF_Updated.csv")



EPSCoR_NSF_Updated <- NSF_Updated %>%
  filter(if_any(everything(), ~ grepl("EPSCoR|IDEeA", ., ignore.case = TRUE)))
#dim(EPSCoR_NSF_Updated)



EPSCoR_NSF_Updated$StartDate <- as.Date(EPSCoR_NSF_Updated$StartDate, format = "%m/%d/%Y")


# Define FY24 range
fy24_start <- as.Date("2023-07-01")
fy24_end <- as.Date("2024-06-30")

EPSCoR_NSF_Updated_FY24 <- EPSCoR_NSF_Updated %>%
  filter(StartDate>=fy24_start & StartDate<=fy24_end)

EPSCoR_NSF_Updated_FY24$AwardedAmountToDate <- as.numeric(gsub("[$,]", "", EPSCoR_NSF_Updated_FY24$AwardedAmountToDate))
sum(EPSCoR_NSF_Updated_FY24$AwardedAmountToDate)




write.csv(EPSCoR_NSF_Updated_FY24,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/EPSCoR_NSF_Updated_FY24.csv")




#################################################################################












# Convert date column and filter
DOE_FY24 <- DOE_CLEAN_DATA %>%
  mutate(`Expenditure Item Date` = as.Date(`Expenditure Item Date`)) %>%
  filter(`Expenditure Item Date` >= fy24_start & `Expenditure Item Date` <= fy24_end)

names(NSF_Updated)

View(NSF_Updated)

##### USDA ####

library(readr)
USDA_FY_24 <- read_csv("External Data Used in Output/USDA_FY_24.csv")


USDA_unique <- USDA_FY_24 %>%
  merge(All_Award_data_sub, by = "Grant Number") %>%
  distinct()

USDA_unique <- USDA_unique %>%
  mutate(
    `Award Start Date` = as.Date(`Award Start Date`, format = "%m/%d/%Y"),
    `Award End Date` = as.Date(`Award End Date`, format = "%m/%d/%Y")
  ) %>%
  mutate(
    Award_Duration_Years = round(as.numeric(difftime(`Award End Date`, `Award Start Date`, units = "days")) / 365.25, 0),
    Award_Dollars_Per_Year = ifelse(Award_Duration_Years > 0, `Award Dollars` / Award_Duration_Years, NA)
  )


#FY 24 Funing from USDA: 
write.csv(USDA_unique,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/USDA_unique.csv")

sum(USDA_unique$Award_Dollars_Per_Year)


#### NIH ####


NIH <- read_csv("External Data Used in Output/NIH.csv")
View(NIH)

colnames(NIH)[1] <- "Grant Number" 


NIH_unique <- NIH %>%
  merge(All_Award_data_sub, by = "Grant Number") %>%
  distinct()

# Keep unique rows only
NIH_unique <- distinct(NIH_unique)

names(NIH_unique)
NIH_unique <- NIH_unique %>%
  mutate(
    `Award Start Date` = as.Date(`Award Start Date`, format = "%m/%d/%Y"),
    `Award End Date` = as.Date(`Award End Date`, format = "%m/%d/%Y")
  ) %>%
  mutate(
    Award_Duration_Years = round(as.numeric(difftime(`Award End Date`, `Award Start Date`, units = "days")) / 365.25, 0)
  )

View(NIH_unique)

write.csv(NIH_unique,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/NIH_unique.csv")





#### DOE

View(All_Award_data)
All_Award_data$`additional comments terms`



EPSCoR_IDEA_rows <- All_Award_data %>%
  filter(if_any(everything(), ~ grepl("EPSCoR|IDEA", ., ignore.case = TRUE)))

EPSCoR_IDEA_rows_filtered <- EPSCoR_IDEA_rows %>%
  mutate(`Award End Date` = as.Date(`Award End Date`, format = "%m/%d/%Y")) %>%
  filter(`Award End Date` > as.Date("07/01/2023",format = "%m/%d/%Y"))

EPSCoR_IDEA_rows_filtered <- EPSCoR_IDEA_rows_filtered %>%
     mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))

DOD_EPSCoR_IDEA_FY24 <- EPSCoR_IDEA_rows_filtered %>%
     filter(grepl("Department of Defense", `Prime Sponsor`, ignore.case = TRUE))


dim(DOD_EPSCoR_IDEA_FY24)

DOE_EPSCoR_IDEA_FY24 <- EPSCoR_IDEA_rows_filtered %>%
     filter(grepl("Department of Energy", `Prime Sponsor`, ignore.case = TRUE))


NASA_EPSCoR_IDEA_FY24 <- EPSCoR_IDEA_rows_filtered %>%
     filter(grepl("National Aeronautics and Space Administration", `Prime Sponsor`, ignore.case = TRUE))


View(NASA_EPSCoR_IDEA_FY24)







### Get the FY24 Expenditure for DOE project

library(readxl)
DOE_CLEAN_DATA <- read_excel("External Data Used in Output/Finally used/DOE_CLEAN_DATA.xlsx")
str(DOE_CLEAN_DATA)

library(dplyr)


# Define FY24 range
fy24_start <- as.Date("2023-07-01")
fy24_end <- as.Date("2024-06-30")

# Convert date column and filter
DOE_FY24 <- DOE_CLEAN_DATA %>%
  mutate(`Expenditure Item Date` = as.Date(`Expenditure Item Date`)) %>%
  filter(`Expenditure Item Date` >= fy24_start & `Expenditure Item Date` <= fy24_end)

sum(DOE_FY24$`Burdened Cost in Provider Ledger Currency`)


write.csv(DOE_FY24,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/DOE_FY24.csv")

# # Proposal Data from RoamWyo
# 
# Proposal_data_FY24_Project_Start_Date <- read_csv("RoamWyo Data/Proposal_data_FY24_Project_Start_Date.csv")
# 
# # 
# # 
# 
# Proposal_data_FY24_Project_Start_Date <- Proposal_data_FY24_Project_Start_Date %>%
#   mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))
# 
# # Department of Defense
# DOD <- Proposal_data_FY24_Project_Start_Date %>%
#   filter(grepl("Department of Defense", `Prime Sponsor`, ignore.case = TRUE))
# 
# write.csv(DOD,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/FY24_DOD_proposals.csv")
# dim(DOD)
# 
# # National Aeronautics and Space Administration
# NASA <- Proposal_data_FY24_Project_Start_Date %>%
#   filter(grepl("National Aeronautics and Space Administration", `Prime Sponsor`, ignore.case = TRUE))
# 
# dim(NASA)
# write.csv(NASA,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/FY24_NASA_proposals.csv")
# 
# 
# 
# # Department of Energy
# DOE <- Proposal_data_FY24_Project_Start_Date %>%
#   filter(grepl("Department of Energy", `Prime Sponsor`, ignore.case = TRUE))
# 
# dim(DOE)
# write.csv(DOE,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/EPSCoR fund tracking Project 041125/Output/FY24_DOE_proposals.csv")
# 
# 
# Proposal_data_subset_start_date <- Proposal_data_FY24_Project_Start_Date %>%
#   select(`Project Title`,`Proposal #`,`Project Start Date`,Status,`epscor-idea`,Sponsor,`Prime Sponsor`,`proposal opportunity name`,`Total Project Cost`)
# 
# EPSCoR_data <- Proposal_data_subset_start_date %>%
#   filter(
#     (!is.na(`epscor-idea`) & (`epscor-idea` == "EPSCoR" | `epscor-idea` == "IDEA")) |
#       grepl("EPSCoR", `proposal opportunity name`, ignore.case = TRUE)
#   )
# 
# 
# View(EPSCoR_data)
# 
# EPSCoR_data <- EPSCoR_data %>%
#   mutate(`Project Number` = gsub("P", "A", gsub("-", "", `Proposal #`)))
# 
# 
# 
# # Award Data from Financial System (WyoCloud)
# 
# library(readxl)
# Project_Information_Results <- read_excel("WyoCloud Data/Project Information_Results 04142025.xlsx")
# Project_Financial_Summary_Results <- read_excel("WyoCloud Data/Project Financial Summary_Results_04142025.xlsx")
# 
# # Ensure the 'Project Start Date' column is in Date format
# Project_Information_Results <- Project_Information_Results %>%
#   mutate(`Project Start Date` = as.Date(`Project Start Date`))
# 
# # # Filter for FY 2024 (July 1, 2023 to June 30, 2024)
# # FY24_projects_info <- Project_Information_Results %>%
# #   filter(`Project Start Date` >= as.Date("2023-07-01") &
# #            `Project Start Date` <= as.Date("2024-06-30"))
# 
# 
# selected_project_info <- Project_Information_Results %>%
#   select(`Project Number`,`Award Sponsor`)
# 
# selected_fin_data <- Project_Financial_Summary_Results %>%
#   select(`Project Number`,`Project Funding Amount`)
# # 
# # 
# 
# Award_data <- merge(selected_project_info,selected_fin_data,by= "Project Number")
# 
# 
# View(EPSCoR_data_awarded)
# 
# 
# 
# # Merge EPSCoR_data with WyoCloud data
# EPSCoR_data_awarded <- merge(EPSCoR_data,Award_data,by = "Project Number")
# 
